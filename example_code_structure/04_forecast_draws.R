


################################################
#### Author: Nafis Sadat
#### Purpose: Create simulations (draws)
################################################


rm(list = ls())
library(AFModel)

################################################
## (1) Get model information
################################################

## Get the Task ID
id <- Sys.getenv("SGE_TASK_ID")
if (id == "" | is.na(id)) {
  id <- 60
}

## Parse the args and get model info

## Open the parser
parser <- ArgumentParser()

## Specify version and description
parser$add_argument("--variable", required = T, type = "character", help = "Variable")
parser$add_argument("--date", required = T, type = "integer", help = "Date")
parser$add_argument("--comment", required = T, type = "character", help = "Comment")


argss <- parser$parse_args()
variable <- argss$variable
date <- argss$date
comment <- argss$comment

## Get mean grid
load(paste0("/share/resource_tracking/forecasting/", 
  variable, "/", date, "_", comment, 
"/summary_files/draws_array.Rdata"))

### Getting model info
model_info <- get_model_specs(data = rmse_distro, model = id, draws = T)
print(paste0("Running model number ", model_info$model, 
  " with the following specifications and ", model_info$draws, 
" draws"))
print(model_info)



################################################
## (2) Prep all data with covariates
################################################



## Bring in full data
input_data <- data.table(read.csv(paste0("/share/resource_tracking/forecasting/", 
  variable, "/input_data/RT_2018_use.csv")))[year <= metadata_list$end_FC]
input_data <- copy(input_data)
input_data <- input_data[year >= eval(metadata_list$start_year - 1)]
input_data[, year_id := as.numeric(year) - min(year)  ]


## Create scaled maxxer for convergence
input_data[, conv_scaler := max(get(gsub("ln\\_|logit\\_", "", model_info$yvar)), na.rm = T)]
input_data[, Y_scaled_conv := shift(get(gsub("ln\\_|logit\\_", "", model_info$yvar)) / conv_scaler), by = "iso3"]


################################################
## (4.1) Load previously run TMB model and data
################################################

load(paste0("/share/resource_tracking/forecasting/", variable, "/", 
  date, "_", comment, "/means/", model_info$model, ".Rdata"))



################################################
## (4.2) Prep draws of covariates and/or Y
################################################


## This block is sort of useless, just needed for bookkeeping
if (model_info$draws > 0) {

  ### Covariates ###

  N_draws <- model_info$draws

  if (!is.null(data_params$specifications$xvar)) {

    #### Get the x_full (levels)
    X_level <- data.table(dcast(melt(data_params$data$x_full), Var2 + Var3 ~ Var1, value.var = "value"))
    colnames(X_level)[1:2] <- c("iso3", "year")

    #### Get the x_diff (diffs)
    X_diff <- data.table(dcast(melt(data_params$data$x_diff), Var2 + Var3 ~ Var1, value.var = "value"))
    colnames(X_diff)[1:2] <- c("iso3", "year")

  } else {
    X_diff <- NULL
    X_level <- NULL
  }
}





################################################
## (4.3) Create forecasts draws!
################################################

### Get the joint precision matrix from the model object
use_prec_mat <- (output_TMB$FC_model$jointPrecision)


### Simulate parameter draws
param_draws <- rmvnorm_prec(
  mu = output_TMB$mean_params,
  prec = use_prec_mat,
  n.sims = N_draws
)

## If we constrained the rhos, then invlogit them here:
if (data_params$data$ar_constrain & data_params$data$ar > 0) {
  print("Invlogit rhos")

  ## Find which columns have rhos and invlogit them
  param_draws[, grep("rho", colnames(param_draws))] <- invlogit(param_draws[, grep("rho", colnames(param_draws))])
}


### Create predictions parallely

registerDoParallel(cores = 10)
system.time(draws <- rbindlist(foreach(d = 1:N_draws) %dopar% {
  if (d %% 10 == 0) print(paste0("Draw = ", d))

  ### Make a dataset for estimation data with the draw
  est_data <- prep_data_for_forecast(
    tmb_obj = output_TMB$obj,
    tmb_data_param = data_params,
    tmb_params = param_draws[d, ],
    draw_Xdata = prepping_X_for_draw(X_diff, X_level,
      tmb_data_param = data_params, d = d
    ),
    draw_Ydata = NULL
  )


  if (model_info$int_decay == 1) {

    ###### RE Decay :
    ## Decay out random effects
    ## First year of decay: end_fit + number of time periods - decay factor year
    ## Last year of decay: end_fit + number of time periods + decay factor year
    ## Decay effective number of years = decay factor year*2

    decay_factor <- 10

    est_data[year >= (metadata_list$end_fit + (metadata_list$end_fit - metadata_list$start_year + 1) - decay_factor), decay_fac := 1:.N, by = "iso3"]
    est_data[!is.na(decay_fac), z_int := z_int * (((decay_factor * 2) - decay_fac) / (decay_factor * 2))]
    est_data[year >= (metadata_list$end_fit + (metadata_list$end_fit - metadata_list$start_year + 1) + decay_factor), z_int := 0]
  }

  ### Forecast with RW errors added on
  draw_forecast <- make_forecast(
    mean_est_data_obj = est_data,
    tmb_output_object = output_TMB,
    tmb_data_param = data_params,
    add_residual = T,
    var_metric = "mad",
    cap_variance = T,
    transform = metadata_list$inv_transform,
    var_threshold = if (data_params$specifications$fd == 1) {
      rmse_distro$diff_mad_cap
    } else {
      rmse_distro$level_mad_cap
    }
  )[, draw := d]

  return(draw_forecast)
}, use.names = T))



stopImplicitCluster()




################################################
## (5) Output draws
################################################


#### This block can change unit of the draws by doing a unary operation (used in GDPpc forecasts)
# ## If we have GDP as the yvar, convert to GDPpc by removing ln_pop (divvying in real space) or labour frac
# if (model_info$yvar == "ln_gdp") {
#   out_draws <- change_units_of_draws(draws = draws, idvar = c("iso3", "year"), input_data = input_data, column = "ln_pop", op = "subtract")
# } else if (model_info$yvar == "ln_gdplab") {

#   ## Convert GDP per labour to GDP per capita
#   input_data[, ln_labor_frax := log(labour_frac)]
#   out_draws <- change_units_of_draws(draws = draws, idvar = c("iso3", "year"), input_data = input_data, column = "ln_labor_frax", op = "add")
# } else {
#   out_draws <- draws
# }

# out_draws <- out_draws[order(iso3, draw, year)]


## Cast wide on draw, tag model number and save out Rdata
out_draws <- prep_draw_output(draws, model = model_info$model)

## Save the draws
write_feather(
  out_draws,
  paste0("/share/resource_tracking/forecasting/", variable, "/", date, "_", comment, "/draws/", model_info$model, ".feather")
)


q("no")

