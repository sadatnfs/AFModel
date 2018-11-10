


################################################
#### Author: Nafis Sadat
#### Purpose: Create draws
#### Created: 2018/05/22
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
load(paste0("/share/resource_tracking/forecasting/", variable, "/", date, "_", comment, "/summary_files/draws_array.Rdata"))

### Getting model info
model_info <- get_model_specs(data = rmse_distro, model = id, draws = T)
print(paste0("Running model number ", model_info$model, " with the following specifications and ", model_info$draws, " draws"))
print(model_info)



################################################
## (2) Prep all data with covariates
################################################

## Bring in full data
input_data <- data.table(read.csv(paste0("/share/resource_tracking/forecasting/", variable, "/input_data/RT_2018_GDP_use.csv")))[year <= metadata_list$end_FC]
input_data <- copy(input_data)
input_data <- input_data[year >= eval(metadata_list$start_year - 1)]
input_data[, year_id := year]


## Create scaled maxxer for convergence
input_data[, conv_scaler := max(get(gsub("ln\\_|logit\\_", "", model_info$yvar)), na.rm = T)]
input_data[, Y_scaled_conv := shift(get(gsub("ln\\_|logit\\_", "", model_info$yvar)) / conv_scaler), by = "iso3"]



################################################
## (4.1) Load previously run TMB model and data
################################################

load(paste0("/share/resource_tracking/forecasting/", variable, "/", date, "_", comment, "/means/", model_info$model, ".Rdata"))



################################################
## (4.2) Prep draws of covariates and/or Y
################################################

if (model_info$draws > 0) {

  ### Covariates ###

  N_draws <- model_info$draws

  # if('edu' %in% model_info$xvar) {
  # ##### Education draws (melt and array it) [NOT USE BECAUSE BAD]
  #   edu_draws <- data.table(read_feather('/home/j/Project/IRH/Forecasting/FBD_edu_20171128_ref_draws.feather',
  #                                        columns = c('location_id','year_id','scenario', paste0('draw_', c(0:eval(model_info$draws - 1))) )))
  #   edu_draws <- edu_draws[year_id %in% c(eval(metadata_list$start_year + 1):metadata_list$end_FC) & scenario == 0][, year:= year_id]
  #   edu_draws <- edu_draws[, .SD, .SDcols = c('location_id', 'year', paste0('draw_', c(0:eval(model_info$draws - 1))) )]
  #   edu_draws <- melt(edu_draws, c('location_id', 'year'), variable.name = 'draw', value.name = 'edu')
  #   edu_draws[, draw:= as.numeric(gsub('draw_', '', draw))]
  #
  #   ## Merge on iso3
  #   edu_level <- merge(edu_draws, unique(input_data[,.(iso3, location_id)]), 'location_id')
  #
  #   ## Make diff
  #   edu_diff <- copy(edu_level)
  #   setkeyv(edu_diff, c('iso3', 'year'))
  #
  #   ## Update draw index
  #   edu_diff[, draw:= draw+1]
  #
  #   edu_diff[, edu:= edu - shift(edu), by = c('iso3')]
  #   edu_diff <- edu_diff[year>= metadata_list$start_year]
  #
  #   edu_level <- edu_level[year>= metadata_list$start_year]
  #
  # }

  if (!is.null(data_params$specifications$xvar)) {

    #### Get the x_full (levels)
    X_level <- data.table(dcast(melt(data_params$data$x_full), Var2 + Var3 ~ Var1, value.var = "value"))
    colnames(X_level)[1:2] <- c("iso3", "year")

    #### Get the x_diff (diffs)
    X_diff <- data.table(dcast(melt(data_params$data$x_diff), Var2 + Var3 ~ Var1, value.var = "value"))
    colnames(X_diff)[1:2] <- c("iso3", "year")


    # if('edu' %in% model_info$xvar) {
    #
    #   print("Prepping level data with education draws")
    #   ## Outer merge on edu draws
    #   ## First, drop edu if it exists
    #   X_level[, edu:= NULL]
    #   X_level <- merge(X_level, edu_level, c('iso3', 'year'), all.y=T)
    #   X_level[, location_id:= NULL]
    #
    #
    #   print("Prepping diff data with education draws")
    #
    #   ## Outer merge on edu draws
    #   ## First, drop edu if it exists
    #   X_diff[, edu:= NULL]
    #   X_diff <- merge(X_diff, edu_diff, c('iso3', 'year'), all.y=T)
    #   X_diff[, location_id:= NULL]
    # }
  } else {
    X_diff <- NULL
    X_level <- NULL
  }

  ### Dependent Variable Draws ###

  ## Sample N_draws
  sample_draws <- sample(c(1:1000), size = model_info$draws)

  # ## Get ln(GDPpc) draws
  # if(model_info$yvar == 'ln_gdppc' | model_info$yvar == 'ln_gdp' ) {
  #   gdp_draws <- data.table(read_feather('/home/j/Project/IRH/LDI_PPP/LDI/output_data/part6_incl_LDI_20180911_new6_log_USD_draws.feather',
  #                           columns = c('iso3', 'year', paste0('draw_', sample_draws))))
  # } else if (model_info$yvar == 'ln_gdplab') {
  #   gdp_draws <- data.table(read_feather('/home/j/Project/IRH/LDI_PPP/LDI/output_data/GDPLab_part6_incl_LDI_20180911_new6_log_2017USD_draws.feather',
  #                                        columns = c('iso3', 'year', paste0('draw_', sample_draws))))
  # }
  #
  # ## Clean up draw names
  # colnames(gdp_draws) <- c('iso3', 'year', paste0('draw_', c(1:model_info$draws)))
  #
  # ## Keep specific locs
  # gdp_draws <- gdp_draws[iso3 %in% unique(input_data$iso3) ]
  #
  # ## Melt long on draws
  # gdp_draws <- melt(gdp_draws, c('iso3', 'year'), variable.name = 'draw', value.name = model_info$yvar)
  # gdp_draws[, yvar:= get(model_info$yvar)]
  #
  # ## Convert to GDP if our depvar is GDP
  # if(model_info$yvar == 'ln_gdp') {
  #
  #   gdp_draws <- merge(gdp_draws, input_data[,.(iso3, year, total_pop)], c('iso3', 'year'))
  #   gdp_draws[, yvar:= yvar * total_pop]
  #   gdp_draws[, c('total_pop'):= NULL]
  # }
  #
  # ## Create first differences (and log yvar of course)
  # gdp_draws[, (model_info$yvar):=NULL]
  # gdp_draws[, yvar:= log(yvar)]
  # gdp_draws[, ydiff:= yvar - shift(yvar), by = c('iso3', 'draw')]
  #
  # ## Fix draws and make pretty
  # gdp_draws[, draw:= as.numeric(gsub('draw_', '', draw))]
  #
  # ## Prep final datasets
  # Y_diff_lev <- gdp_draws[year >= metadata_list$start_year]
}





################################################
## (4.3) Create forecasts draws!
################################################

### Simulate parameter draws
param_draws <- rmvnorm_prec(
  mu = output_TMB$mean_params,
  prec = output_TMB$FC_model$jointPrecision,
  n.sims = N_draws
)

## If we constrained the rhos, then invlogit them here:
if (data_params$data$ar_constrain & data_params$data$ar > 0) {
  print("Invlogit rhos")

  ## Find which columns have rhos and invlogit them
  param_draws[, grep("rho", colnames(param_draws))] <- invlogit(param_draws[, grep("rho", colnames(param_draws))])
}


### Create predictions

registerDoParallel(cores = 3)
system.time(draws <- rbindlist(foreach(d = 1:N_draws) %do% {
  if (d %% 10 == 0) print(paste0("Draw = ", d))

  ### Make a dataset for estimation data with the draw
  est_data <- prep_data_for_forecast(
    tmb_obj = output_TMB$obj,
    tmb_data_param = data_params,
    tmb_params = param_draws[d, ],
    draw_Xdata = prepping_X_for_draw(X_diff, X_level,
      tmb_data_param = data_params, d = d
    ),
    # draw_Ydata = Y_diff_lev[draw == d]
    draw_Ydata = NULL
  )

  ### Forecast with RW errors
  draw_forecast <- make_forecast(
    mean_est_data_obj = est_data,
    tmb_output_object = output_TMB,
    tmb_data_param = data_params,
    add_residual = T,
    # add_residual = F,
    var_metric = "mad",
    cap_variance = T,
    transform = "exp",
    var_threshold = if (data_params$specifications$fd == 1) {
      rmse_distro$diff_mad_cap
    } else {
      rmse_distro$level_mad_cap
    }
  )[, draw := d]

  return(draw_forecast)
}, use.names = T))



# ggplot(draws[iso3=="MKD"]) +
#   # geom_line(aes(x = year, y = ydiff, group = draw, color = draw)) +
#   geom_line(aes(x = year, y = yvar, group = draw, color = draw)) +
#   theme(legend.position = 'none')

# stopImplicitCluster()




################################################
## (5) Output draws
################################################

## If we have GDP as the yvar, convert to GDPpc by removing ln_pop (divvying in real space) or labour frac
if (model_info$yvar == "ln_gdp") {
  out_draws <- change_units_of_draws(draws = draws, idvar = c("iso3", "year"), input_data = input_data, column = "ln_pop", op = "subtract")
} else if (model_info$yvar == "ln_gdplab") {

  ## Convert GDP per labour to GDP per capita
  input_data[, ln_labor_frax := log(labour_frac)]
  out_draws <- change_units_of_draws(draws = draws, idvar = c("iso3", "year"), input_data = input_data, column = "ln_labor_frax", op = "add")
} else {
  out_draws <- draws
}

out_draws <- out_draws[order(iso3, draw, year)]


ggplot(out_draws[iso3 == "USA"]) +
  # geom_line(aes(x = year, y = ydiff, group = draw, color = draw)) +
  geom_line(aes(x = year, y = yvar, group = draw, color = draw)) +
  theme(legend.position = "none")


## Cast wide on draw, tag model number and save out Rdata
out_draws <- prep_draw_output(out_draws, model = model_info$model)


## Save the draws
# save(list = c("out_draws"),
#      file = paste0('/share/resource_tracking/forecasting/', variable, '/', date, '_', comment, '/draws/', model_info$model, '.Rdata'))
write_feather(
  out_draws,
  paste0("/share/resource_tracking/forecasting/", variable, "/", date, "_", comment, "/draws/", model_info$model, ".feather")
)


q("no")
