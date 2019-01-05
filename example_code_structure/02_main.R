
###################################################
### Author:  Nafis Sadat
### Purpose: Central script to hold and submit forecasting jobs
## qsub -N master_script -pe multi_slot 4 -P proj_fin_forecast -j y -o  /share/temp/sgeoutput/$USER/output /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh 1 /path/to/main/script.R
###################################################


rm(list = ls())
library(AFModel)

########################################
#### (1) Run and variable information
########################################

## Set up run info
variable <- "var_name"
comment <- "comment_code"
date <- "put_some_date"
yvv <- "name_of_yvar_in_dataset"
comm <- "your_long_comment"

chaos <- T

if (chaos) {
  cha <- "Add Chaos"
} else {
  cha <- "No Chaos"
}
comment_long <- paste0(cha, " ", date, " ", yvv, " ", comm)


## Prep empty folders and return root path
root_fold <- prep_model(variable, comment, date, erase = F)


#### Set forecasting variable info ####

## Add in the random effect decay over time?
int_decay <- 1

## First year of observed data
start_year <- 1995

## Last year of observed data
end_fit <- 2016

## Forecast to this year
end_FC <- 2050

## Number of years of out-of-sample
oos_years <- 10

## Number of draws to simulate
N_draws <- 1000

## Keep this percent of models
chaos_percent <- 15

## Pack the metadata in a list
metadata_list <- list(
  variable = variable,
  date = date,
  comment = comment,
  comment_long = comment_long,
  start_year = start_year,
  end_fit = end_fit,
  end_FC = end_FC,
  oos_years = oos_years,
  chaos = chaos,
  chaos_percent = chaos_percent,
  transform = 'logit',
  inv_transform = 'invlogit',
  N_draws = 1000
)

## Script to data and paths
mean_script <- "/path/to/mean_forecast_model.R"
draw_script <- "/path/to/forecast_draws.R"
chaos_script <- "/path/to/chaos_compile.R"

## Path to past draws
draw_data <- "/path/to/draws.feather"

shell_file <- paste0(
  "/share/singularity-images/health_fin/forecasting/shells",
  "/health_fin_forecasting_shell_mkl_singularity.sh"
)

########################################
#### (2) Ensemble information
########################################


## List of fixed effects
fixed_effects <- paste0("FE_", c(
  "ln_TFR",
  "edu"
))

## Recency weight schemes
weight_schemes <- seq(0, 0.7, length.out = 11)



## Create a metadata for the ensemble grid
ensemble_metadata <- list(

  ## Dependent variable
  yvar = c(yvv),

  ## Fixed effects (covariates)
  xvar = fixed_effects,

  ## Add random coefficients?
  re_coef = NULL,

  ## Autoregressive and moving average terms
  ar = c(0, 1, 2, 3),
  ma = c(0),

  ## How does the AR and MA models enter
  ### 1 : a global fixed effect for each AR/MA (all countries have same coefficient)
  ### 2 : country specific AR/MA coefficient
  ### 3: a global fixed effect plus a random effect on the AR/MA terms by coutnry
  ar_mod = c(1, 3),
  ma_mod = 0,

  ## Recency weights (0 : equal weights, and higher values use more weights in recent past)
  weight_decays = weight_schemes,

  ## Add global intercept (fixed effect)
  global_int = 1,

  ## Add country specific interecept?
  country_int = 1,

  ## Country intercepts will be random effect?
  country_int_dist = 1,

  ## First difference or levels? (Use c(0,1) if you want both)
  fdiff = 1,

  ## Include the classical convergence term
  conv = c(0),

  ## Include the convergence term in level space, and scaled to (0,1)
  scaled_lev_conv = c(0, 1),

  ## Constrain the AR terms to be between (0,1)?
  ar_constrain = 0
)


## Create ensemble model grid for the y-variable
full_grid <- do.call(create_ensemble_grid, ensemble_metadata)


## List our coefficient priors (convergence terms will always have a negative prior)
## 1 is for positive coefficient, and -1 for negative
coef_priors <- list(
  "conv" = -1,
  "scaled_c" = -1,
  "FE_ln_TFR" = -1,
  "FE_edu" = 1
)


## Save these out
save(
  list = c(
    "metadata_list", "full_grid",
    "ensemble_metadata", "coef_priors"
  ),
  file = paste0(root_fold, "/summary_files/mean_array.Rdata")
)



########################################
#### (3) Submit mean estimatino jobs
########################################


## Create qsub command
proj <- " -P proj_fin_forecast "
slots <- " -pe multi_slot 1"
arrays <- paste0("1:", nrow(full_grid))

qsub_mean <- paste0(
  "qsub -N ", yvv, "_mean_", comment, " ",
  proj,
  slots,
  " -t ", arrays,
  " -j y -o ",
  "/share/temp/sgeoutput/$USER/output ",
  shell_file,
  " 1 ", mean_script,
  " --variable ", variable,
  " --date ", date,
  " --comment ", comment,
  " --oos ", oos_years
)


## Submit the jobs
system(qsub_mean)


## Keep sleeping until they're all done
job_hold(paste0(yvv, "_mean_", comment))




########################################
#### (4) Check and collect ensemble
########################################

# #### Run the following chunk if any of the jobs failed spuriously:
# # Did all the models finish running? (and return models)
# failed_models <- mean_run_check(root_fold = root_fold, mean_array_grid = full_grid, return_fail = T)

# ## Qsubbing for failed jobs
# arrays_failed <- failed_models
# proj <- " -P proj_fin_forecast "
# slots <- " -pe multi_slot 1"

# lapply(failed_models, function(i) {
#   system(paste0(
#     "qsub -N ", yvv, "_mean_", comment, " ",
#     proj,
#     slots,
#     " -t ", i,
#     " -j y -o ",
#     "/share/temp/sgeoutput/$USER/output ",
#   shell_file,
#   " 1 ", mean_script,
#     " --variable ", variable,
#     " --date ", date,
#     " --comment ", comment,
#     " --oos ", oos_years
#   ))
#   return(NULL)
# })

## Quick load previously run data if needed:
# load(paste0(root_fold, '/summary_files/mean_array.Rdata'))


## Get list of models passed
passed_models <- mean_run_check(
  root_fold = root_fold,
  mean_array_grid = full_grid,
  return_fail = F
)


## Bring in all the mean run model info
## for the models that passed the exclusion criteria
data_models_passed <- mean_postfiles_compile(root_fold,
  passed_models,
  mc.cores = 4
)
save(
  list = "data_models_passed",
  file = paste0(
    root_fold,
    "/summary_files/data_models_passed.Rdata"
  )
)


## Apply coefficient direction priors
data_models_passed <- prior_coefs_mean(
  data = data_models_passed,
  coef_prior_list = coef_priors
)



## Distribute draws and chaos RMSE and SRREG and all that. This works regardless of Chaos flag
rmse_distro <- chaos_draw_distro_DMC(
  data = data_models_passed,
  chaos_pc = metadata_list$chaos_percent,
  metadata = ensemble_metadata,
  iso_portion = 1,
  region_portion = 0,
  super_region_portion = 0,
  N_draws = N_draws
)



## Save out metadata and distribution
save(
  list = c("rmse_distro", "metadata_list"),
  file = paste0(
    root_fold,
    "/summary_files/draws_array.Rdata"
  )
)



## Quick load:
# load(paste0(root_fold, '/summary_files/draws_array.Rdata'))

########################################
#### (5) Submit draws
########################################


## Job environment
slotz <- 10
proj <- " -P proj_fin_forecast "
slots <- paste0(" -pe multi_slot ", slotz)
arrays <- paste0("1:", nrow(rmse_distro$array_grid))

## Empty out draws: # system(paste0('rm ', root_fold, '/draws/*'))
## Make qsub line and submit
qsub_draws <- paste0(
  "qsub -N ", yvv, "_draws_", comment, " ",
  proj,
  slots,
  " -t ", arrays,
  " -j y -o /share/temp/sgeoutput/$USER/output ",
  shell_file,
  draw_script,
  " --variable ", variable,
  " --date ", date,
  " --comment ", comment
)

## Submit!
system(paste0(qsub_draws))

## Keep sleeping until donesies
job_hold(paste0(yvv, "_draws_", comment))




########################################
#### (6) Check and collect draws (Chaos or not)
########################################

## Quick load:
# load(paste0(root_fold, '/summary_files/draws_array.Rdata'))


## Check if draws have finished running successfully for all the models
draw_run_check(root_fold = root_fold, array_grid = rmse_distro$array_grid)

## Submit Chaos code for each OOS year
slotz <- 10
proj <- " -P proj_fin_forecast "
slots <- paste0(" -pe multi_slot ", slotz)


chaos_qsubber <- function(oos, chaos) {
  if (chaos) {
    chaos_car <- "T"
  } else {
    chaos_car <- "F"
  }
  paste0(
    "qsub -N ", paste0("chaos_job_", comment, "_", oos),
    proj,
    slots,
    " -j y -o /share/temp/sgeoutput/$USER/output ",
    shell_file,
    chaos_script,
    " --variable ", variable,
    " --date ", date,
    " --comment ", comment,
    " --oos ", oos,
    " --chaos ", chaos_car
  )
}



## Submit for each OOS year (if Chaos)
if (chaos) {
  for (oos in c(1:metadata_list$oos_years)) system(chaos_qsubber(oos, chaos))
} else {
  system(chaos_qsubber(1, chaos))
}


## Keep sleeping until donesies
job_hold(paste0("chaos_job_", comment))

## Check if finished
chaos_collector_check(
  root_fold = root_fold,
  oos_years = metadata_list$oos_years,
  chaos = metadata_list$chaos
)



########################################
#### (7) Fix past draw data because missingness and alignment with retro draws ######
########################################

## Get location metadata
lox <- get_lox()[level == 3, iso3]

## Get past draws data
yvar_draws <- data.table(read_feather(draw_data,
  columns = c("iso3", "year", paste0("draw_", c(1:1000)))
))


yvar_draws <- yvar_draws[(year %in% c(metadata_list$start_year:eval(metadata_list$end_fit))) & (iso3 %in% lox)]
yvar_draws <- yvar_draws[order(iso3, year)]

colnames(yvar_draws) <- c(
  "iso3", "year",
  paste0("draw_", c(1:metadata_list$N_draws))
)

## Set zreoes to small lower bound
yvar_draws[yvar_draws == 0] <- 0.0000001


## Diff the draws BEYOND the first year only!

##### NOTE: Make sure that the correct transformation is used here
##### that is consistent with what we're modeling

yvar_draws[, paste0("draw_", c(1:metadata_list$N_draws)) := lapply(
  paste0("draw_", c(1:metadata_list$N_draws)),
  function(v) logit(get(v))
)]

yvar_draws[, paste0("diff_draw_", c(1:metadata_list$N_draws)) := lapply(
  paste0("draw_", c(1:metadata_list$N_draws)),
  function(v) get(v) - data.table::shift(get(v))
), by = "iso3"]

yvar_draws[year > metadata_list$start_year, paste0(
  "draw_",
  c(1:metadata_list$N_draws)
) := lapply(
  paste0("diff_draw_", c(1:metadata_list$N_draws)),
  function(v) (get(v))
)]
yvar_draws[, paste0("diff_draw_", c(1:metadata_list$N_draws)) := NULL]

print("Make past draw corrections")

if (chaos) {
  chaos_output <- readRDS(paste0(
    root_fold,
    "/summary_files/TMP_chaos_oos_1.rds"
  ))
  chaos_output <- chaos_output[year >= eval(metadata_list$end_fit + 1)]
  chaos_output <- rbindlist(list(chaos_output, yvar_draws), use.names = T)
  setkeyv(chaos_output, c("iso3", "year"))

  saveRDS(chaos_output, paste0(root_fold, "/summary_files/chaos_oos_1.rds"))
} else {
  chaos_output <- readRDS(paste0(root_fold, "/summary_files/TMP_chaos_oos.rds"))
  chaos_output <- chaos_output[year >= eval(metadata_list$end_fit + 1)]
  chaos_output <- rbindlist(list(chaos_output, yvar_draws), use.names = T)
  setkeyv(chaos_output, c("iso3", "year"))

  saveRDS(chaos_output, paste0(root_fold, "/summary_files/chaos_oos.rds"))
}


## Finally, collect and cumsum the Chaos

#### Optional: if we were to divide or multiply by population, bring that data in:
pop_data <- fread(paste0(
  "/share/resource_tracking/forecasting/",
  variable, "/input_data/RT_2018_GDP_use.csv"
))[
  year %in% c(metadata_list$start_year:metadata_list$end_FC),
  .(iso3, year, total_pop)
]


## Cumsum all the data
chaos_list <- final_cum_sum(
  root_fold = root_fold,
  chaos = metadata_list$chaos,
  oos_years = metadata_list$oos_years,
  N_draws = metadata_list$N_draws,

  ## This is the reverse transformation to apply
  rev_trans = "invlogit",

  ### The following block will merge in the previously read population data
  ### and divide the draws by total_pop
  # pop_data = pop_data,
  # pop_var = 'total_pop',
  # pop_action = 'div',

  pop_data = NULL,
  pop_var = NULL,
  pop_action = NULL,

  ## The following flag is a special one-off thing we did for poverty
  scenario = F
)


########################################
#### (7) Correlate draws
########################################

## Correlated draws with AR1 covariance
## If you get singular matrix errors, then you can try turning off AR1 to FALSE
correlated_output <- correlate_draws_1D(
  draws = chaos_list[["draws"]],
  stats = chaos_list[["stats"]],
  metadata_list = metadata_list, AR1 = T
)

## Save out
write_feather(
  correlated_output[["draws"]],
  paste0(root_fold, "/summary_files/correlated_draws.feather")
)
fwrite(
  correlated_output[["stats"]],
  paste0(root_fold, "/summary_files/correlated_stats.csv")
)




########################################
#### (8) Make scenarios
########################################

## Quick loads
# correlated_output = list(); correlated_output[['stats']] = fread(paste0(root_fold, '/summary_files/correlated_stats.csv')); correlated_output[['draws']] = data.table(read_feather(paste0(root_fold, '/summary_files/correlated_draws.feather')))

### Get mean scenario lines
scenario_mean_lines <- mean_scenario_model(correlated_output$stats,
  metadata_list,
  aroc_years = 20, transform = "logit"
)

### Propagate draws of uncertainty
scenario_draws <- scenario_uncertainty(
  mean_data = scenario_mean_lines,
  draws_data = correlated_output$draws,
  metadata_list, transform = "logit"
)

## Save out scenario draws
write_feather(scenario_draws[["draws"]], paste0(
  root_fold,
  "/summary_files/scenario_draws.feather"
))
fwrite(scenario_draws[["stats"]], paste0(
  root_fold,
  "/summary_files/scenario_stats.csv"
))




########################################
#### (9) Upload stats to DB
########################################

#### NOTE: You can use get_entity_info() to get a list of
#### all the entity that's stored in the database

## Get entity info
entity_info <- get_entity_info("oop_per_cap")

## Get model info, and assign a model ID on whether we ran forecasts for that variable or not
model_info <- get_model_info()
model_num <- max(model_info$model_id) + 1


## Prep data for uploading
upload_prepz <- prep_for_db_uploading(
  root_fold = root_fold,
  metadata_list = metadata_list,
  entity_info = entity_info,
  model_num = model_num
)


## Upload!!
upload_data_to_db(upload_prepz)
