
###################################################
### Author:  Nafis Sadat
### Purpose: Central script to hold and submit forecasting jobs
## qsub -N master_script -pe multi_slot 4 -P proj_fin_forecast -j y -o  /share/temp/sgeoutput/$USER/output /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh 1 /path/to/master/script.R
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


## Prep empty folders and return root
root_fold <- prep_model(variable, comment, date, erase = F)


## Set forecasting year cutoff info
start_year <- 1970
end_fit <- 2017
end_FC <- 2100
oos_years <- 15
N_draws <- 1000

## Pack all the metadata in a list
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
  N_draws = 1000
)

# root_fold = prep_model(variable, comment, date, erase = T)


## Script to data and paths
mean_script <- "/path/to/mean_forecast_model.R"
draw_script <- "/path/to/forecast_draws.R"
chaos_script <- "/path/to/chaos_compile.R"
draw_data <- "/path/to/draws.feather"

########################################
#### (2) Ensemble information
########################################

## Create a metadata for the ensemble grid innards
ensemble_metadata <- list(
  yvar = c(yvv),
  xvar = paste0("FE_", c(
    "ln_TFR",
    "edu"
  )),
  re_coef = NULL,
  ar = c(0, 1, 2, 3),
  ar_mod = c(1, 3),
  ma = c(0),
  ma_mod = 0,
  weight_decays = seq(0, 0.7, length.out = 11),
  global_int = 1, country_int = 1, country_int_dist = 1,
  fdiff = 1,
  conv = c(0),
  scaled_lev_conv = c(0, 1),
  ar_constrain = 0
)


## Create ensemble model grid for GDP and GDPpc
GDPs_full_grid <- do.call(create_ensemble_grid, ensemble_metadata)


## List our coefficient priors
coef_priors <- list(
  "conv" = -1, "FE_logit_pop15" = -1,
  "scaled_c" = -1,
  "FE_logit_pop65" = 1,
  "FE_ln_TFR" = -1, "FE_edu" = 1
)


## Save them in root
save(list = c("metadata_list", "GDPs_full_grid", 
              "ensemble_metadata", "coef_priors"), 
     file = paste0(root_fold, "/summary_files/mean_array.Rdata"))



########################################
#### (3) Submit mean jobs
########################################


## Create qsub command
proj <- " -P proj_fin_forecast "
slots <- " -pe multi_slot 1"
arrays <- paste0("1:", nrow(GDPs_full_grid))
# arrays = '1:2'
qsub_mean <- paste0(
  "qsub -N mean_job_", comment, " ",
  proj,
  slots,
  " -t ", arrays,
  " /share/singularity-images/health_fin/forecasting/shells/",
  "health_fin_forecasting_shell_singularity.sh ",
  mean_script,
  " --variable ", variable, 
  " --date ", date, 
  " --comment ", comment, 
  " --oos ", oos_years
)


## Submit!
system(qsub_mean)


## Keep sleeping until donesies
job_hold(paste0("mean_job_", comment))




########################################
#### (4) Check and collect ensemble
########################################

# # Did all the models finish running? (and return models)
# failed_models = mean_run_check(root_fold = root_fold, mean_array_grid = GDPs_full_grid, return_fail = T)
#
# ## Qsubbing for failed jobs
# arrays_failed = failed_models
# proj = ' -P proj_fin_forecast '
# slots = ' -pe multi_slot 1'
#
# lapply(failed_models, function(i) {
#   system(paste0('qsub -N GDP_mean_', comment, ' ',
#          proj,
#          slots,
#          ' -t ' , i,
#          ' -j y -o /share/temp/sgeoutput/sadatnfs/output ',
#          ' /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh ',
#          ' 1 ~/forecasting/gdp/code/03_mean_forecast_model.r ',
#          ' --variable ', variable, ' --date ', date, ' --comment ', comment, ' --oos ', oos_years))
#   return(NULL)
# })


## Quick load:
# load(paste0(root_fold, '/summary_files/mean_array.Rdata'))


## Get list of models passed
passed_models <- mean_run_check(
  root_fold = root_fold,
  mean_array_grid = GDPs_full_grid,
  return_fail = F
)


## Bring in all the postfiles
data_models_passed <- mean_postfiles_compile(root_fold, 
                                             passed_models, mc.cores = 4)
save(list = "data_models_passed", 
     file = paste0(root_fold, "/summary_files/data_models_passed.Rdata"))


## Apply coefficient direction priors
data_models_passed <- prior_coefs_mean(
  data = data_models_passed,
  coef_prior_list = coef_priors
)



## Distribute draws and chaos RMSE and SRREG and all that. This works regardless of Chaos flag
rmse_distro <- chaos_draw_distro_DMC(
  data = data_models_passed, chaos_pc = 10, 
  metadata = ensemble_metadata,
  iso_portion = 1, region_portion = 0, 
  super_region_portion = 0, N_draws = N_draws
)



## Save out metadata and distro
save(list = c("rmse_distro", "metadata_list"), 
     file = paste0(root_fold, "/summary_files/draws_array.Rdata"))



## Quick load:
# load(paste0(root_fold, '/summary_files/draws_array.Rdata'))

########################################
#### (5) Submit draws
########################################


## Job environment
slotz <- 3
mem <- slotz * 1

proj <- " -P proj_fin_forecast "
slots <- paste0(" -pe multi_slot ", slotz) # , ' -l mem_free=', slotz*2, 'g')
arrays <- paste0("1:", nrow(rmse_distro$array_grid))

shell_file <- " /share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_singularity.sh "

## Empty out draws: # system(paste0('rm ', root_fold, '/draws/*'))
## Make qsub line and submit
qsub_draws <- paste0(
  "qsub -N draws_job_", comment, " ",
  proj,
  slots,
  " -t ", arrays,
  " -j y -o /share/temp/sgeoutput/sadatnfs/output ",
  shell_file,
  draw_script,
  " --variable ", variable, 
  " --date ", date, 
  " --comment ", comment
)

## Submit!
system(paste0(qsub_draws))

## Keep sleeping until donesies
job_hold(paste0("draws_job_", comment))




########################################
#### (6) Check and collect draws (Chaos or not)
########################################

## Quick load:
# load(paste0(root_fold, '/summary_files/draws_array.Rdata'))


## Check if draws have finished running successfully for all the models
draw_run_check(root_fold = root_fold, array_grid = rmse_distro$array_grid)

## Submit Chaos code for each OOS year
slotz <- 4
proj <- " -P proj_fin_forecast "
slots <- paste0(" -pe multi_slot ", slotz) # , ' -l mem_free=', slotz*2, 'g')



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
    " -j y -o /share/temp/sgeoutput/sadatnfs/output ",
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
chaos_collector_check(root_fold = root_fold, 
                      oos_years = metadata_list$oos_years, 
                      chaos = metadata_list$chaos)



########################################
#### (7) Fix past draw data because missingness and alignment with GPR draws ######
########################################


lox <- get_lox()[level == 3, iso3]

## Get draws
yvar_draws <- data.table(read_feather(draw_data,
  columns = c("iso3", "year", paste0("draw_", c(1:1000)))
))


yvar_draws <- yvar_draws[(year %in% c(metadata_list$start_year:eval(metadata_list$end_fit))) & (iso3 %in% lox)]
yvar_draws <- yvar_draws[order(iso3, year)]

colnames(yvar_draws) <- c("iso3", "year", 
                          paste0("draw_", c(1:metadata_list$N_draws)))

## Set zreoes to small lower bound
yvar_draws[yvar_draws == 0] <- 0.0000001



## Diff the draws BEYOND the first year only!
yvar_draws[, paste0("draw_", c(1:metadata_list$N_draws)) := lapply(
  paste0("draw_", c(1:metadata_list$N_draws)),
  function(v) log(get(v))
)]

yvar_draws[, paste0("diff_draw_", c(1:metadata_list$N_draws)) := lapply(
  paste0("draw_", c(1:metadata_list$N_draws)),
  function(v) get(v) - data.table::shift(get(v))
), by = "iso3"]

yvar_draws[year > metadata_list$start_year, paste0("draw_", 
                                                   c(1:metadata_list$N_draws)) := lapply(
  paste0("diff_draw_", c(1:metadata_list$N_draws)),
  function(v) (get(v))
)]
yvar_draws[, paste0("diff_draw_", c(1:metadata_list$N_draws)) := NULL]

print("Make past draw corrections")

if (chaos) {
  chaos_output <- readRDS(paste0(root_fold, 
                                 "/summary_files/TMP_chaos_oos_1.rds"))
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


## Use population data to either divvy or multiply
pop_data <- fread(paste0("/share/resource_tracking/forecasting/", 
                         variable, "/input_data/RT_2018_GDP_use.csv"))[
  year %in% c(metadata_list$start_year:metadata_list$end_FC), 
  .(iso3, year, total_pop)
]


## Cumsum all the data and convert to GDP per capita (we'll convert to per labour later on)
chaos_list <- final_cum_sum(
  root_fold = root_fold,
  chaos = metadata_list$chaos,
  oos_years = metadata_list$oos_years,
  N_draws = metadata_list$N_draws,
  rev_trans = "exp",
  # pop_data = pop_data,
  # pop_var = 'total_pop',
  # pop_action = 'div',
  pop_data = NULL,
  pop_var = NULL,
  pop_action = NULL,
  scenario = F
)


########################################
#### (7) Correlate draws
########################################

## Correlated draws with AR1 covariance
correlated_output <- correlate_draws_1D(draws = chaos_list[["draws"]], 
                                        stats = chaos_list[["stats"]], 
                                        metadata_list = metadata_list, AR1 = T)

## Save out
write_feather(correlated_output[["draws"]], 
              paste0(root_fold, "/summary_files/correlated_draws.feather"))
fwrite(correlated_output[["stats"]], 
       paste0(root_fold, "/summary_files/correlated_stats.csv"))




########################################
#### (8) Make scenarios
########################################

## Quick loads
# correlated_output = list(); correlated_output[['stats']] = fread(paste0(root_fold, '/summary_files/correlated_stats.csv')); correlated_output[['draws']] = data.table(read_feather(paste0(root_fold, '/summary_files/correlated_draws.feather')))

### Get mean scenario lines
scenario_mean_lines <- mean_scenario_model(correlated_output$stats, 
                                           metadata_list, aroc_years = 25, transform = "log")

### Propagate draws
scenario_draws <- scenario_uncertainty(
  mean_data = scenario_mean_lines,
  draws_data = correlated_output$draws,
  metadata_list, transform = "log"
)

## Save out
write_feather(scenario_draws[["draws"]], paste0(root_fold, 
                                                "/summary_files/scenario_draws.feather"))
fwrite(scenario_draws[["stats"]], paste0(root_fold, 
                                         "/summary_files/scenario_stats.csv"))




########################################
#### (9) Upload stats to DB
########################################


## Get entity info
entity_info <- get_entity_info("gdp_per_cap")

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
