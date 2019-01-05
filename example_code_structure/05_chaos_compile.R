


################################################
#### Author: Nafis Sadat
#### Purpose: Compiling Chaos Draws
################################################


rm(list = ls())
library(AFModel)

################################################
## (1) Get Variable information
################################################



## Parse the args and get model info

## Open the parser
parser <- ArgumentParser()

## Specify version and description
parser$add_argument("--variable", required = T, type = "character", help = "Variable")
parser$add_argument("--date", required = T, type = "integer", help = "Date")
parser$add_argument("--comment", required = T, type = "character", help = "Comment")
parser$add_argument("--oos", required = T, type = "integer", help = "OOS Year")
parser$add_argument("--chaos", required = T, type = "character", help = "Use Chaos?")

argss <- parser$parse_args()
variable <- argss$variable
date <- argss$date
comment <- argss$comment
oos_years <- argss$oos
chaos <- argss$chaos


print("Getting run and draws grid")
root_fold <- prep_model(variable, comment, date, erase = F)
load(paste0(root_fold, "/summary_files/draws_array.Rdata"))




################################################
## (2) Bring in draws
################################################


## Set up foreach environment
registerDoParallel(cores = 15)


print("Bring in the draws to memory")
system.time(draws_data <- foreach(f = rmse_distro$array_grid[, model_number]) %dopar% {

  ## Load the diff draws
  return(data.table(read_feather(paste0(root_fold, "/draws/", f, ".feather"))))
})


## Assign names so that we can call the list by model numbers
names(draws_data) <- rmse_distro$array_grid[, model_number]

## Unique Country list
country_list <- unique(rmse_distro$data$iso3)

## Total numbah of draws
N_draws <- rmse_distro$N_draws


################################################
## (3) Prep years
################################################



## Make the OOS values for Chaos
year_oos <- lapply(c(1:metadata_list$oos_years), function(x) {
  if (x == 1) {
    years <- c(metadata_list$start_year:eval(metadata_list$end_fit + 1))
  } else if (x == metadata_list$oos_years) {
    years <- c(eval(metadata_list$end_fit + x):eval(metadata_list$end_FC))
  } else {
    years <- c(eval(metadata_list$end_fit + x))
  }
})


################################################
## (4) Compute and cumsum!
################################################


if (chaos == "T" | chaos == T | chaos == "TRUE") {
  print("Compile the diff draws for a single OOS unit and Chaos them")
  system.time(all_draws_for_now <- country_looper(
    oos_input = oos_years, years = year_oos[[oos_years]], draw_type = "diff",
    input_rmse_data = rmse_distro$data, draws_data = draws_data
  ))


  if (oos_years == 1) {
    print("Get just the first year of draw in levels if OOS == 1")
    system.time(first_lev_year <- country_looper(oos_years, metadata_list$start_year,
      draw_type = "level",
      input_rmse_data = rmse_distro$data, draws_data = draws_data
    ))
    colnames(first_lev_year) <- c("iso3", "year", paste0("lev_", c(1:N_draws)))


  }

  stopImplicitCluster()


  print("Save out the draws")

  if (oos_years == 1) {
    saveRDS(all_draws_for_now,
      file = paste0(root_fold, "/summary_files/TMP_chaos_oos_", oos_years, ".rds")
    )
  } else {
    saveRDS(all_draws_for_now,
      file = paste0(root_fold, "/summary_files/chaos_oos_", oos_years, ".rds")
    )
  }
} else {
  print("Compile the diff draws for all years except first year")
  system.time(all_draws_for_now <- country_looper(
    oos_input = NA, years = c(eval(metadata_list$start_year + 1):metadata_list$end_FC),
    draw_type = "diff", input_rmse_data = rmse_distro$data, draws_data = draws_data
  ))


  setkeyv(all_draws_for_now, c("iso3", "year"))

  stopImplicitCluster()

  print("Save out the draws")
  saveRDS(all_draws_for_now,
    file = paste0(root_fold, "/summary_files/TMP_chaos_oos.rds")
  )
}


q("no")
