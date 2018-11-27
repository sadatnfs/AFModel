#### Propagate uncertainty in transformed space
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mean_data PARAM_DESCRIPTION
#' @param draws_data PARAM_DESCRIPTION
#' @param metadata_list PARAM_DESCRIPTION
#' @param transform PARAM_DESCRIPTION, Default: 'log'
#' @param reverse_scenario PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname scenario_uncertainty
#' @export 
scenario_uncertainty <- function(mean_data, draws_data, metadata_list, transform = "log", reverse_scenario = F) {


  ## Check column requirements
  if (any(!colnames(mean_data) %in% c("iso3", "year", "reference", "better", "worse"))) {
    stop("Column names for mean_data must have iso3, year, reference, better, worse")
  }
  if (any(!colnames(draws_data) %in% c("iso3", "year", paste0("draw_", c(1:metadata_list$N_draws))))) {
    stop("Column names must have iso3, year, drawz")
  }

  ## Set inverse transform calls
  if (transform == "log") rev_trans <- "exp"
  if (transform == "logit") rev_trans <- "invlogit"
  if (transform == "level") rev_trans <- "unity"

  if (any(!transform %in% c("log", "logit", "level"))) {
    stop("Transformation must be one of: log, logit, level")
  }

  print("Take the draws dataset, log things, get the rowMeans and create a deviation matrix")

  # draws_data = copy(correlated_output$draws)

  ### NOTE: We do this extra line of subset to not have data.table modify the draws inplace
  draws_data <- draws_data[, .SD, .SDcols = c("iso3", "year", paste0("draw_", c(1:metadata_list$N_draws)))]
  draws_data[, log_ref_mean := log(rowMeans(.SD)), .SDcols = paste0("draw_", c(1:metadata_list$N_draws))]
  draws_data[, paste0("draw_", c(1:metadata_list$N_draws)) := lapply(paste0("draw_", c(1:metadata_list$N_draws)), function(x) get(transform)(get(x)))]
  draws_data[, paste0("dev_", c(1:metadata_list$N_draws)) := lapply(
    paste0("draw_", c(1:metadata_list$N_draws)),
    function(x) (get(paste0(x)) - log_ref_mean)
  ) ]



  print("Replicate base data to make better and worse")
  dev_data <- draws_data[, .SD, .SDcols = c("iso3", "year", paste0("dev_", c(1:metadata_list$N_draws)))]
  dev_data <- dev_data[rep(1:.N, 3)][, scenario := c("reference", "better", "worse"), by = c("iso3", "year")]
  # dev_data <- dev_data[, .SD, .SDcols = c('iso3', 'year', 'scenario', paste0('dev_', c(1:metadata_list$N_draws)))]

  print("Now, take the mean_data DT, melt by scenario, and merge on the deviation")
  draws_scenario <- melt(mean_data, c("iso3", "year"), variable.name = "scenario", value.name = "data")
  draws_scenario[, log_data := log(data)]
  draws_scenario <- merge(dev_data, draws_scenario, c("iso3", "scenario", "year"), all.x = T)

  ## Let's encode scenarios to FBD esque style
  draws_scenario[, scen := 0]

  ### Reverse scenario if asked
  if (reverse_scenario) {
    draws_scenario[scenario == "worse", scen := 1]
    draws_scenario[scenario == "better", scen := -1]
  } else {
    draws_scenario[scenario == "worse", scen := -1]
    draws_scenario[scenario == "better", scen := 1]
  }

  draws_scenario[, scenario := NULL]
  setnames(draws_scenario, "scen", "scenario")

  print("Apply deviation to all draws")
  draws_scenario[, paste0("draw_", c(1:metadata_list$N_draws)) := lapply(c(1:metadata_list$N_draws), function(x) get(rev_trans)(get(paste0("dev_", x)) + log_data)) ]

  draws_scenario <- draws_scenario[, .SD, .SDcols = c("iso3", "year", "scenario", paste0("draw_", c(1:metadata_list$N_draws)))]


  print("Make stats")
  scenario_stats <- stat_maker(data = draws_scenario, melt = T, merge = F, idvar = c("iso3", "scenario", "year"))


  ## Return stuff
  list_out <- list()
  list_out[["draws"]] <- draws_scenario
  list_out[["stats"]] <- scenario_stats
  return(list_out)
}
