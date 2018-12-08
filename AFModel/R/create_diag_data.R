#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param new_model_data PARAM_DESCRIPTION
#' @param baseline_model PARAM_DESCRIPTION
#' @param scatter_yrs PARAM_DESCRIPTION
#' @param aroc_yrs PARAM_DESCRIPTION
#' @param terminal_yr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_diag_data
#' @export
create_diag_data <- function(new_model_data, baseline_model, scatter_yrs, aroc_yrs, terminal_yr) {

  ## Need exactly 3 in aroc_yrs
  if (length(aroc_yrs) != 3) {
    stop("Need vector of length 3 in aroc_yrs")
  }

  ## Get lox
  loxx <- get_lox()[level == 3, .(iso3, location_id, location_name)]

  ## Get baseline model and prep it with locations and etc
  baseline_data <- get_oots(entity_info$entity, model_id = baseline_model)
  baseline_data <- baseline_data[, .(location_id, scenario, year = year_id, old_mean = mean, old_upper = upper, old_lower = lower)]
  baseline_data <- merge(baseline_data, loxx, "location_id", all.x = T)
  baseline_data <- baseline_data[order(location_name, year)]

  new_model_data <- copy(new_model_data)
  setnames(new_model_data, c("mean", "lower", "upper"), paste0("new_", c("mean", "lower", "upper")))


  ## Merge old on to new
  data_merged <- merge(new_model_data, baseline_data, c("iso3", "scenario", "year"), all.x = T)
  data_merged[, location_name := NULL]
  data_merged[, location_id := NULL]
  data_merged <- merge(data_merged, loxx, "iso3", all.x = T)[year <= terminal_yr]


  ## Make necessary AROCs
  aroc_data <- data_merged[year %in% aroc_yrs]
  aroc_data[, aroc_new_mean := 100 * ((new_mean / shift(new_mean))**(1 / (year - shift(year))) - 1), by = c("iso3", "scenario")]
  aroc_data <- aroc_data[!is.na(aroc_new_mean), .(location_name, iso3, scenario, year, aroc_new_mean)]
  aroc_data <- data.table(dcast(aroc_data, iso3 + location_name + scenario ~ year, value.var = "aroc_new_mean"))

  return(list(
    data_merged = data_merged,
    aroc_data = aroc_data,
    scatter_yrs = scatter_yrs,
    aroc_yrs = aroc_yrs
  ))
}
