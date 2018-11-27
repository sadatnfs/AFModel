#### OOS generator for NON CHAOS
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mean_data PARAM_DESCRIPTION
#' @param OOS_data PARAM_DESCRIPTION
#' @param panelvar PARAM_DESCRIPTION
#' @param yearvar PARAM_DESCRIPTION
#' @param depvar_dt PARAM_DESCRIPTION
#' @param oos_start PARAM_DESCRIPTION
#' @param in_sample_end PARAM_DESCRIPTION
#' @param region_metrics PARAM_DESCRIPTION, Default: F
#' @param region_data PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname RMSE_generator
#' @export 
RMSE_generator <- function(mean_data, OOS_data, panelvar, yearvar,
                           depvar_dt, oos_start, in_sample_end,
                           region_metrics = F, region_data = NA) {

  ## Merge in the two datasets we'll compare values from (depending on which space we compute the RMSE in)
  d1 <- copy(mean_data)[get(yearvar) <= in_sample_end, .SD, .SDcols = c(panelvar, yearvar, depvar_dt)]
  colnames(d1)[3] <- "truth"
  d2 <- copy(OOS_data)[, .SD, .SDcols = c(panelvar, yearvar, depvar_dt) ]
  colnames(d2)[3] <- "OOS"
  RMSE_data <- merge(d1, d2, c(panelvar, yearvar), all = T)


  ### Make sure that region_metrics has the right column names to use
  if (!is.na(region_metrics)) {
    if (sum(c(panelvar, "region_name", "super_region_name") %in% colnames(region_data)) != 3) {
      stop("Region metrics need a properly names region_data data.table")
    } else {

      ## Merge in region_data with RMSE data
      RMSE_data <- merge(RMSE_data, region_data[, .SD, .SDcols = c(panelvar, "region_name", "super_region_name")], panelvar, all.x = T)
    }
  }


  ## Compute RMSE
  RMSE_data <- RMSE_data[get(yearvar) >= oos_start]
  RMSE_data[, error_sq := (truth - OOS)**2]


  ## Get CS and global RMSEs
  RMSE_data[, rmse_iso := sqrt(mean(error_sq)), panelvar]
  RMSE_data[, rmse_global := sqrt(mean(error_sq, na.rm = T)), ]



  ##### If we're gonna get region metric:
  if (region_metrics) {
    RMSE_data[, rmse_REG := sqrt(mean(error_sq, na.rm = T)), "region_name"]
    RMSE_data[, rmse_SRREG := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
  }


  ## Keep unique
  RMSE_data <- RMSE_data[get(yearvar) == oos_start]




  # Prep output
  if (region_metrics) {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, "rmse_iso", "rmse_REG", "rmse_SRREG", "rmse_global")]
  } else {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, "rmse_iso", "rmse_global")]
  }


  return(RMSE_data)
}
