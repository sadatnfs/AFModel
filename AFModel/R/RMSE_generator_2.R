#### OOS generator for CHAOS
RMSE_generator_2 <- function(mean_data, OOS_data, panelvar, yearvar,
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


  ## With this scheme:
  ###### OOS at year 1 will be based on 1st 2 years of forecasts;
  ###### OOS at year 2 will be based on 2/3/4th years of forecasts;
  ###### OOS at year 3 will be based on 3/4/5th years of forecasts;
  ## ... OOS at year T-1 will be based on T-2/T-1/T years of forecasts;
  ## ... OOS at year T will be the same values as of T-1;

  ## Start by getting the length of OOS years first
  oos_vector <- c(eval(oos_start):in_sample_end)
  length_of_OOS_yrs <- length(oos_vector)

  ## Get a neighboring vector
  oo <- 0
  for (i in oos_vector) {
    oo <- oo + 1

    if (oo == 1) {
      RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_", i) := sqrt(mean(error_sq)), panelvar]
      RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_global_", i) := sqrt(mean(error_sq, na.rm = T))]
    }

    if (oo > 1 & oo < length_of_OOS_yrs) {
      RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_", i) := sqrt(mean(error_sq)), panelvar]
      RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_global_", i) := sqrt(mean(error_sq, na.rm = T)), ]
    }

    if (oo == length_of_OOS_yrs) {
      RMSE_data[get(yearvar) >= i - 2, paste0("rmse_", i) := sqrt(mean(error_sq)), panelvar]
      RMSE_data[get(yearvar) >= i - 2, paste0("rmse_global_", i) := sqrt(mean(error_sq, na.rm = T)), ]
    }
  }


  ##### If we're gonna get region metric:
  if (region_metrics) {
    ## Get a neighboring vector
    oo <- 0
    for (i in oos_vector) {
      oo <- oo + 1

      if (oo == 1) {
        RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_REG_", i) := sqrt(mean(error_sq, na.rm = T)), "region_name"]
        RMSE_data[get(yearvar) == i | get(yearvar) == eval(i + 1), paste0("rmse_SRREG_", i) := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
      }

      if (oo > 1 & oo < length_of_OOS_yrs) {
        RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_REG_", i) := sqrt(mean(error_sq, na.rm = T)), "region_name"]
        RMSE_data[get(yearvar) >= i - 1 & get(yearvar) <= eval(i + 1), paste0("rmse_SRREG_", i) := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
      }

      if (oo == length_of_OOS_yrs) {
        RMSE_data[get(yearvar) >= i - 2, paste0("rmse_REG_", i) := sqrt(mean(error_sq, na.rm = T)), "region_name"]
        RMSE_data[get(yearvar) >= i - 2, paste0("rmse_SRREG_", i) := sqrt(mean(error_sq, na.rm = T)), "super_region_name"]
      }
    }
  }

  # Get a single column of RMSE

  for (i in oos_vector) {
    RMSE_data[get(yearvar) == i, rmse_iso := get(paste0("rmse_", i))]
    RMSE_data[get(yearvar) == i, rmse_global := get(paste0("rmse_global_", i))]

    if (region_metrics) {
      RMSE_data[get(yearvar) == i, rmse_REG := get(paste0("rmse_REG_", i))]
      RMSE_data[get(yearvar) == i, rmse_SRREG := get(paste0("rmse_SRREG_", i))]
    }
  }

  if (region_metrics) {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, yearvar, "rmse_iso", "rmse_REG", "rmse_SRREG", "rmse_global")]
  } else {
    RMSE_data <- RMSE_data[, .SD, .SDcols = c(panelvar, yearvar, "rmse_iso", "rmse_global")]
  }


  ## Get OOS and model number indicator
  RMSE_data[, oos := get(yearvar) - oos_start + 1]


  return(RMSE_data)
}
