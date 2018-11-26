## Function to forecast
make_forecast <- function(mean_est_data_obj, tmb_output_object, tmb_data_param, add_residual = T, transform = NULL, ...) {

  # mean_est_data_obj = copy(mean_est_data)
  # tmb_output_object = copy(output_TMB)
  # tmb_data_param = copy(data_params)

  ## Extra args
  argss <- list(...)


  ## Get specifications
  specs <- tmb_data_param$specifications


  ## If specs$scaled_c, then we gotta have a back transform:
  if (specs$scaled_c & is.null(transform)) {
    stop("Scaled convergence needs a back transformer")
  }


  ## Loop over each year of forecas
  j <- 0
  for (yeer in c(eval(specs$end_fit):specs$end_FC)) {
    j <- j + 1

    # print(paste0('Predicting year: ', yeer))


    ## Fit all non time variant stuff

    ## global intercept
    mean_est_data_obj[year %in% yeer, pred := glob_int]

    ## Fixed effects
    if (!is.null(specs$xvar)) {
      mean_est_data_obj[year %in% yeer, pred := pred +
        eval(parse(text = paste0("b_", paste0(tmb_data_param$specifications$xvar, "*", tmb_data_param$specifications$xvar), collapse = "+")))]
    }

    ## Add random intercepts
    if (specs$z) {
      mean_est_data_obj[year %in% yeer, pred := pred + z_int]
    }

    ## Add random coefficients
    if (specs$z_coef == 1) {
      mean_est_data_obj[year %in% yeer, pred := pred +
        eval(parse(text = paste0("z_", paste0(tmb_data_param$specifications$re_vars, "*", tmb_data_param$specifications$re_vars), collapse = "+")))]
    }

    ## Add time variant stuff

    ## Make temp lag-level variable
    mean_est_data_obj[year %in% c(yeer - 1, yeer), temp_lev := shift(yvar), by = "iso3"]

    ## Make temp lag-diff variable
    mean_est_data_obj[year %in% c(yeer - 1, yeer), temp_diff := shift(ydiff), by = "iso3"]


    ## Add convergence term
    if (specs$c) {
      mean_est_data_obj[year %in% yeer, pred := pred + conv * temp_lev, by = "iso3"]
    }

    ## Add scaled convergence term
    if (specs$scaled_c) {
      mean_est_data_obj[year %in% yeer, pred := pred + scaled_conv * get(transform)(temp_lev) / conv_scaler, by = "iso3"]
    }


    ## Add AR terms (global or country)
    if (specs$rho >= 1) {
      if (specs$fd) {
        if (specs$ar_mod %in% c(1, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_global_", c(1:specs$rho)), "temp_diff", sep = " * "))), by = "iso3"]
        }
        if (specs$ar_mod %in% c(2, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_country_", c(1:specs$rho)), "temp_diff", sep = " * "))), by = "iso3"]
        }
      } else {
        if (specs$ar_mod %in% c(1, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_global_", c(1:specs$rho)), "temp_lev", sep = " * "))), by = "iso3"]
        }
        if (specs$ar_mod %in% c(2, 3)) {
          mean_est_data_obj[year %in% yeer, pred := pred + eval(parse(text = paste(paste0("ar_country_", c(1:specs$rho)), "temp_lev", sep = " * "))), by = "iso3"]
        }
      }
    }

    ##### End of single year prediction #####



    ### Intercept shifting if we have level space values ###
    ### We will capture the difference in levels in the last year of observed data
    ### which will add on to all of the level projections as intercept shift


    if (j == 1) {
      if (specs$fd == F) {
        ## Get the intercept shift difference
        mean_est_data_obj[, int_shift := mean(yvar - pred, na.rm = T), "iso3"]
      }
      mean_est_data_obj[, c("pred", "temp_lev", "temp_diff") := NULL]
    } else {
      ## Update yvar and ydiff according to the model space we used
      if (specs$fd) {
        mean_est_data_obj[year %in% yeer, ydiff := pred]
        mean_est_data_obj[year %in% yeer, yvar := pred + temp_lev]
      } else {

        ## Int shift
        mean_est_data_obj[year %in% yeer, yvar := pred + int_shift]
        mean_est_data_obj[year %in% yeer, ydiff := yvar - temp_lev]
      }

      mean_est_data_obj[, c("temp_lev", "temp_diff") := NULL]
    }
  }

  ## Simulate errors
  if (add_residual) {

    # print("Adding residuals")

    if (!("var_metric" %in% names(argss))) argss$var_metric <- "mad"
    if (!("cap_variance" %in% names(argss))) argss$cap_variance <- F
    if (!("var_threshold" %in% names(argss))) argss$var_threshold <- NULL

    ## Simulate errors
    rw_errors <- simulate_rw_forecast(
      tmb_output_object = tmb_output_object, tmb_data_params = tmb_data_param,
      var_metric = argss$var_metric, cap_variance = argss$cap_variance, var_threshold = argss$var_threshold
    )

    ## Merge on data
    mean_est_data_obj <- merge(mean_est_data_obj, rw_errors, c("iso3", "year"), all.x = T)
    mean_est_data_obj[is.na(rw), rw := 0]

    ## Add to levels
    mean_est_data_obj[, yvar := yvar + rw]

    ## Remake ydiff
    mean_est_data_obj[, ydiff := yvar - shift(yvar), by = "iso3"]
  }



  return(mean_est_data_obj[, .(iso3, year, yvar, ydiff)])
}
