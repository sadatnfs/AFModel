#### Simulate Residuals
simulate_rw_forecast <- function(tmb_output_object, tmb_data_params, var_metric = "mad", cap_variance = T, var_threshold = NULL) {

  # tmb_output_object = copy(output_TMB)
  # tmb_data_params = copy(data_params)

  ## Get the Y_hat from the TMB output (in-sample fit)
  y_output <- tmb_output_object$Y_hat

  ## Trim y-output to remove if we have AR/MA terms (first years of preds are naughty)
  y_output <- y_output[, eval(max(tmb_data_params$data$ma, tmb_data_params$data$ar) + 1):ncol(y_output)]


  ## If we are in first-diff space, then we can just get the SD of the first diff fits by country
  if (tmb_data_params$specifications$fd) {

    ## Nothing to do here
  } else {

    ## Get first diffs of y_output
    y_output <- y_output[, 2:ncol(y_output)] - y_output[, 1:eval(ncol(y_output) - 1)]
  }

  ## Get the variance measure of the y-diffs
  if (var_metric == "sd") {
    noise_diffs <- apply(y_output, 1, sd)
  }
  if (var_metric == "mad") {
    noise_diffs <- apply(y_output, 1, mad)
  }



  ## Create a data.table with country and standard diffs
  rw_mat <- data.table(iso3 = rownames(tmb_data_params$data$Y_input), noise = noise_diffs)

  ## Cap noise if needed
  if (cap_variance) {
    if (is.null(var_threshold)) {
      rw_mat[, le_cap := (qnorm(0.9) / .6745) * mad(noise) + median(noise)]
      rw_mat[noise > le_cap, noise := le_cap]
      rw_mat[, le_cap := NULL]
    } else {
      rw_mat[noise > var_threshold, noise := var_threshold]
    }
  }

  ## Create first column of zeroes (end_fit value)
  rw_mat[, paste0("yr", tmb_data_params$specifications$end_fit) := 0]

  ## Simulate normal distribution with 0 mean and sd of sd
  yr_vect <- eval(tmb_data_params$specifications$end_fit + 1):tmb_data_params$specifications$end_FC
  rw_mat[, paste0("yr", yr_vect) := lapply(yr_vect, function(x) rnorm(.N, 0, noise))]

  rw_mat[, noise := NULL]

  ## Melt long on yr stub
  rw_mat <- melt(rw_mat, "iso3", value.name = "rw", measure.vars = patterns("^yr"), variable.name = "year")
  rw_mat[, year := as.numeric(gsub("yr", "", year))]
  rw_mat[, rw := cumsum(rw), by = "iso3"]
  rw_mat <- rw_mat[year >= yr_vect[1]]

  return(rw_mat)
}
