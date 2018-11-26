## Run copula for each country
copula_by_country <- function(draws_array, iso, input_corr, metadata_list, AR1 = T) {

  # Create a Correlation matrix (if using AR(1))
  if (AR1) {
    corr_mat <- (input_corr[iso3 == paste0(iso), corrs])**abs(outer(
      0:(metadata_list$end_FC - eval(metadata_list$end_fit)),
      0:(metadata_list$end_FC - eval(metadata_list$end_fit)), "-"
    ))
  } else {
    ## Make square matrix
    corr_mat <- matrix(rep(input_corr[iso3 == paste0(iso), corrs], (metadata_list$end_FC - eval(metadata_list$end_fit) + 1)**2),
      nrow = metadata_list$end_FC - eval(metadata_list$end_fit) + 1
    )

    ## Set diagnoals to 1
    diag(corr_mat) <- 1
  }


  ## Correlate the array for each country
  draws_corr <- data.table(t(draw1Dcopula(X = draws_array[paste0(iso), , ], corr_mat = corr_mat)))

  ## Make it nice and pretty to be returned
  colnames(draws_corr) <- paste0("draw_", c(1:metadata_list$N_draws))
  draws_corr[, year := metadata_list$end_fit:metadata_list$end_FC]
  draws_corr[, iso3 := paste0(iso)]

  setcolorder(draws_corr, c("iso3", "year", paste0("draw_", c(1:metadata_list$N_draws))))

  return(draws_corr)
}
