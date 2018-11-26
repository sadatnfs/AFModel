############################################################
### Testing Inclusion Criteria
############################################################
### Coefficient significance
test_sig <- function(tmb_output_object, alpha = 0.1, intercept_test = T, rigorous = F) {

  # tmb_output_object = copy(output_TMB); alpha=0.1

  ## Get critical region value (2 sided)
  crit_value <- abs(qnorm(p = alpha / 2))


  ## Get the data.table with p-values from SDReport object
  pval_DT <- data.table(summary(tmb_output_object$FC_model, p.value = T), keep.rownames = T)
  colnames(pval_DT) <- c("param", "est", "se", "zval", "pval")

  ## Note: We don't need to test every single parameter's stat sig, so filter those out first

  #### BUT: If we are doing rigorous tests, then we should test everything except the country random effect estimates
  if (!rigorous) {
    pval_DT <- pval_DT[param %in% c(
      "b", "a", "logSigma", "logggroup", "loggrcoef", "c", "rho_global",
      "logSigma_rho_country", "theta_global", "var_theta_global", "var_theta_country"
    )]
  } else {
    pval_DT <- pval_DT[!(param %in% c("z", "z_coef"))]
  }


  ## Create criteria column
  pval_DT[, pass_sig_test := pval < 0.1 ]

  stat_sig <- 1

  ## If there are less number of TRUEs than the number of parameters, return 0
  if (sum(pval_DT$pass_sig_test) != nrow(pval_DT)) {
    stat_sig <- 0
  }

  print(pval_DT)
  return(stat_sig)
}
