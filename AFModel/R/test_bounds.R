### Percent change bounds
test_bounds <- function(data, depvar, predict_years, rev_trans, panelvar, yearvar, sfa_output, threshold = 0) {

  ## Compute year over year percent change
  data_pc <- copy(data)

  data_pc[, upper_bound := exp(sfa_output$upper_beta * (get(paste0(depvar))) + sfa_output$upper_intercept) ]
  data_pc[, lower_bound := -exp(sfa_output$lower_beta * (get(paste0(depvar))) + sfa_output$lower_intercept) ]

  # data_pc[, pc := (shift(log(get(depvar_in_levels)), 1, type = "lead") - log(get(depvar_in_levels)))*100/log(get(depvar_in_levels)), panelvar ]

  data_pc[, pc := (shift(eval(parse(text = paste0(rev_trans, "(", depvar, ")"))), 1, type = "lead") - (eval(parse(text = paste0(rev_trans, "(", depvar, ")"))))) * 100 / eval(parse(text = paste0(rev_trans, "(", depvar, ")"))), panelvar]

  ### Keep if the percent change is within the bound
  data_pc <- data_pc[get(yearvar) %in% c(eval(predict_years[1]) + 1:predict_years[2])]
  data_pc[pc > upper_bound & pc > 0, sfa_up := 1]
  data_pc[pc < lower_bound & pc < 0, sfa_low := 1]

  bounds <- 1
  print(sum_up <- sum(data_pc[, sfa_up], na.rm = T))
  print(sum_low <- sum(data_pc[, sfa_low], na.rm = T))

  if (sum_up > threshold | sum_low > threshold) {
    bounds <- 0
  }

  return(bounds)
}
