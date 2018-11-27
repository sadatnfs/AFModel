######################################################################
### Scenarios
######################################################################
## Create the mean scenario trajectories, based on the convergence regressions
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param metadata_list PARAM_DESCRIPTION
#' @param aroc_years PARAM_DESCRIPTION
#' @param transform PARAM_DESCRIPTION, Default: 'log'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean_scenario_model
#' @export 
mean_scenario_model <- function(data, metadata_list, aroc_years, transform = "log") {

  #### Input: a dataframe with iso3, year, and mean columns
  ### metadata list : for year info
  ### aroc_years : how many years to use for growth regression

  ## Error out if we are trying to AROC more years than what exists in the observed data
  if (aroc_years > metadata_list$end_fit - metadata_list$start_year) {
    stop(paste0("Not enough years of observed data to compute ", aroc_years, " years worth of AROC."))
  }

  ## First, take out the values of mean, and keep only the years to be used in the conv reg
  first_yr_scen <- metadata_list$end_fit - aroc_years
  second_yr_scen <- metadata_list$end_fit
  third_yr_scen <- metadata_list$end_fit + aroc_years

  scen_data <- data[year %in% c(first_yr_scen, second_yr_scen, third_yr_scen),
    .(year, gr_rate = get(transform)(mean) - shift(get(transform)(mean)), conv_future = get(transform)(mean)),
    by = "iso3"
  ]

  ## Anchor everything at the second_yr_scen point
  scen_data[, conv_past := shift(conv_future)]
  scen_data <- scen_data[year == second_yr_scen]


  ## Run regression
  summary(scenario_reg <- lm(data = scen_data, formula = gr_rate ~ 1 + conv_past))

  ## Record the intercept, coefficient, and the worse/better quantiles
  int_val <- coef(scenario_reg)[1]
  conv_coef_val <- coef(scenario_reg)[2]
  resid_worse <- quantile(residuals(scenario_reg), 0.15)
  resid_better <- quantile(residuals(scenario_reg), 0.85)


  ## Start by making the baseline 'constant' values
  better_const <- exp(int_val + resid_better)**(1 / aroc_years)
  worse_const <- exp(int_val + resid_worse)**(1 / aroc_years)


  ## Prep our output dataset
  output_data <- data[, .(iso3, year, reference = (mean))]

  ## Make placeholders for better and worse logged
  output_data[year <= metadata_list$end_fit, better := reference]
  output_data[year <= metadata_list$end_fit, worse := reference]


  ## Loop over each year from t+1 ... T, and make predictions where:
  ## Y_t = Y_t-1 * constant *  (Y_t-1**beta)
  for (yrs in c(eval(metadata_list$end_fit + 1):metadata_list$end_FC)) {

    ## Create placeholder variables for better and worse values with a lag
    output_data[, better_lag := shift(better), by = "iso3"]
    output_data[, worse_lag := shift(worse), by = "iso3"]

    ## Create B/W forecasts at 'yrs'
    output_data[year == yrs, better := (better_lag * better_const * (better_lag**conv_coef_val)**(1 / aroc_years))]
    output_data[year == yrs, worse := (worse_lag * worse_const * (worse_lag**conv_coef_val)**(1 / aroc_years))]

    output_data[, better_lag := NULL]
    output_data[, worse_lag := NULL]
  }

  ## Cap if better<reference or worse>reference
  output_data[worse > reference, worse := reference]
  output_data[better < reference, better := reference]

  return(output_data)
}
