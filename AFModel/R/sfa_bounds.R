### Compute the empirically derived upper and lower bounds
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param depvar PARAM_DESCRIPTION
#' @param transform PARAM_DESCRIPTION
#' @param panelvar PARAM_DESCRIPTION
#' @param yearvar PARAM_DESCRIPTION, Default: 'year'
#' @param plot PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[arm]{invlogit}}
#'  \code{\link[frontier]{sfa}}
#'  \code{\link[ggplot2]{ggplot}}
#' @rdname sfa_bounds
#' @export 
#' @importFrom arm invlogit
#' @importFrom frontier sfa
#' @importFrom ggplot2 ggplot
sfa_bounds <- function(data, depvar, transform, panelvar, yearvar = "year", plot = F) {

  ## Copy out and get YoY percent changes
  sfa_prep <- copy(data)

  if (transform == "log") {
    sfa_prep[, yoy_pc := 100 * (shift(exp(get(depvar)), type = "lead") - (exp(get(depvar)))) / ((exp(get(depvar)))), panelvar]
  }
  if (transform == "logit") {
    sfa_prep[, yoy_pc := 100 * (shift(arm::invlogit(get(depvar)), type = "lead") - (arm::invlogit(get(depvar)))) / ((arm::invlogit(get(depvar)))), panelvar]
  }
  if (transform == "logit_trans") {
    sfa_prep[, yoy_pc := 100 * (shift(inv_logit_trans(get(depvar)), type = "lead") - (inv_logit_trans(get(depvar)))) / ((inv_logit_trans(get(depvar)))), panelvar]
  }
  if (transform == "custom") {
    sfa_prep[, yoy_pc := 100 * (shift(custom_inv_fn(get(depvar)), type = "lead") - (custom_inv_fn(get(depvar)))) / ((custom_inv_fn(get(depvar)))), panelvar]
  }

  sfa_prep <- sfa_prep[yoy_pc < 100 & yoy_pc > -100  ]

  ## Split the obs into positive and negative pc changes and take the log
  sfa_prep[yoy_pc > 0, log_pc := log(yoy_pc)]
  sfa_prep[yoy_pc < 0, neg_log_pc := log(abs(yoy_pc))]


  ## Outlier stuff
  sfa_prep <- sfa_prep[(log_pc > -6 & log_pc < 6) | is.na(log_pc)]
  sfa_prep <- sfa_prep[(neg_log_pc > -6 & neg_log_pc < 6) | is.na(neg_log_pc)]

  ## Separate out the neg and pos DTs
  sfa_neg <- sfa_prep[!is.na(neg_log_pc) & !is.na(depvar), .SD, .SDcols = c(panelvar, yearvar, "yoy_pc", depvar, "neg_log_pc")]
  sfa_pos <- sfa_prep[!is.na(log_pc) & !is.na(depvar), .SD, .SDcols = c(panelvar, yearvar, "yoy_pc", depvar, "log_pc")]

  ## Run SFA on each of the bounds
  sfa_model_pos <- frontier::sfa(data = sfa_pos, formula = log_pc ~ 1 + get(depvar))
  sfa_model_neg <- frontier::sfa(data = sfa_neg, formula = neg_log_pc ~ 1 + get(depvar))

  ## Extract the coefficients and fitted values
  sfa_pos[, intercept := coef(sfa_model_pos)[1]]
  sfa_pos[, upper_beta := coef(sfa_model_pos)[2]]
  sfa_pos[, upper_frontier := exp(fitted(sfa_model_pos))]

  sfa_neg[, intercept := coef(sfa_model_neg)[1]]
  sfa_neg[, lower_beta := coef(sfa_model_neg)[2]]
  sfa_neg[, lower_frontier := -exp(fitted(sfa_model_neg))]

  ## Order by x
  sfa_neg <- sfa_neg[order(get(depvar))]
  sfa_pos <- sfa_pos[order(get(depvar))]

  ## Merge on with sfa_prep
  sfa_prep <- merge(sfa_prep, sfa_pos[, .SD, .SDcols = c(panelvar, yearvar, depvar, "log_pc")], c(panelvar, yearvar), all.x = T)
  sfa_prep <- merge(sfa_prep, sfa_neg[, .SD, .SDcols = c(panelvar, yearvar, depvar, "neg_log_pc")], c(panelvar, yearvar), all.x = T)

  if (plot) {

    ## Make a plot we all like
    if (transform == "logit") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = arm::invlogit(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_neg, aes(x = arm::invlogit(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = arm::invlogit(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = arm::invlogit(get(depvar)), y = upper_frontier), color = "red"))
    }
    if (transform == "logit_trans") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = inv_logit_trans(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_neg, aes(x = inv_logit_trans(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = inv_logit_trans(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = inv_logit_trans(get(depvar)), y = upper_frontier), color = "red"))
    }

    if (transform == "log") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = exp(get(depvar)), y = yoy_pc)) +
        geom_line(data = sfa_neg, aes(x = exp(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = exp(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = exp(get(depvar)), y = upper_frontier), color = "red"))
    }

    if (transform == "custom") {
      plot(ggplot2::ggplot() + geom_point(data = sfa_neg, aes(x = custom_inv_fn(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_neg, aes(x = custom_inv_fn(get(depvar)), y = lower_frontier), color = "red") +
        geom_point(data = sfa_pos, aes(x = custom_inv_fn(get(depvar)), y = yoy_pc)) + geom_line(data = sfa_pos, aes(x = custom_inv_fn(get(depvar)), y = upper_frontier), color = "red"))
    }
  }

  return_list <- list()
  return_list[["upper_intercept"]] <- coef(sfa_model_pos)[1]
  return_list[["upper_beta"]] <- coef(sfa_model_pos)[2]
  return_list[["lower_intercept"]] <- coef(sfa_model_neg)[1]
  return_list[["lower_beta"]] <- coef(sfa_model_neg)[2]

  return(return_list)
}
