#################################################
## Modeling functions
#################################################
# ## Run a simple linear model to initialize values
# run_lm_model <- function(data_params) {
#
#   ## Get both level and diff Ys
#   data_use <- data.table(melt(data_params$data$Y_diff))
#   colnames(data_use) <- c('iso3', 'year', 'yvar')
#
#   data_use_levels <- data.table(melt(data_params$data$Y_input))
#   colnames(data_use_levels) <- c('iso3', 'year', 'ylevel')
#
#   ## Check if FD
#   if(data_params$specifications$fd) {
#     if(!is.null(data_params$specifications$xvar)) {
#       x_use <- data.table(melt(data_params$data$x_diff))
#       colnames(x_use) <- c('covars', 'iso3', 'year', 'x')
#       x_use <- dcast(x_use, iso3 + year ~ covars, value.var = 'x')
#       data_use <- merge(data_use, x_use, c('iso3','year'))
#     }
#   } else {
#     if(!is.null(data_params$specifications$xvar)) {
#       x_use <- data.table(melt(data_params$data$x_input))
#       colnames(x_use) <- c('covars', 'iso3', 'year', 'x')
#       x_use <- dcast(x_use, iso3 + year ~ covars, value.var = 'x')
#       data_use_levels <- merge(data_use, x_use, c('iso3','year'))
#     }
#
#   }
#
#   data_use <- merge(data_use, data_use_levels, c('iso3', 'year'))
#   setkeyv(data_use, c('iso3', 'year'))
#
#
#   ## Conv term?
#   if(data_params$specifications$c) {
#     data_use[, convergence_term:= shift(ylevel), by = 'iso3']
#     conv_form <- 'convergence_term'
#   } else {
#     conv_form <- NULL
#   }
#
#   ## Global intercept?
#   if(data_params$specifications$a) {
#     data_use[, intercept:= 1]
#     inter_form <- 'intercept'
#   } else {
#     inter_form <- NULL
#   }
#
#   ## Prepping 'random' effects as static inputs
#   if(data_params$specifications$z) {
#     re_term <- 'factor(iso3)'
#   } else {
#     re_term <- NULL
#   }
#
#   ## Random coefs
#
#
#   ## Only have a single global AR term as initial values
#   if(data_params$specifications$rho > 0) {
#     if(data_params$specifications$fd) {
#       data_use[, paste0('ar_', c(1:data_params$specifications$rho)):= lapply(c(1:data_params$specifications$rho), function(var) shift(yvar, n = var) ), by = 'iso3']
#     } else {
#       data_use[, paste0('ar_', c(1:data_params$specifications$rho)):= lapply(c(1:data_params$specifications$rho), function(var) shift(ylevel, n = var) ), by = 'iso3']
#     }
#     ar_form <- paste0('ar_', data_params$specifications$rho)
#   } else {
#     ar_form <- NULL
#   }
#
#
#   ## Fit model
#   lm_form <- reformulate(termlabels = c(data_params$specifications$xvar, conv_form, ar_form, inter_form, re_term),
#                          response = ifelse(data_params$specifications$fd, 'yvar', 'ylevel'),
#                          intercept = F)
#
#
#   model <- lm(data = data_use, formula = lm_form)
#
#
#
# }
## Run TMB model
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_params PARAM_DESCRIPTION
#' @param model PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[Matrix]{solve-methods}}
#'  \code{\link[testthat]{equality-expectations}}
#' @rdname run_model
#' @export 
#' @importFrom Matrix solve
#' @importFrom testthat expect_identical
run_model <- function(data_params, model, verbose = F) {

  # arguments <- list(...)

  ### Our output list
  output_list <- list()

  ## Create the ADFun
  random_vec <- NULL
  re_input <- na.omit(c(
    ifelse(data_params$data$country_int > 0 & data_params$data$country_int_dist %in% c(0, 1), "z", NA),
    ifelse(data_params$data$country_int > 0 & data_params$data$country_int_dist == 2, "z_ar1", NA),
    ifelse(data_params$data$re_coef > 0, "z_coef", NA),
    # ifelse(data_params$data$ar>0 & data_params$data$ar_mod %in% c(1,3) , 'rho_global', NA),
    ifelse(data_params$data$ar > 0 & data_params$data$ar_mod %in% c(2, 3), "rho_country", NA),
    # ifelse(data_params$data$ma>0 & data_params$data$ma_mod %in% c(1,3) , 'theta_global', NA),
    ifelse(data_params$data$ma > 0 & data_params$data$ma_mod %in% c(2, 3), "theta_country", NA)
  ))
  if (length(re_input) > 0) {
    random_vec <- re_input
  }

  ## Finally, find the NAs in data_params$parameters, and MAP them out
  map_var <- names(data_params$parameters)[is.na(data_params$parameters)]
  map_var_list <- lapply(map_var, function(x) return(factor(NA)))
  names(map_var_list) <- map_var



  #### Create the objective function as a member of output_list
  output_list[["obj"]] <- MakeADFun(data_params$data,
    data_params$parameters,
    hessian = T,
    DLL = model,
    silent = !verbose,
    random = random_vec,
    checkParameterOrder = T,
    last.par = T
  )

  runSymbolicAnalysis(output_list[["obj"]])

  ## Optimize (put this in a try block)
  output_list[["opt"]] <- nlminb(
    objective = output_list[["obj"]]$fn,
    gradient = output_list[["obj"]]$gr,
    start = output_list[["obj"]]$par
  )



  ### The parameter vector in order of the joint precision can be extracted from obj:
  output_list[["mean_params"]] <- output_list[["obj"]]$env$last.par.best


  ## Get the SEs of the estimates
  output_list[["FC_model"]] <- sdreport(output_list[["obj"]], getJointPrecision = T, getReportCovariance = T, bias.correct = F)




  ### Get the variance-covariance matrix by inverting the joint Precision
  vcov_mat <- Matrix::solve(output_list[["FC_model"]]$jointPrecision)
  rownames(vcov_mat) <- colnames(vcov_mat) <- colnames(output_list[["FC_model"]]$jointPrecision)

  ## Assert that the name of mean_params and vcov_mat and identical (dimension matching test)
  testthat::expect_identical(colnames(vcov_mat), names(output_list[["mean_params"]]))

  ## Finally, record the innards of the object : Y_hat and resid
  output_list[["Y_hat"]] <- output_list[["obj"]]$report()$Y_hat
  output_list[["resid"]] <- output_list[["obj"]]$report()$resid

  # conv <<- 1
  return(output_list)
}
