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
#' @export
#' @rdname run_model
#' @useDynLib AFModel
#' @seealso
#'
#' @import Matrix
#' @import testthat
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
