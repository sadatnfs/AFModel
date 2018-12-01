### Create fun summary stuff
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tmb_output_obj PARAM_DESCRIPTION
#' @param tmb_data_param PARAM_DESCRIPTION
#' @param model_number PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname postfile_maker
#' @export
postfile_maker <- function(tmb_output_obj, tmb_data_param, model_number) {

  # tmb_output_obj = copy(output_TMB)
  # tmb_data_param = copy(data_params)


  ## Model number
  postfile_df <- data.table(model_number = model_number)


  ## Extract fixed effects from the model
  postfile_df <- data.table(model_number = model_number)

  ## Add AR term
  postfile_df[, ar := tmb_data_param$data$ar]

  ## AR mod
  postfile_df[, ar_mod := tmb_data_param$specifications$ar_mod]

  ## Add MA term
  postfile_df[, ma := tmb_data_param$data$ma]

  ## MA mod
  postfile_df[, ma_mod := tmb_data_param$specifications$ma_mod]

  ## FD?
  postfile_df[, fdiff := tmb_data_param$specifications$fd]

  ## AR constrain?
  postfile_df[, ar_constrain := tmb_data_param$specifications$ar_constrain]

  ## RE int decay?
  postfile_df[, RE_int_decay := tmb_data_param$specifications$RE_int_decay * 1]

  ## Convergence term
  if (tmb_data_param$specifications$c) {
    postfile_df[, conv := tmb_output_obj$obj$report()$c]
  }

  ## Scaled convergence term
  if (tmb_data_param$specifications$scaled_c) {
    postfile_df[, scaled_c := tmb_output_obj$obj$report()$scaled_c]
  }

  ## Upweights
  postfile_df[, weights := tmb_data_param$data$weight_decay ]

  ## Fixed effects?
  if (!is.null(tmb_data_param$specifications$xvar)) {
    postfile_df[, paste0("FE_", tmb_data_param$specifications$xvar) := lapply(c(1:length(tmb_data_param$specifications$xvar)), function(x) tmb_output_obj$obj$report()$b[x]) ]
  }

  ## Random coefs?
  if (!is.null(tmb_data_param$specifications$re_vars)) {
    postfile_df[, paste0("RE_", (tmb_data_param$specifications$re_vars)) := T]
  }

  ## Intercepts

  ## Global int?
  postfile_df[, global_int := tmb_data_param$specifications$a]

  ## Country ints and random or not?
  postfile_df[, country_int := tmb_data_param$specifications$z]
  postfile_df[, country_int_dist := tmb_data_param$specifications$z_ran]


  ## Yvar:
  postfile_df[, yvar := tmb_data_param$specifications$yvar]



  return(postfile_df)
}
