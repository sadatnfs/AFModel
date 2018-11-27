################################################
## Forecasting
################################################
## Prep our input (X) data for draws
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param X_diff PARAM_DESCRIPTION
#' @param X_level PARAM_DESCRIPTION
#' @param tmb_data_param PARAM_DESCRIPTION, Default: data_params
#' @param d PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prepping_X_for_draw
#' @export 
prepping_X_for_draw <- function(X_diff, X_level, tmb_data_param = data_params, d) {
  if (!is.null(tmb_data_param$specifications$xvar)) {
    if ("draw" %in% names(X_diff)) {
      if (tmb_data_param$specifications$fd) {
        return(X_diff[draw == d])
      } else {
        return(X_level[draw == d])
      }
    } else {
      if (tmb_data_param$specifications$fd) {
        return(X_diff[, draw := d])
      } else {
        return(X_level[, draw := d])
      }
    }
  } else {
    return(NULL)
  }
}
