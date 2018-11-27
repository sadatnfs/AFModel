#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param antiy PARAM_DESCRIPTION
#' @param y_min PARAM_DESCRIPTION
#' @param y_max PARAM_DESCRIPTION
#' @param epsilon PARAM_DESCRIPTION, Default: 1e-05
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname antilogit_fn
#' @export 
antilogit_fn <- function(antiy, y_min, y_max, epsilon = 1e-5) {
  (exp(antiy) * (y_max + epsilon) + y_min - epsilon) / (1 + exp(antiy))
}
