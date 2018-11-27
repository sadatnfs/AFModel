### Bounded logit transformer
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param y PARAM_DESCRIPTION
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
#' @rdname logit_fn
#' @export 
logit_fn <- function(y, y_min, y_max, epsilon = 1e-5) {
  log((y - (y_min - epsilon)) / (y_max + epsilon - y))
}
