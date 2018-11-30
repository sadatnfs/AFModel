## Inv Lemon squeeze logit transformer
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname inv_logit_trans
#' @export
inv_logit_trans <- function(x) {
  return(((exp(x) / (1 + exp(x))) - (0.5 / 1000)) * (1000 / 999))
}
