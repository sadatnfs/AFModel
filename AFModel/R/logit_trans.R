######################################################################
### Math functions
######################################################################
## Lemon squeeze logit transformer
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
#' @rdname logit_trans
#' @export
logit_trans <- function(x) {
  return(log(((x * 999 / 1000) + (0.5 / 1000)) / (1 - ((x * 999 / 1000) + (0.5 / 1000)))))
}
