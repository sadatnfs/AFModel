###################################################
## Ensemble Architecture
###################################################
## A small function to duplicate and return a data frame with switches
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param param_name PARAM_DESCRIPTION
#' @param param_domain PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname duplicate_switch
#' @export 
duplicate_switch <- function(data, param_name, param_domain) {
  if (length(param_domain) > 0 & !is.null(param_domain)) {
    tmpcol <- colnames(data)
    data <- data[rep(1:.N, length(param_domain))][, (param_name) := param_domain, by = tmpcol]
  } else {
    data[, (param_name) := 0]
  }

  return(data)
}
