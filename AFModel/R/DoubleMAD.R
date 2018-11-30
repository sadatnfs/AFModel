#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param zero.mad.action PARAM_DESCRIPTION, Default: 'warn'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname DoubleMAD
#' @export
DoubleMAD <- function(x, zero.mad.action = "warn") {
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  x <- x[!is.na(x)]
  m <- median(x)
  abs.dev <- abs(x - m)
  left.mad <- median(abs.dev[x <= m])
  right.mad <- median(abs.dev[x >= m])
  if (left.mad == 0 || right.mad == 0) {
    if (zero.mad.action == "stop") stop("MAD is 0")
    if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
    if (zero.mad.action %in% c("na", "warn and na")) {
      if (left.mad == 0) left.mad <- NA
      if (right.mad == 0) right.mad <- NA
    }
  }
  return(c(left.mad, right.mad))
}
