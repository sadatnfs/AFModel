#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param X PARAM_DESCRIPTION
#' @param cor_mat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname draw2Dcopula
#' @export 
draw2Dcopula <- function(X, cor_mat) {
  L <- dim(X)[2]
  D <- dim(X)[3]
  Xsum <- apply(X, c(2, 3), sum)
  mvdat <- mvrnorm(n = D, mu = 0 * 1:L, Sigma = cor_mat, empirical = TRUE)
  ranks <- apply(mvdat, 2, rank, ties.method = "first")
  sortedXsim <- apply(Xsum, 1, function(x) sort(x, index.return = TRUE)$ix)
  sortedX <- Xtad
  for (i in 1:L) {
    sortedX[, i, ] <- Xtad[, i, sortedXsim[, i]]
  }
  Xcorr <- sortedX
  for (i in 1:L) {
    Xcorr[, i, ] <- sortedX[, i, ranks[, i]]
  }
  Xcorr
}
