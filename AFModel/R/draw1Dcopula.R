### Copula functions
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param X PARAM_DESCRIPTION
#' @param corr_mat PARAM_DESCRIPTION
#' @param print PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname draw1Dcopula
#' @export 
draw1Dcopula <- function(X, corr_mat, print = FALSE) {
  mvdat <- t(mvrnorm(n = dim(X)[1], mu = 0 * 1:dim(X)[2], Sigma = corr_mat))
  ranks <- t(apply(mvdat, 1, rank, ties.method = "first"))
  sorted_X <- apply(X, 2, sort)
  sapply(1:dim(X)[2], function(x) sorted_X[, x][as.vector(ranks[x, ])])
}
