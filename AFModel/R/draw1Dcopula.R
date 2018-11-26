### Copula functions
draw1Dcopula <- function(X, corr_mat, print = FALSE) {
  mvdat <- t(mvrnorm(n = dim(X)[1], mu = 0 * 1:dim(X)[2], Sigma = corr_mat))
  ranks <- t(apply(mvdat, 1, rank, ties.method = "first"))
  sorted_X <- apply(X, 2, sort)
  sapply(1:dim(X)[2], function(x) sorted_X[, x][as.vector(ranks[x, ])])
}
