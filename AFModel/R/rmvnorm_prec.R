#' Take multivariate normal draws given a mean vector and precision matrix
#'
#' @param mu vector of parameter means
#' @param prec joint precision matrix
#' @param n.sims number of draws
#'
#' @return length(mu) by n.sims matrix of parameter draws
#'
#' @seealso
#'  \code{\link[MASS]{mvrnorm}}
#'  \code{\link[Matrix]{Cholesky}},\code{\link[Matrix]{solve-methods}}
#' @rdname rmvnorm_prec
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom Matrix Cholesky solve
rmvnorm_prec <- function(mu = NULL, prec, n.sims) {
  if (is.null(mu)) {
    mu <- rep(0, dim(prec)[1])
  }
  z <- matrix(MASS::mvrnorm(length(mu) * n.sims, 0, 1), ncol = n.sims)
  L <- Matrix::Cholesky(prec, super = TRUE)
  z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  outmat <- t(mu + z)
  colnames(outmat) <- colnames(prec)
  return(outmat)
}
