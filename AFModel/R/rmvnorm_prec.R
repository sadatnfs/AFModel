#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mu PARAM_DESCRIPTION, Default: NULL
#' @param prec PARAM_DESCRIPTION
#' @param n.sims PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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
