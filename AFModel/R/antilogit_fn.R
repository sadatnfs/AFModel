#' @rdname logit_fn
#'
#' @export
antilogit_fn <- function(y, y_min, y_max, epsilon = 1e-5) {
  (exp(y) * (y_max + epsilon) + y_min - epsilon) / (1 + exp(y))
}
