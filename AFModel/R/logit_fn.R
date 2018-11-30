#' @title Bounded logit transformation package
#' @description Converts a numeric into logit space on any real bounds
#' @param y number to convert to logit space
#' @param y_min >= 0 lower bound of domain
#' @param y_max upper bound of domain
#' @param epsilon small offset to avoid discontinuity at 0,1, Default: 1e-05
#' @return a number in logit space. antilogit_fn inverts this transformation into normal space.
#' @rdname logit_fn
#'
#' @export
logit_fn <- function(y, y_min, y_max, epsilon = 1e-5) {
  log((y - (y_min - epsilon)) / (y_max + epsilon - y))
}
