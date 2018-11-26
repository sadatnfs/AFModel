### Bounded logit transformer
logit_fn <- function(y, y_min, y_max, epsilon = 1e-5) {
  log((y - (y_min - epsilon)) / (y_max + epsilon - y))
}
