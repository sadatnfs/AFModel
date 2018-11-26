antilogit_fn <- function(antiy, y_min, y_max, epsilon = 1e-5) {
  (exp(antiy) * (y_max + epsilon) + y_min - epsilon) / (1 + exp(antiy))
}
