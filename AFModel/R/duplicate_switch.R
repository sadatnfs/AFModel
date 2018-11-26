###################################################
## Ensemble Architecture
###################################################
## A small function to duplicate and return a data frame with switches
duplicate_switch <- function(data, param_name, param_domain) {
  if (length(param_domain) > 0 & !is.null(param_domain)) {
    tmpcol <- colnames(data)
    data <- data[rep(1:.N, length(param_domain))][, (param_name) := param_domain, by = tmpcol]
  } else {
    data[, (param_name) := 0]
  }

  return(data)
}
