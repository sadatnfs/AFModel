################################################
## Forecasting
################################################
## Prep our input (X) data for draws
prepping_X_for_draw <- function(X_diff, X_level, tmb_data_param = data_params, d) {
  if (!is.null(tmb_data_param$specifications$xvar)) {
    if ("draw" %in% names(X_diff)) {
      if (tmb_data_param$specifications$fd) {
        return(X_diff[draw == d])
      } else {
        return(X_level[draw == d])
      }
    } else {
      if (tmb_data_param$specifications$fd) {
        return(X_diff[, draw := d])
      } else {
        return(X_level[, draw := d])
      }
    }
  } else {
    return(NULL)
  }
}
