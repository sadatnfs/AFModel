## Creating bunch of stuff with the residuals (MAD, median, mean, SD)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param panelvar PARAM_DESCRIPTION
#' @param variable PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_stats
#' @export
create_stats <- function(data, panelvar, variable) {
  data_out <- copy(data)

  data_out <- data_out[!is.na(get(variable))]

  ## Median
  data_out[, median := median(get(variable), na.rm = T), panelvar ]

  ## MAD
  data_out[, mad := mad(get(variable), na.rm = T), panelvar ]

  ## Mean
  data_out[, mean := mean(get(variable), na.rm = T), panelvar ]

  ## SD
  data_out[, sd := sd(get(variable), na.rm = T), panelvar ]

  data_out <- data_out[, .SD, .SDcols = c(panelvar, "mean", "sd", "median", "mad")]

  return(unique(data_out, by = panelvar))
}
