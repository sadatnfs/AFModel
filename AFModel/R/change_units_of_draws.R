#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param draws PARAM_DESCRIPTION
#' @param idvar PARAM_DESCRIPTION
#' @param input_data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION
#' @param op PARAM_DESCRIPTION, Default: c("add", "subtract", "multiply", "divide")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname change_units_of_draws
#' @export 
change_units_of_draws <- function(draws, idvar, input_data, column, op = c("add", "subtract", "multiply", "divide")) {
  if (length(op) != 1) {
    stop("Choose one operation")
  }
  if (!(op %in% c("add", "subtract", "multiply", "divide"))) {
    stop("Choose one op of c('add', 'subtract', 'multiply', 'divide')")
  }

  ## Merge draws with input_data
  draws <- merge(draws, input_data[, .SD, .SDcols = c(idvar, column)], idvar, all.x = T)

  ## Apply the op
  if (op == "add") {
    draws[, yvar := yvar + get(paste0(column))]
  }
  if (op == "subtract") {
    draws[, yvar := yvar - get(paste0(column))]
  }
  if (op == "multiply") {
    draws[, yvar := yvar * get(paste0(column))]
  }
  if (op == "divide") {
    draws[, yvar := yvar / get(paste0(column))]
  }

  ## Remake ydiff
  draws[, ydiff := yvar - shift(yvar), by = c(idvar[1], "draw") ]
  draws[, (column) := NULL]

  return(draws)
}
