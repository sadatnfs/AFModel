### This function will: multiply or divvy by a column from raw data with yvar,
### and regenerate ydiff (helpful for changing denominators)
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
