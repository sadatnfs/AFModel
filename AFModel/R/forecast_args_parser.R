################################################
## Prepping functions
################################################
## A nice function to argparse with variable, date and comment
forecast_args_parser <- function() {

  ## Open the parser
  parser <- ArgumentParser()

  ## Specify version and description
  parser$add_argument("--variable", required = T, type = "character", help = "Variable")
  parser$add_argument("--date", required = T, type = "integer", help = "Date")
  parser$add_argument("--comment", required = T, type = "character", help = "Comment")

  args <- parser$parse_args()
  print(args)

  return(list(variable = args$variable, date = args$date, comment = args$comment))
}
