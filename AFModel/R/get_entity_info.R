## Function to query and get entity info
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param variable PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_entity_info
#' @export
get_entity_info <- function(variable = NA) {
  dbconn <- dbConnect(MySQL(),
    user = "readonly", password = "justlooking",
    dbname = "forecasting", host = "dex-modelingdb-d01.ihme.washington.edu"
  )

  if (is.na(variable)) {
    out <- dbSendQuery(dbconn, paste0("select * from entity_metadata"))
  } else {
    out <- dbSendQuery(dbconn, paste0('select * from entity_metadata where entity_name = "', variable, '"'))
  }

  out_2 <- data.table(fetch(out, n = -1))
  on.exit(dbDisconnect(dbconn))

  return(out_2)
}
