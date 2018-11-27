## Function to query and get model info
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param entity_id PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_model_info
#' @export 
get_model_info <- function(entity_id = NA) {
  dbconn <- dbConnect(MySQL(),
    user = "readonly", password = "justlooking",
    dbname = "forecasting", host = "dex-modelingdb-d01.ihme.washington.edu"
  )

  if (is.na(entity_id)) {
    out <- dbSendQuery(dbconn, paste0("select * from model_versions "))
  } else {
    out <- dbSendQuery(dbconn, paste0('select * from model_versions where entity = "', entity_id, '"'))
  }
  out_2 <- data.table(fetch(out, n = -1))
  on.exit(dbDisconnect(dbconn))

  return(out_2)
}
