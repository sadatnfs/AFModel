## Locs
get_lox <- function() {
  dbconn <- dbConnect(MySQL(),
    user = "readonly", password = "justlooking",
    dbname = "forecasting", host = "dex-modelingdb-d01.ihme.washington.edu"
  )

  out <- dbSendQuery(dbconn, paste0("select * from location_metadata"))
  out_2 <- data.table(fetch(out, n = -1))
  on.exit(dbDisconnect(dbconn))

  return(out_2)
}
