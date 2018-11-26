## Function to query and get output info for given entity and model
get_oots <- function(entity_id, model_id, scenario = 0, year_id = c(1980:2050), location_id = -1) {
  dbconn <- dbConnect(MySQL(),
    user = "readonly", password = "justlooking",
    dbname = "forecasting", host = "dex-modelingdb-d01.ihme.washington.edu"
  )

  if(location_id == -1)  {
    out <- dbSendQuery(dbconn, paste0(
      'select * from outputs where entity = "', entity_id, '"',
      " and model_id = ", model_id,
      " and year_id IN (", paste0(year_id, collapse = ","), ")",
      " and scenario IN (", paste0(scenario, collapse = ","), ")"
    ))
  } else {
    out <- dbSendQuery(dbconn, paste0(
      'select * from outputs where entity = "', entity_id, '"',
      " and model_id = ", model_id,
      " and location_id IN (", paste0(location_id, collapse = ","), ")",
      " and year_id IN (", paste0(year_id, collapse = ","), ")",
      " and scenario IN (", paste0(scenario, collapse = ","), ")"
    ))
  }

  out_2 <- data.table(fetch(out, n = -1))
  on.exit(dbDisconnect(dbconn))

  return(out_2)
}
