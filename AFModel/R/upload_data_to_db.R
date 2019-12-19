### Uploader
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param upload_prepz PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname upload_data_to_db
#' @export
upload_data_to_db <- function(upload_prepz) {


  ## Open connection
  dbconn <- dbConnect(MySQL(),
    user = "XXX", password = "XXX",
    dbname = "XXX", host = "XXX"
  )


  ## Make sure that we are good on the model numbering
  model_info <- get_model_info(entity_info$entity)
  if (nrow(model_info) == 0) {
    model_num <- 1
  } else {
    model_num <- max(model_info$model_id) + 1
  }

  if (model_num > upload_prepz$model_info$model_id) {
    print(paste0("Model ID ", model_num, " have already been uploaded! Increment by 1"))
    upload_prepz$model_info$model_id <- upload_prepz$model_info$model_id + 1
    upload_prepz$outputs$model_id <- upload_prepz$outputs$model_id + 1
  }

  ## Upload model_info first
  print("Uploading model info")
  dbWriteTable(dbconn,
    value = upload_prepz$model_info,
    row.names = F,
    name = "model_versions", append = TRUE
  )

  ## Upload data
  print("Uploading data to outputs table")

  ##  Save out tempfile of data
  tmpfile <- fwrite(
    upload_prepz$outputs[, .(entity, model_id, location_id, year_id, mean, upper, lower, scenario)],
    paste0(
      "/share/resource_tracking/temp_dump/",
      upload_prepz$model_info$comment, "_output_rt1.csv"
    )
  )

  ## Build query
  query <- paste0(
    "load data infile ",
    " '/share/resource_tracking/temp_dump/", upload_prepz$model_info$comment, "_output_rt1.csv'",
    " into table forecasting.outputs ",
    "fields terminated by ',' ",
    "lines terminated by '\n' ignore 1 lines;"
  )

  # ## Save out tempfile of data csv to infile up
  # tmpfile <- fwrite(upload_prepz$outputs[,.(entity, model_id, location_id, year_id, mean, upper, lower, scenario)],
  #                   paste0('/ihme/code/resource_tracking/temp_dump/',
  #                          upload_prepz$model_info$comment, '_output_rt1.csv'))
  #
  # print("Build query")
  # query  <- paste0("load data infile ",
  #                  " '/ihme/code/resource_tracking/temp_dump/", upload_prepz$model_info$comment, "_output_rt1.csv'",
  #                  " into table forecasting.outputs ",
  #                  "fields terminated by ',' ",
  #                  "lines terminated by '\n' ignore 1 lines;")



  print("Build query")
  print(query)


  # Submit the update query and dc
  dbGetQuery(dbconn, query)


  ### Use write table for now:
  # dbWriteTable(dbconn,
  #              value = upload_prepz$outputs[,.(entity, model_id, location_id, year_id, mean, upper, lower, scenario)],
  #              row.names=F,
  #              name = "outputs", append = TRUE )

  on.exit(dbDisconnect(dbconn))

  print("Upload completed")
  return(0)
}
