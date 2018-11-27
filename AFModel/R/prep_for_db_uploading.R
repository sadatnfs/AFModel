#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param root_fold PARAM_DESCRIPTION
#' @param metadata_list PARAM_DESCRIPTION
#' @param entity_info PARAM_DESCRIPTION
#' @param model_num PARAM_DESCRIPTION
#' @param custom_data PARAM_DESCRIPTION, Default: NULL
#' @param custom_draw_data_path PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prep_for_db_uploading
#' @export 
prep_for_db_uploading <- function(root_fold, metadata_list, entity_info, model_num, custom_data = NULL, custom_draw_data_path = NULL) {


  ## Get data from root_fold:
  ## First check whether scenario data exists
  if (is.null(custom_data)) {
    tryCatch({
      data <- fread(paste0(root_fold, "/summary_files/correlated_stats.csv"))
      try(data <- fread(paste0(root_fold, "/summary_files/scenario_stats.csv")))
    },
    error = function(e) {
      stop("Neither correlated nor scenario stats found")
    },
    warning = function(w) {
      stop("No scenarios")
    }
    )
  } else {
    data <- custom_data
  }

  ## Check whether iso3, year, mean, upper and lower are in our dataset
  if (!any(colnames(data) %in% c("iso3", "year", "mean", "upper", "lower"))) {
    stop("Not all of the following columns are in the dataframe: iso3, year, mean, upper, lower")
  }



  ## Add scenario column if scenario not in data
  if (!"scenario" %in% colnames(data)) {
    data[, scenario := 0]
  }


  ## If location_id is not in data, we merge it on
  if (!"location_id" %in% colnames(data)) {

    ## Get location_set
    dbconn <- dbConnect(MySQL(),
      user = "readonly", password = "justlooking",
      dbname = "forecasting", host = "dex-modelingdb-d01.ihme.washington.edu"
    )

    lox <- get_lox()[level == 3, .(iso3, location_id)]
    on.exit(dbDisconnect(dbconn))

    data <- merge(data, lox, "iso3", all.x = T)
  }

  ## Prep data in the format we desire
  data_oot <- data[, .(
    entity = entity_info$entity,
    model_id = model_num, location_id, year_id = year, mean, upper, lower, scenario
  )]


  ## Get location of draws (depending on whether we ran scenarios or not)
  if (is.null(custom_draw_data_path)) {
    if (file.exists(paste0(root_fold, "/summary_files/scenario_draws.feather"))) {
      draws <- paste0(root_fold, "/summary_files/scenario_draws.feather")
    } else if (file.exists(paste0(root_fold, "/summary_files/correlated_draws.feather"))) {
      draws <- paste0(root_fold, "/summary_files/correlated_draws.feather")
    } else {
      draws <- "NO DRAW FILES FOUND"
    }
  } else {
    draws <- custom_draw_data_path
  }

  ## Create a data.table with the model and entity info
  model_info <- data.table(
    model_id = model_num,
    date = metadata_list$date,
    entity = entity_info$entity,
    comment = metadata_list$comment,
    comment_long = metadata_list$comment_long,
    draws = draws
  )

  list_out <- list()
  list_out[["outputs"]] <- data_oot
  list_out[["model_info"]] <- model_info
  return(list_out)
}
