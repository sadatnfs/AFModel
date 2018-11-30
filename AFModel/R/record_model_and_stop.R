## A simple function to record a fail/pass and stop running code
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param model PARAM_DESCRIPTION
#' @param variable PARAM_DESCRIPTION
#' @param comment PARAM_DESCRIPTION
#' @param date PARAM_DESCRIPTION, Default: gsub("-", "", Sys.Date())
#' @param type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname record_model_and_stop
#' @export
record_model_and_stop <- function(model, variable, comment, date = gsub("-", "", Sys.Date()), type) {
  if (!(type %in% c("passed", "failed"))) {
    stop("Really?")
  }

  ## Just echo a blank file in failed folder
  folder_path <- "/share/resource_tracking/forecasting/"
  paste0(folder_path, variable, "/", date, "_", comment, "/", type)
  system(paste0("echo ", model, " >> ", folder_path, variable, "/", date, "_", comment, "/", type, "/", model, ".txt"))

  if (type == "passed") {
    print(paste0("Model run sucessfull! Saved out empty file in : ", folder_path, variable, "/", date, "_", comment, "/", type, "/", model, ".txt"))
    q("no")
  } else if (type == "failed") {
    stop(paste0("Model run failed. Saved out empty file in : ", folder_path, variable, "/", date, "_", comment, "/", type, "/", model, ".txt"))
  }
}
