## Checking whether all the Chaos collectors have finished
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param root_fold PARAM_DESCRIPTION
#' @param oos_years PARAM_DESCRIPTION
#' @param chaos PARAM_DESCRIPTION
#' @param scenario PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname chaos_collector_check
#' @export
chaos_collector_check <- function(root_fold, oos_years, chaos, scenario = F) {


  ## Get list of files
  if (!scenario) {
    list_of_chaos <- list.files(paste0(root_fold, "/summary_files"), pattern = ".rds")
  } else if (scenario) {
    list_of_chaos <- list.files(paste0(root_fold, "/summary_files_scenarios"), pattern = ".rds")
    oos_years <- oos_years * 2
  }

  ## Keep only Chaos rds
  list_of_chaos <- list_of_chaos[grep("chaos", list_of_chaos)]

  if (chaos) {
    if (length(list_of_chaos) == oos_years) {
      print("All Chaos collectors have finished running")
    } else {
      stop(paste0(eval(oos_years - length(list_of_chaos)), " out of ", oos_years, " have not finished."))
    }
  } else {
    if (length(list_of_chaos) == 1) {
      print("Draw collector have finished running")
    } else {
      stop("Nope")
    }
  }
}
