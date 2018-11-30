######################################################################
### Ensemble Collectors
######################################################################
## Checking whether all mean models have finished running
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param root_fold PARAM_DESCRIPTION
#' @param mean_array_grid PARAM_DESCRIPTION
#' @param return_fail PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean_run_check
#' @export
mean_run_check <- function(root_fold, mean_array_grid, return_fail = F) {

  ## Get a list of models that passed and failed
  list_of_models_passed <- data.table(model_number = as.numeric(strsplit(list.files(paste0(root_fold, "/passed")), ".txt")))
  list_of_models_failed <- data.table(model_number = as.numeric(strsplit(list.files(paste0(root_fold, "/failed")), ".txt")))

  print(paste0(
    nrow(list_of_models_passed) + nrow(list_of_models_failed),
    " models out of ", nrow(mean_array_grid),
    " have finished running. Remaining: ",
    nrow(mean_array_grid) - (nrow(list_of_models_passed) + nrow(list_of_models_failed))
  ))
  print(paste0("with ", nrow(list_of_models_passed), " passed and ", nrow(list_of_models_failed), " failed"))

  ## Make sure that we have fail and passes for ALL the models we're supposed to run
  if (eval(nrow(list_of_models_passed) + nrow(list_of_models_failed)) != nrow(mean_array_grid)) {
    # print("All of the models did not finish running. Check the following models!")

    print("Models missing from run are:")
    merg <- merge(rbind(list_of_models_passed, list_of_models_failed)[order(model_number), done := T],
      data.table(model_number = mean_array_grid$id),
      "model_number",
      all.y = T
    )[is.na(done)]
    print(merg)

    if (return_fail) {
      return(merg[, model_number])
    } else {
      stop("DIE DIE DIE DIE DIE. Seriously.")
    }
  } else {
    print("All models have finished running")
  }

  return(list_of_models_passed[, model_number])
}
