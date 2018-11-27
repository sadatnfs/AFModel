## Bring in mean forecast estimates
## Bring in mean postfile outputs
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param root_fold PARAM_DESCRIPTION
#' @param passed_models PARAM_DESCRIPTION
#' @param mc.cores PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean_estimates_compile
#' @export 
mean_estimates_compile <- function(root_fold, passed_models, mc.cores) {
  registerDoParallel(cores = mc.cores)

  ## Load the passed models stuff on the number of rows in all models passed
  system.time(data_models_passed_list <- rbindlist(foreach(model = passed_models) %dopar% {

    ## Load the output meanz data
    load(paste0(root_fold, "/means/", model, ".Rdata"))

    ## Only return mean_forecast
    return(mean_forecast[, model_number := model])
  }, use.names = T, fill = T))

  on.exit(stopImplicitCluster())
  return(data_models_passed_list)
}
