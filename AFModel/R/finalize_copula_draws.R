## Take copulated output for all countries, bind to past and make stats
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_draws PARAM_DESCRIPTION
#' @param copula_draws PARAM_DESCRIPTION
#' @param metadata_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname finalize_copula_draws
#' @export 
finalize_copula_draws <- function(data_draws, copula_draws, metadata_list) {

  ## Keep only observed past
  data_draws <- data_draws[year < metadata_list$end_fit]

  ## Bind with copula draws
  data_draws <- rbindlist(list(data_draws, copula_draws), use.names = T)
  setkeyv(data_draws, c("iso3", "year"))

  return(data_draws)
}
