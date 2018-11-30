######################################################################
### Copula functions
######################################################################
## Get stats dataframe and get past correlation across time
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_stats PARAM_DESCRIPTION
#' @param metadata_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_past_corr
#' @export
get_past_corr <- function(data_stats, metadata_list) {
  input_corr <- data_stats[
    year %in% c(eval(metadata_list$end_fit + metadata_list$oos_years):metadata_list$end_FC),
    .(corrs = cor(mean, year)), "iso3"
  ]
  return(input_corr)
}
