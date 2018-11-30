## Prep draw data
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_draws PARAM_DESCRIPTION
#' @param metadata_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[reshape2]{cast}}
#' @rdname prep_array_for_copula
#' @export
#' @importFrom reshape2 acast
prep_array_for_copula <- function(data_draws, metadata_list) {
  draws_melt <- melt(data_draws[year >= metadata_list$end_fit],
    id.vars = c("iso3", "year"), value.name = "data", variable.name = "draw"
  )
  draws_array <- reshape2::acast(draws_melt, iso3 ~ draw ~ year, value.var = "data")
  return(draws_array)
}
