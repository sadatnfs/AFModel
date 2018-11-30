## Take draws object, cast wide on draws, and tag model number
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param draws PARAM_DESCRIPTION
#' @param model PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prep_draw_output
#' @export
prep_draw_output <- function(draws, model) {
  return(dcast(draws, iso3 + year ~ draw, value.var = c("yvar", "ydiff"))[, model := model])
}
