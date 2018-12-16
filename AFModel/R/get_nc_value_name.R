#' @title Get data information from xarray file
#' @description Get dimension information from xarray file
#' @param nc_file filepath of the NetCDF file
#' @return a vector of variable names
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_nc_value_name
#' @export
get_nc_value_name <- function(nc_file) {

  ## Get names
  nc_obj <- nc_open(nc_file)
  name <- names(nc_obj$var)

  ## Close file
  nc_close(nc_obj)

  ## Return the name
  return(name)
}
