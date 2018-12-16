#' @title Get data from Xarray NetCDF file
#' @description Get data from Xarray NetCDF file
#' @param nc_file Path to Xarray NetCDF file
#' @param dimname Get the specified data column from the xarray. If set to \code{NULL}, then the names are grabbed from the xarray directly in this function
#' @param start Start point of indices, Default: NA
#' @param count Number of indices, Default: NA
#' @param df_return returns a long \code{data.table} instead of a multidimensional array, Default: T
#' @return array or data.table
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ncdf4.helpers]{nc.get.dim.names}}
#' @rdname xarray_nc_to_R
#' @export
#' @importFrom ncdf4.helpers nc.get.dim.names
xarray_nc_to_R <- function(nc_file, dimname = NULL, start = NA, count = NA, df_return = T) {

  ## Open the file and show the attribuets
  ncin <- nc_open(nc_file)
  print(ncin)

  ## Get the full array, using the variable name we want
  if(is.null(dimname)) {
    name <- names(nc_obj$var)
  }

  Rarray <- ncvar_get(ncin, dimname, start = start, count = count, collapse_degen = F)

  ## Get the fillvalue info
  fillvalue <- ncatt_get(ncin, dimname, "_FillValue")

  ## Get the dimension names in the right order
  array_dim <- ncdf4.helpers::nc.get.dim.names(ncin, dimname)


  ## Close the file
  nc_close(ncin)

  ## Get all of the dimension information in the order specified
  array_dim_list <- list()
  for (i in array_dim) {
    array_dim_list[[i]] <- ncin$dim[[i]]$vals
  }

  ## Fill in NaNs with NA
  Rarray[Rarray == fillvalue$value] <- NA


  ## Assign the dimension labels to the R array
  for (i in 1:length(array_dim_list)) {
    dimnames(Rarray)[[i]] <- array_dim_list[[i]]
  }

  ## Attach the dimension name to the array
  names(attributes(Rarray)$dimnames) <- array_dim

  if (dt_return) {
    return(data.table(melt(Rarray)))
  } else {
    return(Rarray)
  }
}
