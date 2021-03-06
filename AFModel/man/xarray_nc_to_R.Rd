% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xarray_nc_to_R.R
\name{xarray_nc_to_R}
\alias{xarray_nc_to_R}
\title{Get data from Xarray NetCDF file}
\usage{
xarray_nc_to_R(nc_file, dimname = NULL, start = NA, count = NA,
  dt_return = T)
}
\arguments{
\item{nc_file}{Path to Xarray NetCDF file}

\item{dimname}{Get the specified data column from the xarray. If set to \code{NULL}, then the names are grabbed from the xarray directly in this function}

\item{start}{Start point of indices, Default: NA}

\item{count}{Number of indices, Default: NA}

\item{dt_return}{returns a long \code{data.table} instead of a multidimensional array, Default: T}
}
\value{
array or data.table
}
\description{
Get data from Xarray NetCDF file
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[ncdf4.helpers]{nc.get.dim.names}}
}
