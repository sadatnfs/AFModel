% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_data_param.R
\name{make_data_param}
\alias{make_data_param}
\title{FUNCTION_TITLE}
\usage{
make_data_param(input_data, yvar, xvar, REs, global_int = T,
  country_int = T, country_int_dist = 1, re_vars = NULL,
  ar_mod = 2, ar = 0, ma_mod = 2, ma = 0, start_year = 1970,
  end_fit = 2017, end_FC = 2040, weight_decay = 0, chaos = T,
  fd = F, conv = F, scaled_lev_conv = F, ar_constrain = F,
  int_decay = F)
}
\arguments{
\item{input_data}{PARAM_DESCRIPTION}

\item{yvar}{PARAM_DESCRIPTION}

\item{xvar}{PARAM_DESCRIPTION}

\item{REs}{PARAM_DESCRIPTION}

\item{global_int}{PARAM_DESCRIPTION, Default: T}

\item{country_int}{PARAM_DESCRIPTION, Default: T}

\item{country_int_dist}{PARAM_DESCRIPTION, Default: 1}

\item{re_vars}{PARAM_DESCRIPTION, Default: NULL}

\item{ar_mod}{PARAM_DESCRIPTION, Default: 2}

\item{ar}{PARAM_DESCRIPTION, Default: 0}

\item{ma_mod}{PARAM_DESCRIPTION, Default: 2}

\item{ma}{PARAM_DESCRIPTION, Default: 0}

\item{start_year}{PARAM_DESCRIPTION, Default: 1970}

\item{end_fit}{PARAM_DESCRIPTION, Default: 2017}

\item{end_FC}{PARAM_DESCRIPTION, Default: 2040}

\item{weight_decay}{PARAM_DESCRIPTION, Default: 0}

\item{chaos}{PARAM_DESCRIPTION, Default: T}

\item{fd}{PARAM_DESCRIPTION, Default: F}

\item{conv}{PARAM_DESCRIPTION, Default: F}

\item{scaled_lev_conv}{PARAM_DESCRIPTION, Default: F}

\item{ar_constrain}{PARAM_DESCRIPTION, Default: F}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[reshape2]{cast}}
 \code{\link[data.table]{melt.data.table}}
}
