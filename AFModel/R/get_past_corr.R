######################################################################
### Copula functions
######################################################################
## Get stats dataframe and get past correlation across time
get_past_corr <- function(data_stats, metadata_list) {
  input_corr <- data_stats[
    year %in% c(eval(metadata_list$end_fit + metadata_list$oos_years):metadata_list$end_FC),
    .(corrs = cor(mean, year)), "iso3"
  ]
  return(input_corr)
}
