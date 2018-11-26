## Prep draw data
prep_array_for_copula <- function(data_draws, metadata_list) {
  draws_melt <- melt(data_draws[year >= metadata_list$end_fit],
    id.vars = c("iso3", "year"), value.name = "data", variable.name = "draw"
  )
  draws_array <- reshape2::acast(draws_melt, iso3 ~ draw ~ year, value.var = "data")
  return(draws_array)
}
