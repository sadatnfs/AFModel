### Run all copula sequence
correlate_draws_1D <- function(draws = chaos_draws, stats = chaos_stats, metadata_list = metadata_list, AR1 = T) {
  print("Get correlations")
  input_corr <- get_past_corr(stats, metadata_list)

  print("Prepping draws")
  draws_array <- prep_array_for_copula(draws, metadata_list)

  print("Run copula by country")
  copula_draws <- rbindlist(foreach(iso = dimnames(draws_array)[[1]]) %do%
    copula_by_country(draws_array = draws_array, iso = iso, input_corr = input_corr, metadata_list, AR1 = AR1))

  print("Finalize draws")
  cop_draws <- finalize_copula_draws(draws, copula_draws, metadata_list)
  cop_stats <- stat_maker(data = cop_draws, melt = T, merge = F, idvar = c("iso3", "year"))

  return(list(draws = cop_draws, stats = cop_stats))
}
