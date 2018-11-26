### Stat making function across draws
stat_maker <- function(data, melt = F, merge = T, idvar = c("iso3", "year")) {

  ## Melt
  if (melt == T) {
    ## Melt super long
    print("Melting data")
    data2 <- melt(data, idvar, value.name = "data", variable.name = "draws")
  } else {
    data2 <- data
  }



  ## Get stats
  print("Make stats")
  data_stats <- data2[, as.list(c(
    mean(data),
    quantile(data, c(0.025, 0.975))
  )), by = idvar]

  colnames(data_stats) <- c(idvar, "mean", "lower", "upper")
  setkeyv(data_stats, idvar)

  if (merge) {
    ## Merge with data
    print("Merge with data")
    outdata <- merge(data_stats, data, idvar)
    setkeyv(outdata, idvar)
    return(outdata)
  }

  else {
    return(data_stats)
  }
}
