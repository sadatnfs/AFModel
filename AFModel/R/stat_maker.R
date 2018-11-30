### Stat making function across draws
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param melt PARAM_DESCRIPTION, Default: F
#' @param merge PARAM_DESCRIPTION, Default: T
#' @param idvar PARAM_DESCRIPTION, Default: c("iso3", "year")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname stat_maker
#' @export
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
