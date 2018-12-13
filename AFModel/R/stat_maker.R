### Stat making function across draws
#' @title Stat maker
#' @description Create row-wise uncertainty intervals from a set of simulated columns of data
#' @param data a \code{data.table} with draws
#' @param melt melt the data long if the draws are wide , Default: F
#' @param merge merge the statistics with the draws dataframe?, Default: T
#' @param idvar unique identifier, Default: c("iso3", "year")
#' @return a dataset with the unique identifier, mean and 2.5th and 97.5th quantiles, and optionally, the set of draws merged on if \code{merge} is \code{TRUE}
#' @details If \code{data} has already been melted long, then the column with the data and draw names \code{MUST} be respectively named \code{data} and \code{draws}
#' @examples
#' \dontrun{
#'  # Simulate data IDs
#' sim_data <- data.table(expand.grid(a = c(1:10), b = LETTERS[1:5]))
#' # Simulate data columns
#' n_cols <- 10
#' sim_data[, paste0('draw_', c(1:n_cols)):= lapply(paste0('draw_', c(1:n_cols)), function(x) rnorm(.N, 1, 1))]
#' # Use stat maker
#' sim_stats <- stat_maker(data = sim_data, melt = T, merge = F, idvar = c('a', 'b'))
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
