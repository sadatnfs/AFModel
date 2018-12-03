
#' @title Forecasting a vector of data with an ARIMA process
#' @description This function will take in a vector of data and forecast using an ARIMA process as specified
#' @param data vector of data
#' @param order ARIMA order (defaults to an AR(1) process)
#' @param start first year of data available
#' @param end_data last year of data available
#' @param end_forecast last year to forecast to
#' @return a vector of data with length \code{end_forecast - start} which has the forecasted data appended
#' @rdname arima_forecast
#' @export
arima_forecast <- function(data,  order = c(1,0,0), start, end_data, end_forecast) {

  ## Declare TS data
  ts_data <- ts(data, start = start, end = end_data)

  ## Run AR(1) model
  ar1_model <- Arima(y = ts_data,
                     order = order,
                     include.drift = T)

  ## Forecast to 2100
  forecast_model <- forecast(ar1_model, h = eval(end_forecast - end_data))

  ## Return vector
  return(c(as.vector(ts_data), as.vector(forecast_model$mean) ))

}
