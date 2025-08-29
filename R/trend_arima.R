#' trend prediction with arima model
#'
#' @param year # years of input data
#' @param y # y values of input data (e.a. TFR or MAB)
#' @param year_begin # begin of prediction
#' @param year_end # end of prediction
#' @param trend_prop # y value of the end point: proportion of trend vs. past
#'
#' @return #tibble prediction data
#' @export
#' @autoglobal
#'
#' @examples
trend_arima <- function(year, y, year_begin, year_end, trend_prop) {
  
# years: numeric
  year_begin <- as.numeric(year_begin)  
  year_end <- as.numeric(year_end)
  trend_prop <- as.numeric(trend_prop) 
  
# last value of past
  y_past_last <- tail(y, 1)
  
# arima: fit and forecast
  arima_model <- forecast::auto.arima(y, d = 2, max.p = 3, max.q = 3)
  arima_forecast <- forecast::forecast(arima_model, h = year_end - year_begin + 1)

# output
  dat_out <- tibble(
    year = year_begin:year_end,
    y_pred = as.numeric(pmax(0, arima_forecast$mean)),
    category = "ARIMA"
  ) |> 
    dplyr::mutate(y = y_past_last + trend_prop * (y_pred - y_past_last))

  return(dat_out)
}



