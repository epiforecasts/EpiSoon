#' Forecasts cases for a Rt forecasts
#'
#' @param cases A dataframe containing `date` and `cases` variables
#' @param forecast_date A character string date (format "yyyy-mm-dd") indicating
#' the forecast date. Defaults to `NULL` in which case it will be assumed that the
#' forecast date is the  day before the first date present in the `fit_samples` dataframe
#' @param rdist A function to be used to sample the number of cases. Must take two
#' arguments with the first specfying the number of samples and the second the mean. Defaults
#' to `rpois` if not supplied
#' @inheritParams forecast_rt
#' @inheritParams summarise_forecast
#' @inheritParams predict_cases
#' @return Forecast cases for over a future forecast horizon
#' @export
#' @importFrom dplyr mutate group_split
#' @importFrom purrr map_dfr
#' @importFrom lubridate days
#' @examples
#'
#' forecast <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                         model = function(ss, y) {
#'                         bsts::AddAutoAr(ss, y = y, lags = 7)
#'                         },
#'                         horizon = 7, samples = 10)
#'
#'
#' forecast_cases(EpiSoon::example_obs_cases,
#'                fit_samples =  forecast,
#'                serial_interval = EpiSoon::example_serial_interval)
forecast_cases <- function(cases = NULL, fit_samples = NULL,
                           serial_interval = NULL, forecast_date = NULL,
                           horizon = NULL, rdist = NULL
                           ){

  if(is.null(serial_interval)) {
    stop("serial_interval is missing. See EpiSoon::example_serial_interval for the required format.")
  }
  predictions <- fit_samples %>%
    dplyr::group_split(sample) %>%
    purrr::map_dfr(~ predict_cases(
          cases = cases,
          rts = .,
          serial_interval = serial_interval,
          forecast_date = min(.$date, na.rm = TRUE) - lubridate::days(1),
          horizon = horizon,
          rdist = rdist) %>%
       dplyr::mutate(horizon = 1:dplyr::n()), .id = "sample") %>%
    dplyr::mutate(sample = as.numeric(sample))


  return(predictions)
}
