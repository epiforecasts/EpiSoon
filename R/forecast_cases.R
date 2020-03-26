#' Forecasts cases for a Rt forecasts
#'
#' @param cases A dataframe containing `date` and `cases` variables
#' @param forecast_date A character string date (format "yyyy-mm-dd") indicating
#' the forecast date. Defaults to `NULL` in which case it will be assumed that the
#' forecast date is the last data present in the `cases` dataframe
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
#' @examples
#'
#' ## Observed cases
#' cases <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-10"),
#'                                by = "days"),
#'                     cases = 1:10)
#'
#' ## Forecast Rts
#' rts <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-10"),
#'                                by = "days"),
#'                    rt = rnorm(10, 1.2, 0.01))
#'
#'
#' forecast <- forecast_rt(rts,
#'                         model = function(ss, y) {
#'                         bsts::AddAutoAr(ss, y = y, lags = 7)
#'                         },
#'                         horizon = 7, samples = 10)
#'
#'
#' forecast_cases(cases, forecast, EpiSoon::example_serial_interval)
forecast_cases <- function(cases = NULL, fit_samples = NULL,
                           serial_interval = NULL, forecast_date = NULL,
                           horizon = NULL, rdist = NULL
                           ){

  predictions <- fit_samples %>%
    dplyr::group_split(sample) %>%
    purrr::map_dfr(~ predict_cases(
          cases = cases,
          rts = .,
          serial_interval = serial_interval,
          forecast_date = forecast_date,
          horizon = horizon,
          rdist = rdist) %>%
       dplyr::mutate(horizon = 1:dplyr::n()), .id = "sample") %>%
    dplyr::mutate(sample = as.numeric(sample))


  return(predictions)
}
