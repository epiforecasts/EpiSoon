#' Predict cases for a single Rt sample forecasts
#'
#' @param cases A dataframe containing `date` and `cases` variables
#' @param forecast_date A character string date (format "yyyy-mm-dd") indicating
#' the forecast date. Defaults to `NULL` in which case it will be assumed that the
#' forecast date is the last data present in the `cases` dataframe
#' @param rdist A function to be used to sample the number of cases. Must take two
#' arguments with the first specfying the number of samples and the second the mean. Defaults
#' to `rpois` if not supplied
#' @inheritParams forecast_rt
#' @inheritParams draw_from_si_prob
#' @return Forecast cases for over a future forecast horizon
#' @export
#' @importFrom lubridate days
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_dbl
#' @examples
#'
#' forecast <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                         model = function(...) {EpiSoon::bsts_model(model =
#'                                 function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                         horizon = 7, samples = 1)
#'
#'
#' purrr::map_dfr(1:100, ~ predict_cases(cases = EpiSoon::example_obs_cases,
#'               rts = forecast,
#'               forecast_date = as.Date("2020-03-10"),
#'               serial_interval = example_serial_interval)) %>%
#'               dplyr::group_by(date) %>%
#'               dplyr::summarise(cases = mean(cases))
predict_cases <- function(cases = NULL,
                          rts = NULL,
                          serial_interval = NULL,
                          forecast_date = NULL,
                          horizon = NULL,
                          rdist = NULL) {


  ## Set forecast data to maximum observed case date if not given
  if (is.null(forecast_date)) {
    forecast_date <- max(cases$date)
  }

  ## If horizon is supplied limit the rts to this date
  if (!is.null(horizon)) {
    rts <- rts %>%
      dplyr::filter(date <= (forecast_date + lubridate::days(horizon)))
  }

  ## If no sampler given assume poisson
  if (is.null(rdist)) {
    rdist <- rpois
  }

  ## Filter cases based on forecast date
  cases <- dplyr::filter(cases, date <= as.Date(forecast_date))

  ## Filter rts based on forecast date
  rts <- rts %>%
    dplyr::filter(date > forecast_date)

  ## Initialise predictions for first time point.
  predictions <- rts %>%
    dplyr::mutate(
      index = 1:dplyr::n(),
      ## Calculate infectiousness from onserved data
      infectiousness = purrr::map_dbl(index,
                                 ~ sum(cases$cases * draw_from_si_prob((nrow(cases) + . - 1):.,
                                                           serial_interval))),
      cases = rdist(1, rt[1] * infectiousness[1]))

  if (nrow(rts) > 1) {
    for(i in 2:nrow(rts)) {
      ## Previous cases
      previous_cases <- predictions$cases[1:(i -1)]
      ## Update infectiousness
      predictions[i, ]$infectiousness <- predictions[i, ]$infectiousness +
        sum(previous_cases * draw_from_si_prob(length(previous_cases):1, serial_interval))

      ## Update cases
      predictions[i, ]$cases <- rdist(1, predictions[i, ]$infectiousness * predictions[i, ]$rt)
    }
  }


  ## Select return variables
  predictions <- dplyr::select(predictions,
                               date, cases)


  return(predictions)
}
