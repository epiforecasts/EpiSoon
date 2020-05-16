#' Draw from the Serial Interval Probability Distribution
#'
#' @param days_ago Numeric vector of days in the past. Defaults to `NULL`.
#' @param serial_interval A numeric vector describing the probability distribution the serial interval.
#' See `EpiNow::covid_serial_interval` for an example of the format.
#'
#' @return A draw from the probability distribution the serial interval.
#' @export
#' @importFrom purrr map_dbl
#' @examples
#'
#' ## Draw
#' draw_from_si_prob(rev(c(1, 2, 4, 10, 1:100)), EpiSoon::example_serial_interval)
draw_from_si_prob <- function(days_ago = NULL,
                              serial_interval = NULL) {


  ##Add zeroth day to days ago
  days_ago <- days_ago + 1

  var_length <- length(serial_interval)
  if (max(days_ago, na.rm = TRUE) > var_length) {
    serial_interval <- c(serial_interval, rep(0, max(days_ago) - var_length))
  }

  draws <- serial_interval[days_ago]


  return(draws)
}


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


#' Predict cases for Rts based on observed data
#'
#' @param cases A dataframe containing `date` and `cases` variables
#' @param rdist A function to be used to sample the number of cases. Must take two
#' arguments with the first specfying the number of samples and the second the mean. Defaults
#' to `rpois` if not supplied
#' @inheritParams predict_cases
#' @inheritParams draw_from_si_prob
#' @return Forecast cases for the current timestep
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_dbl
#' @examples
#'
#' purrr::map_dfr(1:100, ~ predict_current_cases(cases = EpiSoon::example_obs_cases,
#'                       rts = EpiSoon::example_obs_rts,
#'                       serial_interval = EpiSoon::example_serial_interval)) %>%
#'                       dplyr::group_by(date) %>%
#'                       dplyr::summarise(cases = mean(cases))
predict_current_cases <- function(
  cases = NULL,
  rts = NULL,
  serial_interval = NULL,
  rdist = NULL) {

  ## Set sampling dist
  if (is.null(rdist)) {
    rdist <- rpois
  }

  ## Get first and last date
  first_rt_date <- min(rts$date, na.rm = TRUE)
  final_rt_date <- max(rts$date, na.rm = TRUE)

  ## Get early cases
  early_cases <- dplyr::filter(cases, date <= first_rt_date)$cases

  ## Get following cases
  subsequent_cases <- dplyr::filter(cases,
                                    date > first_rt_date,
                                    date <= final_rt_date)$cases

  ## Build an iterative list of cases
  cases <- purrr::map(1:length(subsequent_cases), ~ c(early_cases, subsequent_cases[1:.]))
  cases <- c(list(early_cases), cases)


  predictions <-
    dplyr::mutate(rts,
                  infectiousness =
                    purrr::map_dbl(cases,
                                   function(cases_vect) {
                                     inf <- sum(cases_vect * EpiSoon::draw_from_si_prob(
                                       (length(cases_vect) - 1):0,
                                       serial_interval
                                     ))

                                     return(inf)
                                   }),
                  cases = purrr::map2_dbl(rt, infectiousness, ~ rdist(1, .x * .y))
    )

  return(predictions)

}
