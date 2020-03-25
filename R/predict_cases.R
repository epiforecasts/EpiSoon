#' Predict cases for Rt forecasts
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
#' @return Forecast cases for over a future forecast horizon.
#' @export
#' @importFrom lubridate days
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_dbl
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
#'                                as.Date("2020-01-20"),
#'                                by = "days"),
#'                    rt = rep(1.2, 20))
#'
#' ## Example serial interval
#' mean_si <- 4.7
#' sd_si <- 2.9
#'
#' mu_log <- log(mean_si) - 1/2 * log((sd_si / mean_si)^2 + 1)
#' sd_log <- sqrt(log((sd_si/mean_si)^2 + 1))
#'
#'
#' serial_interval <- rlnorm(1:100, mu_log, sd_log) %>%
#'    round(0) %>%
#'    table %>%
#'    {. / sum(.)}
#'
#' predict_cases(cases, rts, serial_interval)
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
