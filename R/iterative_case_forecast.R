#' Iteratively forecast cases using a iterative Rt forecasts
#'
#' @param it_fit_samples Dataframe of iterative forecasts as produced by `iterative_rt_forecast`.
#' @return A dataframe of iterative case forecasts
#' @export
#' @inheritParams forecast_cases
#'
#' @importFrom purrr map_dfr safely
#' @importFrom dplyr filter group_split
#' @examples
#'
#' cases <- data.frame(cases = 1:20,
#'                     date = as.Date("2020-01-01") + lubridate::days(1:20))
#' rts <- data.frame(rt = 1:10,
#'                  date = as.Date("2020-01-01") + lubridate::days(1:10))
#'
#'
#' it_forecast <-
#'   iterative_rt_forecast(rts, model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                         horizon = 7, samples = 10)
#'
#'#' ## Example serial interval
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
#' iteractive_case_forecast(it_fit_samples = it_forecast, cases = cases,
#'                          serial_interval = serial_interval)
iteractive_case_forecast <- function(it_fit_samples = NULL, cases = NULL,
                                    serial_interval, rdist = NULL) {

  predictions <- it_fit_samples %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(it_fit_samples$forecast_date)) %>%
    purrr::map_dfr( ~ forecast_cases(
      cases,
      fit_samples = dplyr::select(., -forecast_date),
      rdist = rdist,
      serial_interval = serial_interval,
      forecast_date = .$forecast_date[1]
    ), .id = "forecast_date")

  return(predictions)

}

# rts <- out %>%
#   tidyr::unnest(R) %>%
#   dplyr::group_by(date) %>%
#   dplyr::mutate(sample = 1:dplyr::n()) %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(rt = R)
#
# preds <- rts %>%
#   dplyr::group_split(sample) %>%
#   purrr::map_dfr(~iterative_case_forecast(rts = dplyr::select(., -sample),
#                                           cases = cases,
#                                           serial_interval = serial_intervals[, 1],
#                                           horizon = 1),
#                  .id = "sample") %>%
#   dplyr::mutate(sample = as.numeric(sample)) %>%
#   dplyr::select(forecast_date, date, cases, horizon, sample)
#
#
# scores <- preds %>%
#   dplyr::group_split(forecast_date) %>%
#   setNames(unique(preds$forecast_date)) %>%
#   purrr::map_dfr(~score_case_forecast(dplyr::select(., -forecast_date),
#                                       cases), .id = "forecast_date")
#
# summarised_score <- scores %>%
#   dplyr::summarise(crps = mean(crps, na.rm = TRUE))
