
#' Score a Model Fit
#'
#' @param observations A dataframe of observations against which to score. Should contain a `date` and `rt` column.
#'
#' @return A dataframe containing the following scores per forecast timepoint: dss, crps,
#' logs, bias, and sharpness as well as the forecast date and time horizon.
#' @export
#'
#' @importFrom dplyr filter select
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom scoringRules dss_sample crps_sample logs_sample
#' @importFrom scoringutils bias sharpness
#' @inheritParams summarise_forecast
#' @examples
#'
#' ## Observed data
#' observations <- data.frame(rt = 1:20,
#'                            date = as.Date("2020-01-01")
#'                                   + lubridate::days(1:20))
#'
#' ## Fit a model (using a subset of observations)
#' samples <- forecast_rt(observations[1:10, ],
#'                      model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                      horizon = 7, samples = 10)
#' pred_cases <- predict_cases()
#'
#' ## Score the model fit (with observations during the time horizon of the forecast)
#' score_forecast(samples, observations)
score_case_forecast <- function(pred_cases, obs_cases) {
  pred_cases <- pred_cases %>%
    dplyr::rename(rt = cases)

  obs_cases <- obs_cases %>%
    dplyr::rename(rt = cases)

  scores <- EpiSoon::score_forecast(pred_cases, obs_cases)

  return(scores)
}

