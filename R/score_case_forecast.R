
#' Score a case forecast
#'
#' @param pred_cases Dataframe of predicted cases with the following variables: `sample`, `date`,
#' `cases` and forecast horizon. As produced by `forecast_cases`.
#' @param obs_cases Dataframe of observed cases with the following variables: `date` and `cases`.
#' @return A dataframe containing the following scores per forecast timepoint: dss, crps,
#' logs, bias, and sharpness as well as the forecast date and time horizon.
#' @export
#'
#' @importFrom dplyr rename
#' @examples
#'
#' ## Observed cases
#' cases <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-20"),
#'                                by = "days"),
#'                     cases = 1:20)
#' ## Observed rts
#' observations <- data.frame(rt = 1:20,
#'                            date = as.Date("2020-01-01")
#'                                   + lubridate::days(1:20))
#'
#' ## Fit a model (using a subset of observations)
#' samples <- forecast_rt(observations[1:10, ],
#'                      model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                      horizon = 7, samples = 10)
#'
#' pred_cases <- forecast_cases(cases[1:10, ], samples, EpiSoon::example_serial_interval)
#'
#' ## Score the model fit (with observations during the time horizon of the forecast)
#' score_case_forecast(pred_cases, cases)
score_case_forecast <- function(pred_cases, obs_cases) {
  pred_cases <- pred_cases %>%
    dplyr::rename(rt = cases)

  obs_cases <- obs_cases %>%
    dplyr::rename(rt = cases)

  scores <- EpiSoon::score_forecast(pred_cases, obs_cases)

  return(scores)
}

