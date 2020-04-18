
#' Score a case forecast
#'
#' @param pred_cases Dataframe of predicted cases with the following variables: `sample`, `date`,
#' `cases` and forecast horizon. As produced by `forecast_cases`.
#' @param obs_cases Dataframe of observed cases with the following variables: `date` and `cases`.
#' @return A dataframe containing the following scores per forecast timepoint: dss, crps,
#' logs, bias, and sharpness as well as the forecast date and time horizon.
#' @export
#'
#' @inheritParams score_forecast
#' @importFrom dplyr rename
#' @examples
#' ## Fit a model (using a subset of observations)
#' samples <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                      model = function(...) {EpiSoon::bsts_model(model =
#'                      function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                      horizon = 7, samples = 10)
#'
#' pred_cases <- forecast_cases(EpiSoon::example_obs_cases,
#'                              samples, EpiSoon::example_serial_interval)
#'
#' ## Score the model fit (with observations during the time horizon of the forecast)
#' score_case_forecast(pred_cases, EpiSoon::example_obs_cases)
#'
#'
#' ## Score the model fit (with observations during the time horizon of the forecast)
#' score_case_forecast(pred_cases, EpiSoon::example_obs_cases, scores = c("crps", "sharpness", "bias"))
score_case_forecast <- function(pred_cases, obs_cases, scores = "all") {

  pred_cases <-  dplyr::rename(pred_cases, rt = cases)

  obs_cases <- dplyr::rename(obs_cases, rt = cases)

  scores <- EpiSoon::score_forecast(pred_cases, obs_cases,
                                    scores = scores)

  return(scores)
}

