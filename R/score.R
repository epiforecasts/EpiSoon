
#' Score a Model Fit
#'
#' @param observations A dataframe of observations against which to score. Should contain a `date` and `rt` column.
#' @param scores Character vector defaulting to "all". Select which scores to return, default is all scores but
#' any subset can be returned.
#' @return A dataframe containing the following scores per forecast timepoint: dss, crps,
#' logs, bias, and sharpness as well as the forecast date and time horizon.
#' @export
#'
#' @importFrom dplyr filter select select_if
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom scoringutils score
#' @inheritParams summarise_forecast
#' @examples
#' \dontrun{
#' ## Fit a model (using a subset of observations)
#' samples <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                      model = function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                      horizon = 7, samples = 10)
#'
#' ## Score the model fit (with observations during the time horizon of the forecast)
#' score_forecast(samples, EpiSoon::example_obs_rts)
#'
#' ## Return just CRPS, bias and sharpness
#' score_forecast(samples, EpiSoon::example_obs_rts, scores = c("crps", "sharpness", "bias"))
#'
#' ## Return just the CRPS
#' score_forecast(samples, EpiSoon::example_obs_rts, scores = "crps")
#' }
score_forecast <- function(fit_samples, observations, scores = "all") {

  observations <-
    dplyr::filter(observations,
                  date >= min(fit_samples$date),
                  date <= max(fit_samples$date)
    )
  observations <-
    dplyr::rename(observations, true_value = rt)

  fit_samples <-
    dplyr::filter(fit_samples,
                  date >= min(observations$date),
                  date <= max(observations$date)
    )
  fit_samples <-
    dplyr::rename(fit_samples, prediction = rt)

  combined <-
    dplyr::inner_join(observations, fit_samples, by = "date", multiple = "all")

  scores <- scoringutils::score(combined)
  scores <- dplyr::select_if(scores, ~ any(!is.na(.)))

  return(scores)
}



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
#' \dontrun{
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
#' }
score_case_forecast <- function(pred_cases, obs_cases, scores = "all") {

  pred_cases <-  dplyr::rename(pred_cases, rt = cases)

  obs_cases <- dplyr::rename(obs_cases, rt = cases)

  scores <- EpiSoon::score_forecast(pred_cases, obs_cases,
                                    scores = scores)

  return(scores)
}

