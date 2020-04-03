
#' Evaluate a Model for Forecasting Rts
#'
#' @param obs_rts Dataframe of Rt observations to forecast with and score
#' against. Should contain a `date` and `rt` column. If multiple samples are included this
#' should be denoted using a numeric `sample` variable.
#' @param obs_cases Dataframe of case observations to use for case prediction and
#' scoring. Should contain a `date` and `cases` column. If multiple samples are included this
#' should be denoted using a numeric `sample` variable.
#' @param return_raw Logical, should raw cases and rt forecasts be returned. Defaults to `FALSE`.
#' @inheritParams score_forecast
#' @inheritParams iterative_rt_forecast
#' @inheritParams iterative_case_forecast
#' @return
#' @export
#' @importFrom dplyr slice group_split filter
#' @importFrom purrr map_dfr map2 map2_dfr
#' @examples
#' ## Evaluate a model based on a single sample of input cases
#' evaluate_model(EpiSoon::example_obs_rts,
#'                EpiSoon::example_obs_cases,
#'                model = function(...) {EpiSoon::bsts_model(model =
#'                                 function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                horizon = 7, samples = 10,
#'                serial_interval = example_serial_interval)
#'
#'
#' ## Samples of observed data
#' sampled_obs <- EpiSoon::example_obs_rts %>%
#'    dplyr::mutate(sample = 1) %>%
#'    dplyr::bind_rows(EpiSoon::example_obs_rts %>%
#'    dplyr::mutate(sample = 2))
#'
#' sampled_cases <- EpiSoon::example_obs_cases %>%
#'    dplyr::mutate(sample = 1) %>%
#'    dplyr::bind_rows(EpiSoon::example_obs_cases %>%
#'    dplyr::mutate(sample = 2))
#'
#'
#' ## Evaluate a model across samples
#' evaluate_model(sampled_obs,
#'                sampled_cases,
#'                model = function(...) {EpiSoon::bsts_model(model =
#'                                 function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                horizon = 7, samples = 10,
#'                serial_interval = EpiSoon::example_serial_interval)
evaluate_model <- function(obs_rts = NULL,
                           obs_cases = NULL,
                           model = NULL,
                           horizon = 7,
                           samples = 1000,
                           timeout = 30,
                           bound_rt = TRUE,
                           min_points = 3,
                           serial_interval = NULL,
                           rdist = NULL,
                           return_raw = FALSE) {

  ## Split obs_rt into a list if present
  if (!is.null(suppressWarnings(obs_rts$sample))) {
    obs_rts <- obs_rts %>%
      dplyr::group_split(sample)
  }else{
    obs_rts <- list(obs_rts)
  }

  if (!is.null(suppressWarnings(obs_cases$sample))) {
    obs_cases <- obs_cases %>%
      dplyr::group_split(sample) %>%
      purrr::map(~ select(., -sample))
  }else{
    obs_cases <- list(obs_cases)
  }

  if (length(obs_cases) != length(obs_rts)) {
    stop("Must have the same number of Rt and case samples.")
  }

  ## Iteratively forecast for each time point
  safe_it <- purrr::safely(iterative_rt_forecast)

  samples <- obs_rts %>%
    purrr::map_dfr(
      ~ safe_it(., model = model, horizon = horizon,
                           samples = samples, bound_rt = bound_rt,
                           timeout = timeout, min_points = min_points)[[1]],
      .id = "obs_sample")


  ## Summarise the forecast
  summarised_forecasts <- samples %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(samples$forecast_date)) %>%
    purrr::map_dfr(summarise_forecast, .id = "forecast_date")


  if (return_raw) {
    ## Raw forecasts
    raw_samples <- samples
  }


  ## Filter the forecasts to be in line with observed data
  samples <- samples %>%
    dplyr::group_split(obs_sample) %>%
    purrr::map(~ dplyr::select(., -obs_sample)) %>%
    purrr::map2(obs_rts,
                ~ dplyr::filter(.x, date <= max(.y$date)))

  ## Score the forecasts
  scored_forecasts <-
    purrr::map2_dfr(samples, obs_rts,
                    function(sample, obs) {
                      dplyr::group_split(sample, forecast_date) %>%
                        setNames(unique(sample$forecast_date)) %>%
                        purrr::map_dfr(~ dplyr::select(., -forecast_date) %>%
                                         score_forecast(obs), .id = "forecast_date")
                    }, .id = "sample")

  if (length(unique(scored_forecasts$sample)) == 1) {
    scored_forecasts <- scored_forecasts %>%
      dplyr::select(-sample)
  }

  safe_case <- purrr::safely(iterative_case_forecast)

  ## Predict cases for each iterative forecast
  case_predictions <- purrr::map2_dfr(
    samples, obs_cases,
    function(sample, case) {
      safe_case(
        it_fit_samples = sample, cases = case,
        serial_interval = serial_interval, rdist = rdist
      )[[1]]
    }, .id = "obs_sample")

  summarised_case_forecasts <- case_predictions %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(case_predictions$forecast_date)) %>%
    purrr::map_dfr(summarise_case_forecast, .id = "forecast_date")

  if (return_raw) {
    ## Summarise case predictions
    raw_case_preds <- case_predictions
  }


  ## Limit case predictions to observed data
  case_predictions <- case_predictions %>%
    dplyr::group_split(obs_sample) %>%
    purrr::map(~ dplyr::select(., -obs_sample)) %>%
    purrr::map2(obs_cases,
                ~ dplyr::filter(.x, date <= max(.y$date)))

  ## Score for each forecast
  score_cases <-
    purrr::map2_dfr(case_predictions, obs_cases,
                    function(sample, obs) {
                      dplyr::group_split(sample, forecast_date) %>%
                        setNames(unique(sample$forecast_date)) %>%
                        purrr::map_dfr(~ dplyr::select(., -forecast_date) %>%
                                         score_case_forecast(obs), .id = "forecast_date")
                    }, .id = "sample")


  ## Return output
  out <- list(summarised_forecasts, scored_forecasts,
              summarised_case_forecasts, score_cases)

  names(out) <- c("forecast_rts", "rt_scores", "forecast_cases", "case_scores")

  if (return_raw) {
    out$raw_rt_forecast <- raw_samples
    out$raw_case_forecast <- raw_case_preds
  }

  return(out)
}
