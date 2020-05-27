
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
#' @importFrom rlang has_name
#' @examples
#' \dontrun{
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
#'                }
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
  if (rlang::has_name(obs_rts, "sample")) {
    obs_rts <- obs_rts %>%
      dplyr::group_split(sample)
  }else{
    obs_rts <- list(obs_rts)
  }

  if (rlang::has_name(obs_cases, "sample")) {
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
  safe_score_forecast <- purrr::safely(score_forecast)

  scored_forecasts <-
    purrr::map2_dfr(samples, obs_rts,
                    function(sample, obs) {
                      scores <- dplyr::group_split(sample, forecast_date) %>%
                        setNames(unique(sample$forecast_date)) %>%
                        purrr::map(~ dplyr::select(., -forecast_date) %>%
                                     safe_score_forecast(obs) %>%
                                     .$result)

                      scores <- purrr::compact(scores)
                      scores <- dplyr::bind_rows(scores, .id = "forecast_date")
                      return(scores)
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

  ## Score cases for each forecast
  safe_score_case_forecast <- purrr::safely(score_case_forecast)

  scored_cases <-
    purrr::map2_dfr(case_predictions, obs_cases,
                    function(sample, obs) {
                      scores <- dplyr::group_split(sample, forecast_date) %>%
                        setNames(unique(sample$forecast_date)) %>%
                        purrr::map(~ dplyr::select(., -forecast_date) %>%
                                     safe_score_case_forecast(obs) %>%
                                     .$result)

                      scores <- purrr::compact(scores)
                      scores <- dplyr::bind_rows(scores, .id = "forecast_date")
                      return(scores)
                    }, .id = "sample")

  ## Return output
  out <- list(summarised_forecasts, scored_forecasts,
              summarised_case_forecasts, scored_cases)

  names(out) <- c("forecast_rts", "rt_scores", "forecast_cases", "case_scores")

  if (return_raw) {
    out$raw_rt_forecast <- raw_samples
    out$raw_case_forecast <- raw_case_preds
  }

  return(out)
}

#' Compare forecasting models
#'
#' @param models A list of models. A configuration is given in the
#' examples. Each model needs to be wrapped in a function that takes a `...` argument and returns a dataframe
#' of samples with each column representing a time horizon.
#' Example: `function(...) {EpiSoon::bsts_model(model = function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)}`.
#'
#' @inheritParams evaluate_model
#' @return A list of dataframes as produced by `evaluate model` but with an additional model column.
#' @export
#' @importFrom purrr transpose safely
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows
#' @examples
#'\dontrun{
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("AR 3" =
#'                     function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)},
#'                "Semi-local linear trend" =
#'                function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                "ARIMA" =
#'                     function(...){fable_model(model = fable::ARIMA(y ~ time), ...)})
#'
#'
#'
#' ## Compare models
#' evaluations <- compare_models(EpiSoon::example_obs_rts,
#'                               EpiSoon::example_obs_cases, models,
#'                               horizon = 7, samples = 10,
#'                               serial_interval = example_serial_interval)
#'
#' ## Example evaluation plot for comparing forecasts
#' ## with actuals for a range of models and time horizons.
#' plot_forecast_evaluation(evaluations$forecast_rts, EpiSoon::example_obs_rts, c(1, 3, 7)) +
#'   ggplot2::facet_grid(model ~ horizon) +
#'   cowplot::panel_border()
#'
#' ## Hack to plot observed cases vs predicted
#' plot_forecast_evaluation(evaluations$forecast_cases,
#'                          EpiSoon::example_obs_cases, c(1, 3, 7)) +
#'   ggplot2::facet_wrap(model ~ horizon, scales = "free") +
#'   cowplot::panel_border()
#'   }
compare_models <- function(obs_rts = NULL,
                           obs_cases = NULL,
                           models = NULL,
                           horizon = 7, samples = 1000,
                           bound_rt = TRUE, timeout = 30,
                           serial_interval = NULL,
                           min_points = 3,
                           rdist = NULL,
                           return_raw = FALSE) {


  safe_eval <- purrr::safely(evaluate_model)

  ## Evaluate each model (potential to swap in furrr here)
  evaluations <- models %>%
    furrr::future_map(
      ~ safe_eval(obs_rts,
                  obs_cases,
                  model = .,
                  horizon = horizon,
                  samples = samples,
                  bound_rt = bound_rt,
                  timeout = timeout,
                  serial_interval = serial_interval,
                  min_points = min_points,
                  rdist = rdist,
                  return_raw = return_raw)[[1]],
      .progress = TRUE
    ) %>%
    purrr::transpose() %>%
    purrr::map(~ dplyr::bind_rows(., .id = "model"))


  return(evaluations)

}

#' Compare timeseries and forecast models
#'
#'
#' @param obs_rts A dataframe of observed Rts including a `timeseries` variable to denote
#' each timeseris and a  `rt` vector (to forecast) and a `date` vector (to denote time). Optionally this dataframe can
#' contain samples for each timeseries in which case this should be denoted using a `sample` variable.
#' @param obs_cases A dataframe of observed cases including a `timeseries` variable to denote
#' each timeseris and a  `cases` vector (to forecast) and a `date` vector (to denote time). Optionally this dataframe can
#' contain samples for each timeseries in which case this should be denoted using a `sample` variable.
#' @inheritParams compare_models
#' @return A list of dataframes as produced by `evaluate model` but with an additional model column.
#' @export
#' @importFrom purrr transpose map
#' @importFrom dplyr group_split select mutate sample_frac arrange
#' @importFrom furrr future_pmap
#' @importFrom tidyr expand_grid unnest
#' @importFrom tibble tibble
#' @examples
#'\dontrun{
#' ## Example data
#' obs_rts <- EpiSoon::example_obs_rts %>%
#'     dplyr::mutate(timeseries = "Region 1") %>%
#'     dplyr::bind_rows(EpiSoon::example_obs_rts %>%
#'     dplyr::mutate(timeseries = "Region 2"))
#'
#' obs_cases <- EpiSoon::example_obs_cases %>%
#'     dplyr::mutate(timeseries = "Region 1") %>%
#'     dplyr::bind_rows(EpiSoon::example_obs_cases %>%
#'     dplyr::mutate(timeseries = "Region 2"))
#'
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("AR 3" =
#'                     function(...){EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)},
#'                "Semi-local linear trend" =
#'                function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                "ARIMA" =
#'                     function(...){fable_model(model = fable::ARIMA(y ~ time), ...)})
#'
#'
#' ## Compare models
#' evaluations <- compare_timeseries(obs_rts, obs_cases, models,
#'                                   horizon = 7, samples = 10,
#'                                   serial_interval = EpiSoon::example_serial_interval)
#'
#' evaluations
#'
#' ## Example evaluation plot for comparing forecasts
#' ## with actuals for a range of models and timeseries.
#' plot_forecast_evaluation(evaluations$forecast_rts, obs_rts, c(7)) +
#'   ggplot2::facet_grid(model ~ timeseries) +
#'   cowplot::panel_border()
#'
#' ## Hack to plot observed cases vs predicted
#' plot_forecast_evaluation(evaluations$forecast_cases,
#'                          obs_cases, c(7)) +
#'   ggplot2::facet_grid(model ~ timeseries, scales = "free") +
#'   cowplot::panel_border()
#'   }


compare_timeseries <- function(obs_rts = NULL,
                               obs_cases = NULL,
                               models = NULL,
                               horizon = 7,
                               samples = 1000,
                               bound_rt = TRUE,
                               min_points = 3,
                               timeout = 30,
                               serial_interval = NULL,
                               rdist = NULL,
                               return_raw = FALSE) {

  ## Make a nested tibble of timseries and observed ata
  data_tibble <- tibble::tibble(
    timeseries = unique(obs_cases$timeseries),
    rts = obs_rts %>%
      dplyr::group_split(timeseries),
    cases = obs_cases %>%
      dplyr::group_split(timeseries)
  )

  ## Make a tibble of all data and model combinations
  combinations <- tidyr::expand_grid(data = list(data_tibble), models = models) %>%
    tidyr::unnest("data") %>%
    dplyr::mutate(model_name = names(models)) %>%
    ## Randomised order to load balance across cores
    dplyr::sample_frac(size = 1, replace = FALSE)

  ## Safe model evaluation
  safe_eval <- purrr::safely(evaluate_model)


  ## Run each row combination and pull out the model evaluation
  evaluations <- combinations %>%
    dplyr::mutate(
      eval = furrr::future_pmap(list(rts, cases, models),
                                function(rt, case, model) {
                                  safe_eval(
                                    obs_rts = rt,
                                    obs_cases = case,
                                    model = model,
                                    horizon = horizon,
                                    samples = samples,
                                    bound_rt = bound_rt,
                                    timeout = timeout,
                                    serial_interval = serial_interval,
                                    min_points = min_points,
                                    rdist = rdist,
                                    return_raw = return_raw
                                  )[[1]]}),
      .progress = TRUE) %>%
    dplyr::select(timeseries, model = model_name, eval) %>%
    dplyr::arrange(timeseries, model)

  ## Output
  out_names <- c("forecast_rts", "rt_scores", "forecast_cases", "case_scores")

  if (return_raw) {
    out_names <- c(out_names, "raw_rt_forecast", "raw_case_forecast")
  }

  out <- purrr::map(out_names, function(list_obj) {
    out <- evaluations %>%
      dplyr::mutate(eval = purrr::map(eval, ~ .[[list_obj]])) %>%
      tidyr::unnest("eval")
  })

  names(out) <- out_names


  return(out)
}
