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
#' @importFrom dplyr group_split select mutate
#' @importFrom furrr future_pmap
#' @importFrom tidyr expand_grid unnest
#' @importFrom tibble tibble
#' @examples
#'
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
#'                     function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)},
#'                "Semi-local linear trend" =
#'                function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)})
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


compare_timeseries <- function(obs_rts = NULL,
                               obs_cases = NULL,
                               models = NULL,
                               horizon = 7,
                               samples = 1000,
                               bound_rt = TRUE,
                               min_points = 3,
                               timeout = 30,
                               serial_interval = NULL,
                               rdist = NULL) {

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
    dplyr::mutate(model_name = names(models))

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
                                    rdist = rdist
                                  )[[1]]}),
      .progress = TRUE) %>%
      dplyr::select(timeseries, model = model_name, eval)

  ## Output
  out_names <- c("forecast_rts", "rt_scores", "forecast_cases", "case_scores",
           "raw_rt_forecast", "raw_case_forecast")

  out <- purrr::map(out_names, function(list_obj) {
    out <- evaluations %>%
      dplyr::mutate(eval = purrr::map(eval, ~ .[[list_obj]])) %>%
      tidyr::unnest("eval")
  })

  names(out) <- out_names


  return(out)
}
