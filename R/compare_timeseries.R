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
#' @importFrom purrr transpose
#' @importFrom dplyr bind_rows group_split select
#' @importFrom furrr future_map2
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
#'                     function(ss, y){bsts::AddAr(ss, y = y, lags = 3)},
#'                "Semi-local linear trend" =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
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


  obs_rts <- obs_rts %>%
    dplyr::group_split(timeseries)

  timeseries <- unique(obs_cases$timeseries)

  obs_cases <- obs_cases %>%
    dplyr::group_split(timeseries)

  evaluations <-
    furrr::future_map2(obs_rts, obs_cases,
                       ~ compare_models(obs_rts = dplyr::select(.x, -timeseries),
                                        obs_cases = dplyr::select(.y, -timeseries),
                                        models = models,
                                        horizon = horizon , samples = samples,
                                        bound_rt = bound_rt, timeout = timeout,
                                        serial_interval = serial_interval,
                                        min_points = min_points,
                                        rdist = rdist),
                       .progress = TRUE) %>%
    setNames(timeseries) %>%
    purrr::transpose() %>%
    purrr::map(~ dplyr::bind_rows(., .id = "timeseries"))


  return(evaluations)
}
