#' Compare timeseries and forecast models
#'
#'
#' @param observations A dataframe of observations including a `timeseries` variable to denote
#' each timeseris and a  `rt` vector (to forecast) and a `date` vector (to denote time). Optionally this dataframe can
#' contain samples for each timeseries in which case this should be denoted using a `sample` variable.
#' @inheritParams compare_models
#' @return A list of dataframes as produced by `evaluate model` but with an additional model column.
#' @export
#' @importFrom purrr transpose
#' @importFrom dplyr bind_rows group_split
#' @importFrom furrr future_map
#' @examples
#'
#' ## Dummy data
#' observations <- data.frame(rt = 1:20,
#'                             date = as.Date("2020-01-01")
#'                            + lubridate::days(1:20))
#'
#' observations <- observations %>%
#'     dplyr::mutate(timeseries = "Region 1") %>%
#'     dplyr::bind_rows(observations %>%
#'     dplyr::mutate(timeseries = "Region 2"))
#'
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("Sparse AR" = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
#'                "Semi-local linear trend" = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
#'
#'
#'
#' ## Compare models
#' evaluations <- compare_timeseries(observations, models,
#'                                   horizon = 7, samples = 10)
#'
#' evaluations
#'
#' ## Example evaluation plot for comparing forecasts
#' ## with actuals for a range of models and timeseries.
#' plot_forecast_evaluation(evaluations$forecast, observations, c(7)) +
#'   ggplot2::facet_grid(model ~ timeseries) +
#'   cowplot::panel_border()
#'


compare_timeseries <- function(observations = NULL,
                               models = NULL,
                               horizon = 7,
                               samples = 1000,
                               bound_rt = TRUE) {


  evaluations <- observations %>%
    dplyr::group_split(timeseries) %>%
    setNames(unique(observations$timeseries)) %>%
    furrr::future_map(~ compare_models(observations = ., models = models,
                                       horizon = horizon , samples = samples,
                                       bound_rt = bound_rt),
                       .progress = TRUE) %>%
    purrr::transpose() %>%
    purrr::map(~ dplyr::bind_rows(., .id = "timeseries"))


  return(evaluations)
}
