#' Compare timeseries and forecast models
#'
#'
#' @param timeseries A list of named timeseries with each one having a `rt` vector and a `date` vector.
#' @inheritParams compare_models
#' @return A list of dataframes as produced by `evaluate model` but with an additional model column.
#' @export
#' @importFrom purrr transpose
#' @importFrom dplyr bind_rows
#' @importFrom furrr future_map
#' @examples
#'
#' ## Dummy data
#' observations <- data.frame(rt = 1:20,
#'                             date = as.Date("2020-01-01")
#'                            + lubridate::days(1:20))
#'
#' timeseries <- list(observations, observations)
#' names(timeseries) <- c("Region 1", "Region 2")
#'
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("Sparse AR" = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
#'                "Semi-local linear trend" = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
#'
#'
#'
#' ## Compare models
#' evaluations <- compare_timeseries(timeseries, models,
#'                                   horizon = 7, samples = 10)
#'
#' evaluations
#'
#' ## Example evaluation plot for comparing forecasts
#' ## with actuals for a range of models and timeseries.
#' plot_forecast_evaluation(evaluations$forecast, observations, c(7)) +
#'   ggplot2::facet_grid(model ~ region) +
#'   cowplot::panel_border()
#'


compare_timeseries <- function(timeseries = NULL,
                               models = NULL,
                               horizon = 7,
                               samples = 1000,
                               bound_rt = TRUE) {




  evaluations <- timeseries %>%
    furrr::future_map(~ compare_models(observations = ., models = models,
                                       horizon = horizon , samples = samples,
                                       bound_rt = bound_rt),
                       .progress = TRUE) %>%
    purrr::transpose() %>%
    purrr::map(~ dplyr::bind_rows(., .id = "region"))


  return(evaluations)
}
