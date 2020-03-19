#' #' Compare timeseries and forecast models
#' #'
#' #'
#' #' @param timeseries A list of named timeseries with each one having a `rt` vector and a `date` vector.
#' #' @inheritParams compare_models
#' #' @return A list of dataframes as produced by `evaluate model` but with an additional model column.
#' #' @export
#' #' @importFrom purrr transpose
#' #' @importFrom dplyr bind_rows
#' #' @importFrom furrr future_map
#' #' @examples
#' #'
#' #' ## Dummy data
#' #' observations <- data.frame(rt = 1:20,
#' #'                             date = as.Date("2020-01-01")
#' #'                            + lubridate::days(1:20))
#' #'
#' #' timeseries <- list(observations, observations)
#' #' names(timeseries) <- c("Region 1", "Region 2")
#' #'
#' #' ## List of forecasting bsts models wrapped in functions.
#' #' models <- list("Sparse AR" = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
#' #'                "Semi-local linear trend" = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
#' #'
#' #'
#' #'
#' #' ## Compare models
#' #' evaluations <- compare_timeseries(timeseries, models,
#' #'                                   horizon = 7, samples = 10)
#' #'
#' #' evaluations
#' #'
#' #' scores <- evaluations$scores
#' #'
#' #' ## Example evaluation plot for comparing forecasts
#' #' ## with actuals for a range of models and timeseries.
#' #' plot_forecast_evaluation(evaluations$forecast, observations, c(7)) +
#' #'   ggplot2::facet_grid(model ~ region) +
#' #'   cowplot::panel_border()
#' #'
#'
#' summarise_scores <- function(scores, variables = NULL) {
#'
#'
#'   default_groups <- "score"
#'
#'   if (!is.null(scores[["model"]])) {
#'     default_groups <- c(default_groups, "model")
#'   }
#'
#'   summarised_scores <- scores %>%
#'     tidyr::gather(key = "score", value = "value", dss, crps, logs, bias, sharpness) %>%
#'     dplyr::group_by(.dots = c(variables, default_groups)) %>%
#'     dplyr::summarise(
#'       bottom = quantile(value, 0.025, na.rm = TRUE),
#'       lower =  quantile(value, 0.25, na.rm = TRUE),
#'       median =  median(value, na.rm = TRUE),
#'       mean = mean(value, na.rm = TRUE),
#'       upper = quantile(value, 0.75, na.rm = TRUE),
#'       top = quantile(value, 0.975, na.rm = TRUE),
#'       sd = sd(value, na.rm = TRUE)
#'     ) %>%
#'     dplyr::ungroup()
#'
#'
#'   return(summarised_scores)
#' }
#'
#'
#' plot_scores <- function(scores) {
#'
#'
#'
#' }
