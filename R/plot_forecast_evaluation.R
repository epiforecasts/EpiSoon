

#' Plot a Forecast
#'
#' @param forecasts A dataframe as produced by `forecast_rt`
#' or `forecast_cases`
#' @param horizon_to_plot Numeric vector, the forecast horizon to plot.
#'
#' @inheritParams plot_forecast
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_x_date labs
#' @importFrom cowplot theme_cowplot
#' @return A `ggplot2` object
#' @export
#'
#' @examples
#'
#'
#' ## Evaluate a model
#' forecast_eval <- evaluate_model(EpiSoon::example_obs_rts,
#'                                 EpiSoon::example_obs_cases,
#'                                 model = function(...) {EpiSoon::bsts_model(model =
#'                                 function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                                 serial_interval = EpiSoon::example_serial_interval,
#'                                 horizon = 7, samples = 10)
#'
#' ## Plot Rt forecast
#' plot_forecast_evaluation(forecast_eval$forecast_rts,
#'                          EpiSoon::example_obs_rts,
#'                          horizon_to_plot = 7)
#'
#'
#' ## Plot case forecast
#' plot_forecast_evaluation(forecast_eval$forecast_cases,
#'                          EpiSoon::example_obs_cases,
#'                          horizon_to_plot = 7)
#'
plot_forecast_evaluation <- function(forecasts = NULL,
                                     observations = NULL,
                                     horizon_to_plot = 1) {


  forecasts <- forecasts %>%
    dplyr::filter(horizon %in% horizon_to_plot)

  plot <- plot_forecast(forecasts,
                observations,
                obs_cutoff_at_forecast = FALSE)

  return(plot)
}
