

#' Plot a Forecast
#'
#' @param forecasts A dataframe as produced by `forecast_rt`
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
#' observations <- data.frame(rt = 1:20,
#'                            date = as.Date("2020-01-01")
#'                                   + lubridate::days(1:20))
#'
#' ## Evaluate a model
#' forecast_eval <- evaluate_model(observations,
#'                                 model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                                horizon = 7, samples = 10)
#'
#' forecasts <- forecast_eval$forecasts
#'
#' ## Plot forecast
#' plot_forecast_evaluation(forecasts, observations, horizon_to_plot = 7)
plot_forecast_evaluation <- function(forecasts = NULL,
                                     observations = NULL,
                                     horizon_to_plot = 1) {


  forecasts <- forecasts %>%
    dplyr::filter(horizon == horizon_to_plot)

  plot <- plot_forecast(forecasts,
                observations,
                obs_cutoff_at_forecast = FALSE)

  return(plot)
}
