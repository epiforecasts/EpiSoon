

#' Plot a Forecast
#'
#' @param forecast A dataframe as produced by `fit_model`.
#' @param observation A dataframe of observations containing the following variables:
#' `rt` and `date`.
#' @param horizon_cutoff Numeric, defaults to NULL. Forecast horizon to plot up to.
#' @param obs_cutoff_at_forecast Logical defaults to `TRUE`. Should observations only be shown
#' up to the date of the forecast.
#'
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
#'                                 model = bsts::AddAutoAr,
#'                                horizon = 7, samples = 10)
#'
#' forecasts <- forecast_eval$forecasts
#'
#' ## Plot forecast
#' plot_forecast_evaluation(forecasts, observations, horizon_to_plot = 1)
plot_forecast_evaluation <- function(forecasts = NULL,
                                     observation = NULL,
                                     horizon_to_plot = 1) {


  forecasts <- forecasts %>%
    dplyr::filter(horizon == horizon_to_plot)

  plot <- plot_forecast(forecasts,
                observation,
                obs_cutoff_at_forecast = FALSE)

  return(plot)
}
