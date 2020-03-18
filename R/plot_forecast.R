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
#'                            date = as.Date("2020-01-01") + lubridate::days(1:20))
#'
#' ## Fit a model
#' samples <- fit_model(observations[1:10, ],
#'                      model = bsts::AddAutoAr,
#'                      horizon = 7, samples = 10)
#'
#' ## Summarise forecast
#' summarised_forecast <- summarise_forecast(samples)
#'
#' ## Plot forecast
#' plot_forecast(summarised_forecast, observations)
plot_forecast <- function(forecast = NULL,
                          observation = NULL,
                          horizon_cutoff = NULL,
                          obs_cutoff_at_forecast = TRUE) {


  if (!is.null(obs_cutoff_at_forecast)) {
    observations <- observations %>%
      dplyr::filter(date <= min(forecast$date))
  }

  if (!is.null(horizon_cutoff)) {
    forecast <- forecast %>%
      dplyr::filter(horizon <= horizon_cutoff)
  }

  plot <- ggplot2::ggplot(forecast, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = bottom), alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = top), alpha =  0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = bottom, ymax = top), alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::geom_line(data = observations,
                       ggplot2::aes(y = rt), size = 1.1) +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::labs(x = "Date", y = "Effective Reproduction no.")

  return(plot)
  }
