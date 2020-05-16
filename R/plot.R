#' Plot a Forecast
#'
#' @param forecast A dataframe with summarised forecasts as produced
#' by `summarise_forecast` or `summarise_case_forecast` .
#' @param observations A dataframe of observations containing the
#' following variables:
#' - either `rt` or `cases`
#' - and `date`.
#' @param horizon_cutoff Numeric, defaults to NULL. Forecast horizon to plot up to.
#' @param obs_cutoff_at_forecast Logical defaults to `TRUE`. Should observations only be shown
#' up to the date of the forecast.
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_x_date labs
#' @importFrom cowplot theme_cowplot
#' @importFrom rlang has_name
#' @return A `ggplot2` object
#' @export
#'
#' @examples
#'
#' ## Forecast an Rt sample
#' samples <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                      model = function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                      horizon = 21, samples = 10)
#'
#' ## Summarise forecast
#' summarised_forecast <- summarise_forecast(samples)
#'
#' ## Plot forecast_cases
#' plot_forecast(summarised_forecast, EpiSoon::example_obs_rts)
#'
#' ## Forecast a case sample
#' pred_cases <- forecast_cases(EpiSoon::example_obs_cases, samples,
#'                             serial_interval = EpiSoon::example_serial_interval)
#'
#' summarised_case_forecast <- summarise_case_forecast(pred_cases)
#'
#' plot_forecast(summarised_case_forecast, EpiSoon::example_obs_cases)
plot_forecast <- function(forecast = NULL,
                          observations = NULL,
                          horizon_cutoff = NULL,
                          obs_cutoff_at_forecast = TRUE) {

  if (obs_cutoff_at_forecast) {
    observations <- observations %>%
      dplyr::filter(date < min(forecast$date))
  }

  if (!is.null(horizon_cutoff)) {
    forecast <- forecast %>%
      dplyr::filter(horizon <= horizon_cutoff)
  }

  if ("cases" %in% colnames(observations)) {
    case_plot <- TRUE
    observations <- observations %>%
      dplyr::mutate(y = cases)
  } else {
    case_plot <- FALSE
    observations <- observations %>%
      dplyr::mutate(y = rt)
  }

  plot <- ggplot2::ggplot(forecast, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = bottom), alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = top), alpha =  0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = bottom, ymax = top), alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::geom_point(data = observations,
                        ggplot2::aes(y = y), size = 1.1,
                        alpha = ifelse(rlang::has_name(observations, "sample"),
                                       max(1 / max(observations$sample, na.rm = TRUE), 0.01),
                                       0.7 )) +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

  if (case_plot) {
    plot <- plot + ggplot2::labs(x = "Date", y = "New daily cases")
  } else {
    plot <- plot + ggplot2::labs(x = "Date", y = "Effective Reproduction no.")
  }

  return(plot)
}



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


#' Plot forecast scores
#'
#' @return A dataframe of summarised scores in a tidy format.
#' @export
#' @examples
#'
plot_scores <- function() {

  ##  Some thought required here as to what the best - most general purpose scoring plot would be.
}
