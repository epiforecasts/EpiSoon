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
#'\dontrun{
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
#' }
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
#'\dontrun{
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
#'                          }
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
plot_scores <- function() {

  ##  Some thought required here as to what the best - most general purpose scoring plot would be.
}



#' Summary plots to compare timeseries and forecast models
#'
#' @param compare_timeseries_output A named list of dataframes produced by
#' \code{compare_timeseries}
#' @param type Type(s) of summary plot to be produced for Rt and case
#' observations for each model in n \code{compare_timeseries_output}.
#' \code{"summary_score"} provides a plot of model fit scores for
#' the 0-7 and 8-14 day horizons. If desired, a subset of scores can be
#' specified using the \code{score} argument. \code{"horizon_score"}
#' provides a plot of scores (CRPS, Calibration, Sharpness, Median, IQR,
#' CI, Bias) across horizons. \code{"region_score"} provides a plot of
#' scores (CRPS, Calibration, Sharpness, Bias, Median, IQR) by region for
#' the 0-7 and 8-14 day horizons.
#' @param score (Optional) One or more of
#' \code{c("Bias","CRPS","Sharpness","Calibration","Median","IQR","CI")} when
#' \code{type="summary_score"} is used.
#'
#' @return A named list of `ggplot2` objects
#' @export
#' @importFrom dplyr mutate bind_rows filter group_by ungroup recode_factor
#' @importFrom cowplot plot_grid theme_cowplot panel_border
#' @importFrom purrr map_dfr
#' @importFrom lubridate days
#' @importFrom ggplot2 ggplot labs aes geom_point position_dodge geom_linerange
#' @importFrom ggplot2 theme facet_wrap coord_flip geom_line
#' @importFrom ggplot2 scale_x_continuous scale_fill_viridis_d facet_grid
#' @importFrom ggplot2 scale_y_log10 scale_color_viridis_d aes_string
#'
#' @examples
#' \dontrun{
#' obs_rts <- EpiSoon::example_obs_rts %>%
#'   dplyr::mutate(timeseries = "Region 1") %>%
#'  dplyr::bind_rows(EpiSoon::example_obs_rts %>%
#'   dplyr::mutate(timeseries = "Region 2"))
#'
#' obs_cases <- EpiSoon::example_obs_cases %>%
#'   dplyr::mutate(timeseries = "Region 1") %>%
#'   dplyr::bind_rows(EpiSoon::example_obs_cases %>%
#'   dplyr::mutate(timeseries = "Region 2"))
#'
#' models <- list("AR 3" = function(...){
#'   EpiSoon::bsts_model(model = function(ss, y){
#'     bsts::AddAr(ss, y = y, lags = 3)
#'   }, ...)},
#'   "Semi-local linear trend" = function(...) {
#'     EpiSoon::bsts_model(model = function(ss, y){
#'     bsts::AddSemilocalLinearTrend(ss, y = y)
#'   }, ...)}
#' )
#'
#' forecast_eval <-
#'   compare_timeseries(obs_rts, obs_cases, models,
#'                      horizon = 10, samples = 10,
#'                      serial_interval = EpiSoon::example_serial_interval)
#'
#' ## Produce all plots
#' plot_compare_timeseries(forecast_eval)
#'
#' ## Produce subsets of plots
#' plot_compare_timeseries(forecast_eval, type = "summary_score")
#' plot_compare_timeseries(forecast_eval, type = "summary_score",
#'                        score = "Bias")
#' plot_compare_timeseries(forecast_eval, type = "horizon_score")
#' plot_compare_timeseries(forecast_eval, type = "region_score")
#' plot_compare_timeseries(forecast_eval,
#'                         type = c("horizon_score","region_score")
#' }
plot_compare_timeseries <- function(compare_timeseries_output,
                                    type = c("summary_score",
                                             "horizon_score",
                                             "region_score"),
                                    score = c("Bias",
                                              "CRPS",
                                              "Sharpness",
                                              "Calibration",
                                              "Median",
                                              "IQR",
                                              "CI") ) {
    ## Prepare plotting output
    plot_output <- list()

    ## Pull in results
    rt_scores <- compare_timeseries_output$rt_scores
    case_scores  <- compare_timeseries_output$case_scores

    ## Fix attributes of calibration to remove warnings
    names(rt_scores$calibration) <- NULL
    names(case_scores$calibration) <- NULL

    ## Identify maximum available horizon
    max_horizon <- max(rt_scores$horizon)

    if("summary_score" %in% type) {
      sum_scores <-
        summarise_scores_recent_all(rt_scores, recent = 7) %>%
        dplyr::mutate(type = "Rt") %>%
        dplyr::bind_rows(summarise_scores_recent_all(case_scores,
                                                     recent = 7) %>%
                           dplyr::mutate(type = "Cases")) %>%
        adjust_score(group_var = c("horizon", "type", "data")) %>%
        dplyr::mutate(type = type %>%
                        factor(levels = c("Rt", "Cases")))

      for(score_choice in score) {
        plot_output[[paste0(tolower(score_choice), "_summary_plot")]] <-
           summary_plot(sum_scores, score_choice)
      }
    }
    if("horizon_score" %in% type) {
      plot_rt_horizon_score <-
        EpiSoon::summarise_scores(rt_scores, "horizon") %>%
        adjust_score("horizon") %>%
        make_horizon_score_plot(label = "Rt")

      plot_cases_horizon_score <-
        EpiSoon::summarise_scores(case_scores, "horizon") %>%
        adjust_score("horizon") %>%
        make_horizon_score_plot(label = "Cases")

      ##  plot_horizon_score
      plot_output[["horizon_score_plot"]] <-
        cowplot::plot_grid(plot_rt_horizon_score,
                           plot_cases_horizon_score,
                           ncol = 1)
    }
    if("region_score" %in% type) {
      ## Summarised scores
      scores_0_7 <- list(rt = rt_scores, case = case_scores) %>%
        purrr::map_dfr(
          ~ dplyr::filter(., horizon <= 7) %>%
            EpiSoon::summarise_scores("timeseries"), .id = "type")

      plot_output[["region_score_7_plot"]]  <-
        scores_0_7 %>%
        dplyr::filter(!score %in% "ci") %>%
        make_region_score_plot(title = "Horizon: 0 to 7")

      if(max_horizon > 7) {
        scores_8_plus <- list(rt = rt_scores, case = case_scores) %>%
          purrr::map_dfr(
            ~ dplyr::filter(., horizon > 7) %>%
              EpiSoon::summarise_scores("timeseries"), .id = "type")
        plot_output[["region_score_7_up_plot"]] <-
          scores_8_plus %>%
          dplyr::filter(!score %in% "ci") %>%
          make_region_score_plot(title = "Horizon: 7+") +
          ggplot2::labs(x = "", y = "")
      }
    }
    return(plot_output)
}

## Internal unexported functions for plotting/summary ------------------

adjust_score <- function(df, group_var) {
  df_update <- df %>%
    dplyr::group_by(score, .dots = group_var) %>%
    dplyr::mutate(upper_min = 10 * min(upper)) %>%
    dplyr::ungroup() #%>%
  df_update <-
    df_update[which(df_update$upper <= df_update$upper_min |
                    df_update$score %in% c("bias", "calibration")),] %>%
    # dplyr::filter(upper <= upper_min |
    #                 score %in% c("bias", "calibration")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!score %in% c("logs", "dss")) %>%
    dplyr::mutate(score = score %>%
                    factor(levels = c("crps", "calibration",
                                      "sharpness", "bias",
                                      "median", "iqr", "ci")) %>%
                    dplyr::recode_factor(
                      "crps" = "CRPS",
                      "calibration" = "Calibration",
                      "sharpness" = "Sharpness",
                      "bias" = "Bias",
                      "median" = "Median",
                      "iqr" = "IQR",
                      "ci" = "CI"
                    ))
  return(df_update)
}

## Summarise scores
summarise_scores_by_horizon <- function(scores) {

  score_7 <- scores %>%
    dplyr::filter(horizon <= 7) %>%
    EpiSoon::summarise_scores() %>%
    dplyr::mutate(horizon = "0 -- 7")

  score_14 <- scores %>%
    dplyr::filter(horizon <= 14, horizon > 7) %>%
    EpiSoon::summarise_scores() %>%
    dplyr::mutate(horizon = "8 -- 14")

  # score_14_plus <- scores %>%
  #   dplyr::filter(horizon > 14) %>%
  #   EpiSoon::summarise_scores() %>%
  #   dplyr::mutate(horizon = "14+")
  #

  scores <- score_7 %>%
    dplyr::bind_rows(score_14) %>%
    # dplyr::bind_rows(score_14_plus) %>%
    dplyr::mutate(horizon = horizon %>%
                    factor(levels = c("0 -- 7", "8 -- 14")))

  return(scores)
}

## Summarise by horizon for all and recent data only
summarise_scores_recent_all <- function(scores, recent = NULL) {
  all_scores <- scores %>%
    summarise_scores_by_horizon()

  recent_scores <- scores %>%
    dplyr::filter(date >= max(date) - lubridate::days(recent)) %>%
    summarise_scores_by_horizon()

  sum_scores <- all_scores %>%
    dplyr::mutate(data = "All") %>%
    dplyr::bind_rows(recent_scores %>%
                       dplyr::mutate(data = paste0("Most recent ",
                                                   recent, " days")))
  return(sum_scores)
}

## Region score plot
plot_region_score <- function(scores, label = NULL) {
  scores %>%
    adjust_score("timeseries") %>%
    ggplot2::ggplot(ggplot2::aes(x = timeseries, y = median,
                                 col = model)) +
    ggplot2::geom_point(size = 1.1,
                        position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_point(ggplot2::aes(y = median), shape = 2, size = 1.1,
                        position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper),
                            alpha = 0.4, size = 1.1,
                            position =
                              ggplot2::position_dodge(width = 1)) +
    ggplot2::scale_color_viridis_d(option = "cividis") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(col = "Model", y = "Score value", x = "Region",
                  tag = label) +
    ggplot2::facet_wrap(~ score, scales = "free_x") +
    ggplot2::coord_flip()
}

## Internal plot
plot_internal <- function(df, label = NULL) {
  plot <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = horizon, y = mean, col = model,
                                 group = model)) +
    ggplot2::geom_line(size = 1.2, alpha = 0.6) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_point(ggplot2::aes(y = median), shape = 2, size = 2) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper),
                            alpha = 0.4, size = 1.5,
                            position =
                              ggplot2::position_dodge(width = 3)) +
    ggplot2::scale_color_viridis_d(option = "cividis") +
    ggplot2::scale_x_continuous(breaks = unique(df$horizon)) +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(col = "Model", y = "", x = "Horizon", tag = label) +
    ggplot2::facet_wrap(~ score, scales = "free_y")

  return(plot)
}

# Generate summary plots
summary_plot <- function(scores, target_score) {
  scores %>%
    dplyr::filter(score %in% target_score) %>%
    ggplot2::ggplot(
      ggplot2::aes_string(x = "model", y = "mean", col = "data")) +
    ggplot2::geom_point(size = 2,
                        position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_point(ggplot2::aes(y = median), shape = 2, size = 2,
                        position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper),
                            alpha = 0.4, size = 1.5,
                            position =
                              ggplot2::position_dodge(width = 1)) +
    ggplot2::scale_fill_viridis_d(option = "cividis", begin = 0.2,
                                  end = 0.6) +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(horizon ~ type, scales = "free_x") +
    cowplot::panel_border()  +
    ggplot2::labs(x = "Model", y = target_score, col = "Data")
}

## Summarise scores across time horizons
make_horizon_score_plot <- function(df, label = NULL) {
  df <- df %>%
    dplyr::filter(horizon %% 3 == 0, horizon <= 15)

  score_plot <- df %>%
    dplyr::filter(!score %in% "Bias") %>%
    dplyr::filter(mean > 0, median > 0, lower > 0, upper > 0) %>%
    plot_internal(label = label) +
    ggplot2::scale_y_log10()

  bias_plot <- df %>%
    dplyr::filter(score %in% "Bias") %>%
    plot_internal(label = "") +
    ggplot2::theme(legend.position = "none")

  plot <- cowplot::plot_grid(score_plot, bias_plot,
                             nrow = 1, rel_widths = c(3,1))

  return(plot)
}

## Summarise scores across regions
make_region_score_plot <- function(df, title = NULL) {

  plot_region_score_rt <- df[which(df$type %in% "rt"),] %>%
    # dplyr::filter(type %in% "rt") %>%
    plot_region_score(label = "Rt") +
    ggplot2::labs(title = title, x = "", y = "")

  plot_region_score_cases <-  df[which(df$type %in% "case"),] %>%
    # dplyr::filter(type %in% "case") %>%
    plot_region_score(label = "Cases") +
    ggplot2::theme(legend.position = "none")

  plot_region_score_new <- cowplot::plot_grid(plot_region_score_rt,
                                          plot_region_score_cases,
                                          nrow = 1)
  return(plot_region_score_new)
}
