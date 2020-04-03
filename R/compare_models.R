#' Compare forecasting models
#'
#' @param models A list of models. A configuration is given in the
#' examples. Each model needs to be wrapped in a function that takes a `...` argument and returns a dataframe
#' of samples with each column representing a time horizon.
#' Example: `function(...) {EpiSoon::bsts_model(model = function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)}`.
#'
#' @inheritParams evaluate_model
#' @return A list of dataframes as produced by `evaluate model` but with an additional model column.
#' @export
#' @importFrom purrr transpose safely
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows
#' @examples
#'
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("AR 3" =
#'                     function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)},
#'                "Semi-local linear trend" =
#'                function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)})
#'
#'
#'
#' ## Compare models
#' evaluations <- compare_models(EpiSoon::example_obs_rts,
#'                               EpiSoon::example_obs_cases, models,
#'                               horizon = 7, samples = 10,
#'                               serial_interval = example_serial_interval)
#'
#' ## Example evaluation plot for comparing forecasts
#' ## with actuals for a range of models and time horizons.
#' plot_forecast_evaluation(evaluations$forecast_rts, EpiSoon::example_obs_rts, c(1, 3, 7)) +
#'   ggplot2::facet_grid(model ~ horizon) +
#'   cowplot::panel_border()
#'
#' ## Hack to plot observed cases vs predicted
#' plot_forecast_evaluation(evaluations$forecast_cases,
#'                          EpiSoon::example_obs_cases, c(1, 3, 7)) +
#'   ggplot2::facet_wrap(model ~ horizon, scales = "free") +
#'   cowplot::panel_border()
 compare_models <- function(obs_rts = NULL,
                            obs_cases = NULL,
                            models = NULL,
                            horizon = 7, samples = 1000,
                            bound_rt = TRUE, timeout = 30,
                            serial_interval = NULL,
                            min_points = 3,
                            rdist = NULL,
                            return_raw = FALSE) {


   safe_eval <- purrr::safely(evaluate_model)

   ## Evaluate each model (potential to swap in furrr here)
   evaluations <- models %>%
     furrr::future_map(
       ~ safe_eval(obs_rts,
                   obs_cases,
                   model = .,
                   horizon = horizon,
                   samples = samples,
                   bound_rt = bound_rt,
                   timeout = timeout,
                   serial_interval = serial_interval,
                   min_points = min_points,
                   rdist = rdist,
                   return_raw = return_raw)[[1]],
       .progress = TRUE
    ) %>%
     purrr::transpose() %>%
     purrr::map(~ dplyr::bind_rows(., .id = "model"))


   return(evaluations)

   }
