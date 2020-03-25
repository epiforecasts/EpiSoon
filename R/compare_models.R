#' Compare forecasting models
#'
#' @param models A list of `bsts` models. An example configuration is given in the
#' examples. Each `bsts` model needs to be wrapped in a function that takes a `ss` and `y`
#' argument (i.e. `function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}`).
#'
#' @inheritParams evaluate_model
#' @return A list of dataframes as produced by `evaluate model` but with an additional model column.
#' @export
#' @importFrom purrr map transpose safely
#' @importFrom dplyr bind_rows
#' @examples
#'
#' ## Dummy data
#' observations <- data.frame(rt = 1:20,
#'                             date = as.Date("2020-01-01")
#'                            + lubridate::days(1:20))
#'
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("Sparse AR" =
#'                     function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
#'                "Semi-local linear trend" =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
#'
#'
#'
#' ## Compare models
#' evaluations <- compare_models(observations, models, horizon = 7, samples = 10)
#'
#' ## Example evaluation plot for comparing forecasts
#' ## with actuals for a range of models and time horizons.
#' plot_forecast_evaluation(evaluations$forecast, observations, c(1, 3, 7)) +
#'   ggplot2::facet_grid(model ~ horizon) +
#'   cowplot::panel_border()
#'
 compare_models <- function(observations = NULL, models = NULL,
                            horizon = 7, samples = 1000,
                            bound_rt = TRUE, timeout = 30) {


    safe_eval <- purrr::safely(evaluate_model)

   ## Evaluate each model (potential to swap in furrr here)
   evaluations <- models %>%
     purrr::map(
       ~ safe_eval(observations,
                        model = .,
                        horizon = horizon,
                        samples = samples,
                        bound_rt = bound_rt,
                        timeout = timeout)[[1]]
    ) %>%
     purrr::transpose() %>%
     purrr::map(~ dplyr::bind_rows(., .id = "model"))


   return(evaluations)

   }
