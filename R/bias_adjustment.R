#' Make a simple bias correction based on CRPS
#'
#' @description
#' Provides a wrapper for different EpiSoon models and corrects biases in
#' forecasts by subtracting a constant from following predictions
#'
#' A model is supplied. This model is fit to the data up until a
#' period of observations of size `correction_period`. Forecasts are generated
#' from this model for all time points in the `correction_period`.
#' A constant is subtracted from the predictions so that CRPS is minimised.
#' The model is then
#' refitted for the entire timeseries and predictions are generated. To these
#' predictions the correction_term obtained from the `correction_period` is
#' then subtracted to create adjusted forecasts.
#'
#' @param model A model, analogous to the form
#' `function(...){EpiSoon::fable_model(model = , ...)}` or
#' `function(...){EpiSoon::bsts_model(model = , ...)}`.
#' @inheritParams bsts_model
#' @param correction_period The number of most recent timepoints to find an
#' appropriate bias adjustment term
#' @return A dataframe of predictions (with columns representing the
#'  time horizon and rows representing samples).
#' @export
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_rows n ungroup mutate rename group_by filter
#' @importFrom tidyr pivot_longer pivot_wider
#' @examples
#'
#' y <- EpiSoon::example_obs_rts[1:20, ]$rt
#' model = fable::ARIMA(y ~ time)
#' samples = 10
#' horizon = 7
#' weighting_period = 5
#'
#' # make list with models
#' baseline_ensemble <- function(...) {
#' EpiSoon::fable_model(model = fabletools::combination_model(fable::ARIMA(y), fable::ETS(y), fable::NAIVE(y),
#'                                                            fable::RW(y ~ drift()), cmbn_args = list(weights = "inv_var")), ...)
#' }
#' models <- list(
#'   Mixfabl = baseline_ensemble,
#'   "ARIMA" = function(...){EpiSoon::fable_model(model = fable::ARIMA(y), ...)},
#'   "ETS" = function(...){EpiSoon::fable_model(model = fable::ETS(y), ...)},
#'   "Drift" = function(...){EpiSoon::fable_model(model = fable::RW(y ~ drift()), ...)}
#' )
#'
#' model = function(...){EpiSoon::fable_model(model = fable::ARIMA(y), ...)}
#'
#' # make forecst on its own
#' forecast <- crps_ensemble(y = y,
#'                           models = models,
#'                           samples = 10,
#'                           horizon = 7,
#'                           weighting_period = 5)
#'
#'
#' # together with forecast_rt
#' fc_rt <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'             model = function(...){
#'               crps_ensemble(models = models,
#'                             weighting_period = 5,
#'                             ...)},
#'             samples = 10,
#'             horizon = 7)
#'
#' forecast_eval <- evaluate_model(EpiSoon::example_obs_rts,
#'                EpiSoon::example_obs_cases,
#'                model = function(...){
#'                 crps_ensemble(models = models,
#'                             weighting_period = 5,
#'                             ...)},
#'                horizon = 7, samples = 10,
#'                serial_interval = example_serial_interval,
#'                min_points = 10)
#'
#' plot_forecast_evaluation(forecast_eval$forecast_rts,
#'                          EpiSoon::example_obs_rts,
#'                          horizon_to_plot = 7)
#'
#'


simple_bias_correction <- function(y = NULL,
                                   model = NULL,
                                   samples = NULL,
                                   horizon = NULL,
                                   correction_period = 5) {


  #### Error Handling
  # check if y is there
  if(is.null(y)) stop("parameter y is missing")


  # check if stackr is installed
  if (!suppressWarnings(require("stackr", quietly = TRUE) == TRUE)) {
    stop("package stackr must be installed. You can install it using devtools::install_github('nikosbosse/stackr')")
  }
  if (length(y) <= correction_period) {
    stop("not enough observations to do bias correction. Adjust correction_period")
  }

  #### split data into train data and data for bias correction. Use train data to
  # generate forecasts, score them against weight data, then generate forecasts
  # based on the entire time series and use the obtained correction term
  # to adjust bias

  n <- length(y)
  y_train <- y[1:(n - correction_period)]
  y_weight <- y[(n - correction_period + 1):n]

  #### fit models on train data and generate forecasts
  fc_bias <- model(y = y_train,
                   samples = samples,
                   horizon = correction_period)

  bias_correction <- stackr::bias_adjustment(y_weight, fc_bias, lambda = rep(1, correction_period))

  stackr::bias_adjustment(rnorm(5), replicate(5, rnorm(10)), lambda = rep(1, correction_period))


  #### generate real forecasts and use weights to stack
  fc <- model(y = y,
              samples = samples,
              horizon = correction_period)


  return(fc - bias_correction)
}


