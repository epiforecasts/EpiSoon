#' Fable model wrapper
#'
#'
#' @description Provides an interface for models from the `fable` package. Not the `ARIMA` model
#' requires the `feast` package. If `future` is being used `fable` will require `future.apply` in
#' order to not silently fail.
#'
#' @param model A `fable` model object. For  models that use a formula interface time
#' can be accessed using `time`.
#' @param bias_correction logical. If TRUE, bias will be corrected based on
#' the fitted values. This is only a temporary solution and will be replaced
#' as soon as possible.
#' @inheritParams bsts_model
#' @return A dataframe of predictions (with columns representing the
#'  time horizon and rows representing samples).
#' @export
#' @importFrom tsibble tsibble
#' @importFrom fabletools model forecast
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @examples
#'
#' ## Used on its own
#' fable_model(y = EpiSoon::example_obs_rts[1:10, ]$rt,
#'            model = fable::ARIMA(y ~ time),
#'            samples = 10, horizon = 7)
#'
#' fable_model(y = EpiSoon::example_obs_rts[1:10, ]$rt,
#'            model = fable::ARIMA(y ~ time),
#'            samples = 10, horizon = 7,
#'            bias_correction = TRUE)
#'
#'
#' forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'            model = function(...){
#'            fable_model(model = fable::ARIMA(y ~ time),
#'                        ...)},
#'            horizon = 7, samples = 10)
#'
#' forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'            model = function(...){
#'            fable_model(model = fable::ARIMA(y ~ time),
#'                        bias_correction = TRUE,
#'                        ...)},
#'            horizon = 7, samples = 10)
#'

fable_model <- function(y = NULL, samples = NULL,
                        horizon = NULL, model = NULL,
                        bias_correction = FALSE) {


  ## Make input numeric into correct tsibble format
  timeseries <- tsibble::tsibble(y = y, time = 1:length(y), index = time)

  ## Define model with data
  model <- fabletools::model(timeseries, model)

  ## Fit and forecast model
  forecast <- fabletools::forecast(model, h = horizon, times = samples)

  if (bias_correction) {
    bias_correction <- mean(residuals(model)$.resid,
                            na.rm = TRUE)

  } else {
    bias_correction <- 0
  }

  if (samples == 1) {
    ## If only using a single sample use central estimate
    samples <- t(data.frame(forecast$y))
    samples <- data.frame(samples)
  } else {
    ## Pull out distributions
    dist <- forecast$.distribution

    ## If samples are present pull out
    if (length(dist[[1]]) == 2) {
      samples <- purrr::map(dist, ~ .[[1]][[1]]) - bias_correction
    } else {
      ## If dist is present sample from it
      samples <-  purrr::map(dist,
                             ~ rnorm(samples,
                                     mean = .$mean - bias_correction,
                                     sd = .$sd))
    }

    ## Bind samples together
    names(samples) <- 1:length(samples)
    samples <- dplyr::bind_cols(samples)
  }

  return(samples)
}
