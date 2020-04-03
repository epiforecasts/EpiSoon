#' Fable model wrapper
#'
#' @param model A `fable` model object. For models that use a formula interface time
#' can be accessed using `time`.
#' @inheritParams bsts_model
#' @return A dataframe of predictions (with columns representing the
#'  time horizon and rows representing samples).
#' @export
#' @importFrom tsibble tsibble
#' @importFrom fabletools model forecast
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @examples
#' ## Used on its own
#' fable_model(y = EpiSoon::example_obs_rts[1:10, ]$rt,
#'            model = fable::ARIMA(y ~ time),
#'            samples = 10, horizon = 7)
#'
#'
#'forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'            model = function(...){
#'            fable_model(model = fable::ARIMA(y ~ time), ...)},
#'            horizon = 7, samples = 10)
fable_model <- function(y = NULL, samples = NULL,
                       horizon = NULL, model = NULL) {


## Make input numeric into correct tsibble format
timeseries <- tsibble::tsibble(y = y, time = 1:length(y), index = time)

## Define model with data
model <- fabletools::model(timeseries, model)

## Fit and forecast model
forecast <- fabletools::forecast(model, h = horizon, times = samples)

if (samples == 1) {
  ## If only using a single sample use central estimate
 samples <- t(data.frame(forecast$y))
}else{
  ## Pull out distributions
  dist <- forecast$.distribution

  ## If samples are present pull out
  if (length(dist[[1]]) == 2) {
    samples <- purrr::map(dist, ~ .[[1]][[1]])
  }else {
  ## If dist is present sample from it
    samples <-  purrr::map(dist,
                            ~ rnorm(samples,
                                    mean = .$mean,
                                    sd = .$sd))
  }

  ## Bind samples together
  names(samples) <- 1:length(samples)
  samples <- dplyr::bind_cols(samples)
}

  return(samples)
}
