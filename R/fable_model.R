#' Fable model wrapper
#'
#'
#' @description Provides an interface for models from the `fable` package.
#' Note the `feasts::ARIMA` model requires the `feast` package. If `future`
#' is being used `fable` will require `future.apply` in
#' order to not silently fail.
#'
#' @param model A `fable` model object. For  models that use a formula interface time
#' can be accessed using `time`.
#' @inheritParams bsts_model
#' @return A dataframe of predictions (with columns representing the
#'  time horizon and rows representing samples).
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @examples \dontrun{
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
#'}
fable_model <- function(y = NULL, samples = NULL,
                       horizon = NULL, model = NULL) {

check_suggests("tsibble")
check_suggests("fable")
check_suggests("fabletools")
check_suggests("feasts")
check_suggests("future.apply")

## Make input numeric into correct tsibble format
timeseries <- tsibble::tsibble(y = y, time = 1:length(y), index = time)

## Define model with data
model <- fabletools::model(timeseries, model)

## Fit and forecast model
forecast <- fabletools::forecast(model, h = horizon, times = samples)

if (samples == 1) {
  ## If only using a single sample use central estimate
 samples <- t(data.frame(forecast$y))
 samples <- data.frame(samples)
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
