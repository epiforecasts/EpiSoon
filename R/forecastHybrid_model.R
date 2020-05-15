#' BSTS model wrapper
#'
#' @param y Numeric vector of time points to forecast
#' @param samples Numeric, number of samples to take.
#' @param horizon Numeric, the time horizon over which to predict.
#' @return A dataframe of predictions (with columns representing the time horizon and rows representing samples).
#' @export
#' @examples \dontrun{
#'
#' library(forecastHybrid)
#'
#' ## Used on its own
#' forecastHybrid_model(y = EpiSoon::example_obs_rts$rt,
#'                      samples = 1, horizon = 7,
#'                      weights = "cv.errors", windowSize = 7, cvHorizon = 2) -> tmp
#'
#' ## Used for forecasting
#'  forecast_rt(EpiSoon::example_obs_rts,
#'                     model = EpiSoon::forecastHybrid_model,
#'                     horizon = 7, samples = 10)
#'}
forecastHybrid_model <- function(y = NULL, samples = NULL,
                                 horizon = NULL, ...) {


  check_suggests("forecastHybrid")


  ## Fit the model
  fitted_model <- suppressWarnings(forecastHybrid::hybridModel(y, ...))

  ## Predict using the model
  prediction <- forecastHybrid:::forecast.hybridModel(fitted_model, h = horizon)

  ## Extract samples and tidy format
  sample_from_model <- prediction

  if (samples == 1) {
    sample_from_model <- t(as.data.frame(sample_from_model$mean))
    rownames(sample_from_model) <- NULL
  }else{
    sample_from_model <- prediction
  }

  return(sample_from_model)

}
