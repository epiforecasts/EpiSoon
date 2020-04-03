#' BSTS model wrapper
#'
#' @param y Numeric vector of time points to forecast
#' @param samples Numeric, number of samples to take.
#' @param model A `bsts` model object wrapped in a function with an `ss` and `y` argument.
#' @param horizon Numeric, the time horizon over which to predict.
#' @return A dataframe of predictions (with columns representing the time horizon and rows representing samples).
#' @export
#' @importFrom bsts bsts predict.bsts
#' @examples
#'
#'
#'forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'            model = function(...){bsts_model(model = function(ss, y){
#'            bsts::AddAutoAr(ss, y = y, lags = 10)}, ...)},
#'            horizon = 7, samples = 10)
bsts_model <- function(y = NULL, samples = NULL,
                       horizon = NULL, model = NULL) {


  model <- model(list(), y)

  ## Fit the model
  fitted_model <- bsts::bsts(y,
               state.specification = model,
               niter = ifelse(samples < 100, 100 + samples, samples * 2),
               ping = 0)


  ## Predict using the model
  prediction <- bsts::predict.bsts(fitted_model, horizon = horizon,
                                   burn = ifelse(samples < 100, 100, samples),
                                   quantiles = c(.025, .975))

  ## Extract samples and tidy format
  samples <- as.data.frame(prediction$distribution)

  return(samples)






}
