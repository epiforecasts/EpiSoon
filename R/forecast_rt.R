#' Fit and Forecast using a BSTS Model
#'
#' @param rts A dataframe of containing two variables `rt` and  `date` with
#' `rt` being numeric and `date` being a date.
#' @param model An uninitialised `bsts` model
#' @param horizon Numeric, the time horizon over which to predict
#' @param samples Numeric, the number of samples to from the posterior of the model
#' fit. Note that twice this number of MCMC steps will be taken with half used as burn in.
#' @param bound_rt Logical, defaults to `TRUE`. Should Rt values be bounded to be greater than or
#' equal to 0.
#' @param timeout Numeric, the number of seconds to allow before terminating model fitting. Defaults to
#' 30 seconds.
#' @return A dataframe of samples containing the following variables:
#'  `sample`, `date`, `rt`, and `horizon`.
#' @importFrom bsts bsts predict.bsts
#' @importFrom lubridate days
#' @importFrom dplyr mutate n group_by ungroup
#' @importFrom tidyr gather
#' @importFrom R.utils withTimeout
#' @export
#'
#' @examples
#' forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'             model = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 10)},
#'             horizon = 7, samples = 10)
forecast_rt <- function(rts, model = model,
                      horizon = 7, samples = 1000,
                      bound_rt = TRUE, timeout = 30) {

  ## Set up for model fitting
  y <- rts$rt

  model <- model(list(), y)

  ## Fit the model
  fitted_model <- R.utils::withTimeout(
    bsts::bsts(y,
               state.specification = model,
               niter = ifelse(samples < 100, 100 + samples, samples * 2),
               ping = 0),
    timeout = timeout, onTimeout = "error"
  )


  ## Predict using the model
  prediction <- bsts::predict.bsts(fitted_model, horizon = horizon,
                                   burn = ifelse(samples < 100, 100, samples),
                                   quantiles = c(.025, .975))

  ## Extract samples and tidy format
  samples <- as.data.frame(prediction$distribution)

  colnames(samples) <- max(rts$date) + lubridate::days(1:horizon)

  samples <- samples %>%
    dplyr::mutate(sample = 1:dplyr::n()) %>%
    tidyr::gather(key = "date", value = "rt", -sample) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::group_by(sample) %>%
    dplyr::mutate(horizon = 1:dplyr::n(),
                  rt = ifelse(rt < 0 & bound_rt, 0, rt)) %>%
    dplyr::ungroup()

  return(samples)
}
