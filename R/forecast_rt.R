#' Fit and Forecast using a BSTS Model
#'
#' @param rts A dataframe of containing two variables `rt` and  `date` with
#' `rt` being numeric and `date` being a date.
#' @param model A model object in the format of `bsts_model` or `fable_model`. See the corresponding
#' help files for details.
#' @param bound_rt Logical, defaults to `TRUE`. Should Rt values be bounded to be greater than or
#' equal to 0.
#' @param timeout Numeric, timeout of model fitting in seconds. Defaults to 30 seconds.
#' @return A dataframe of samples containing the following variables:
#'  `sample`, `date`, `rt`, and `horizon`.
#'@inheritParams bsts_model
#' @importFrom lubridate days
#' @importFrom dplyr mutate n group_by ungroup
#' @importFrom tidyr gather
#' @importFrom R.utils withTimeout
#' @export
#'
#' @examples
#' forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'             model = function(...){
#'             EpiSoon::bsts_model(model = function(ss, y){
#'             bsts::AddAutoAr(ss, y = y, lags = 10)}, ...)
#'             },
#'             horizon = 7, samples = 10)
forecast_rt <- function(rts, model = model,
                      horizon = 7, samples = 1000,
                      bound_rt = TRUE, timeout = 30) {

  ## Set up for model fitting
  y <- rts$rt

  ## Forecast and return samples
  samples <- R.utils::withTimeout(
    model(y = y, samples = samples, horizon = horizon),
    timeout = timeout, onTimeout = "error"
  )

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
