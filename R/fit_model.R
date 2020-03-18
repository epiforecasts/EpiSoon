#' Fit and Sample From a BSTS Model
#'
#' @param rts A dataframe of containing two variables `rt` and  `date` with
#' `rt` being numeric and `date` being a date.
#' @param model An uninitialised `bsts` model
#' @param horizon Numeric, the time horizon over which to predict
#' @param samples Numeric, the number of samples to from the posterior of the model
#' fit. Note that twice this number of MCMC steps will be taken with half used as burn in.
#'
#' @return A dataframe of samples containing the following variables:
#'  `sample`, `date`, `rt`, and `horizon`.
#' @importFrom bsts bsts predict.bsts
#' @importFrom lubridate days
#' @importFrom dplyr mutate n group_by ungroup
#' @importFrom tidyr gather
#' @export
#'
#' @examples
#'
#' rts <- data.frame(rt = 1:10,
#'                  date = as.Date("2020-01-01") + lubridate::days(1:10))
#'
#'
#' fit_model(rts, model = bsts::AddAutoAr, horizon = 7, samples = 10)
fit_model <- function(rts, model = model,
                      horizon = 7, samples = 1000) {

  ## Set up for model fitting
  y <- rts$rt

  model <- model(list(), y)

  ## Fit the model
  fitted_model <- bsts::bsts(y,
                             state.specification = model,
                             niter = samples * 2,
                             ping=0)


  ## Predict using the model
  prediction <- bsts::predict.bsts(fitted_model, horizon = horizon,
                                   burn = samples,
                                   quantiles = c(.025, .975))

  ## Extract samples and tidy format
  samples <- as.data.frame(prediction$distribution)

  colnames(samples) <- max(rts$date) + lubridate::days(1:horizon)

  samples <- samples %>%
    dplyr::mutate(sample = 1:dplyr::n()) %>%
    tidyr::gather(key = "date", value = "rt", -sample) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::group_by(sample) %>%
    dplyr::mutate(horizon = 1:dplyr::n()) %>%
    dplyr::ungroup()

  return(samples)
}
