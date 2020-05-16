#' Fit and Forecast using a Model
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
                        bound_rt = TRUE, timeout = 100) {

  ## Set up for model fitting
  y <- rts$rt

  ## Forecast and return samples
  samples <- R.utils::withTimeout(
    model(y = y, samples = samples, horizon = horizon),
    timeout = timeout, onTimeout = "error"
  )

  colnames(samples) <- max(rts$date) + lubridate::days(1:horizon)

  samples <-
    dplyr::mutate(samples,
                  sample = 1:dplyr::n()) %>%
    tidyr::gather(key = "date", value = "rt", -sample) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::group_by(sample) %>%
    dplyr::mutate(horizon = 1:dplyr::n(),
                  rt = ifelse(rt < 0 & bound_rt, 0, rt)) %>%
    dplyr::ungroup()

  return(samples)
}

#' Forecasts cases for a Rt forecasts
#'
#' @param cases A dataframe containing `date` and `cases` variables
#' @param forecast_date A character string date (format "yyyy-mm-dd") indicating
#' the forecast date. Defaults to `NULL` in which case it will be assumed that the
#' forecast date is the  day before the first date present in the `fit_samples` dataframe
#' @param rdist A function to be used to sample the number of cases. Must take two
#' arguments with the first specfying the number of samples and the second the mean. Defaults
#' to `rpois` if not supplied
#' @inheritParams forecast_rt
#' @inheritParams summarise_forecast
#' @inheritParams predict_cases
#' @return Forecast cases for over a future forecast horizon
#' @export
#' @importFrom dplyr mutate group_split
#' @importFrom purrr map_dfr
#' @importFrom lubridate days
#' @examples
#' ## Rt forecast
#' forecast <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                         model = function(...){
#'                         EpiSoon::bsts_model(model = function(ss, y){
#'                         bsts::AddAutoAr(ss, y = y, lags = 10)}, ...)
#'                         },
#'                         horizon = 7, samples = 10)
#'
#'
#' forecast_cases(EpiSoon::example_obs_cases,
#'                fit_samples =  forecast,
#'                serial_interval = EpiSoon::example_serial_interval)
forecast_cases <- function(cases = NULL, fit_samples = NULL,
                           serial_interval = NULL, forecast_date = NULL,
                           horizon = NULL, rdist = NULL
){

  if(is.null(serial_interval)) {
    stop("serial_interval is missing. See EpiSoon::example_serial_interval for the required format.")
  }
  predictions <-
    dplyr::group_split(fit_samples, sample) %>%
    purrr::map_dfr(~ predict_cases(
      cases = cases,
      rts = .,
      serial_interval = serial_interval,
      forecast_date = min(.$date, na.rm = TRUE) - lubridate::days(1),
      horizon = horizon,
      rdist = rdist) %>%
        dplyr::mutate(horizon = 1:dplyr::n()), .id = "sample") %>%
    dplyr::mutate(sample = as.numeric(sample))


  return(predictions)
}


