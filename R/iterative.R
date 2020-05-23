#' Iteratively forecast using a BSTS model
#'
#' @param min_points Numeric, defaults to 3. The minimum number of time points at which to begin
#' iteratively evaluating the forecast.
#' @return
#' @export
#' @inheritParams forecast_rt
#'
#' @importFrom purrr map_dfr safely
#' @importFrom dplyr filter
#' @examples
#'
#'
#' iterative_rt_forecast(EpiSoon::example_obs_rts,
#'                       model = function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                       horizon = 7, samples = 10, min_points = 4)
iterative_rt_forecast <- function(rts,
                                  model = NULL,
                                  horizon = 7,
                                  samples = 1000,
                                  timeout = 30,
                                  bound_rt = TRUE,
                                  min_points = 3) {

  safe_fit <- purrr::safely(forecast_rt)

  ## Dates to iterate over - remove first three to allow enough data for modelling
  dates <- rts$date[-c(1:min_points)]
  names(dates) <- rts$date[-c(1:min_points)]

  ## Iteratively fit
  samples <- purrr::map_dfr(dates, ~ safe_fit(dplyr::filter(rts, date <= .),
                                              model = model,
                                              samples = samples,
                                              horizon = horizon,
                                              bound_rt = bound_rt,
                                              timeout = timeout)[[1]],
                            .id = "forecast_date")



  return(samples)
}



#' Iteratively forecast cases using a iterative Rt forecasts
#'
#' @param it_fit_samples Dataframe of iterative forecasts as produced by `iterative_rt_forecast`.
#' @return A dataframe of iterative case forecasts
#' @export
#' @inheritParams forecast_cases
#'
#' @importFrom purrr map_dfr safely
#' @importFrom dplyr filter group_split
#' @examples
#'
#' ## Iterative Rt forecast
#' it_forecast <-
#'   iterative_rt_forecast(EpiSoon::example_obs_rts,
#'                         model = function(...){EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                         horizon = 7, samples = 10)
#'
#'
#' ## Iterative case forecast
#' iterative_case_forecast(it_fit_samples = it_forecast,
#'                          cases = EpiSoon::example_obs_cases,
#'                          serial_interval = EpiSoon::example_serial_interval)
iterative_case_forecast <- function(it_fit_samples = NULL, cases = NULL,
                                    serial_interval, rdist = NULL) {

  predictions <- it_fit_samples %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(it_fit_samples$forecast_date)) %>%
    purrr::map_dfr( ~ forecast_cases(
      cases,
      fit_samples = dplyr::select(., -forecast_date),
      rdist = rdist,
      serial_interval = serial_interval,
      forecast_date = .$forecast_date[1]
    ), .id = "forecast_date")

  return(predictions)

}




#' Iteratively Forecast Cases Directly
#'
#' @param min_points Numeric, defaults to 3. The minimum number of time points at which to begin
#' iteratively evaluating the forecast.
#' @return
#' @export
#' @inheritParams forecast_cases_firect
#'
#' @importFrom purrr map_dfr safely
#' @importFrom dplyr filter
#' @examples
#'
#'
#' iterative_case_forecast_direct(EpiSoon::example_obs_cases,
#'                                model = function(...) {EpiSoon::bsts_model(model =
#'                                   function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                                horizon = 7, samples = 10, min_points = 4)
#'

iterative_case_forecast_direct <- function(cases,
                                           model = NULL,
                                           horizon = 7,
                                           samples = 1000,
                                           timeout = 30,
                                           bound_cases = TRUE,
                                           min_points = 3) {

  safe_fit <- purrr::safely(forecast_cases_direct)

  ## Dates to iterate over - remove first three to allow enough data for modelling
  dates <- cases$date[-c(1:min_points)]
  names(dates) <- cases$date[-c(1:min_points)]

  ## Iteratively fit
  samples <- purrr::map_dfr(dates, ~ safe_fit(dplyr::filter(cases, date <= .),
                                              model = model,
                                              samples = samples,
                                              horizon = horizon,
                                              bound_cases = bound_cases,
                                              timeout = timeout)[[1]],
                            .id = "forecast_date")



  return(samples)
}


