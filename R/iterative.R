#' Iteratively Forecast
#'
#' @param min_points Numeric, defaults to 3. The minimum number of time points at which to begin
#' iteratively evaluating the forecast.
#' @return A tibble of iterative forecasts
#' @export
#' @inheritParams forecast_rt
#'
#' @importFrom purrr map_dfr safely
#' @examples
#' \dontrun{
#' iterative_rt_forecast(EpiSoon::example_obs_rts,
#'   model = function(...) {
#'     EpiSoon::bsts_model(
#'       model =
#'         function(ss, y) {
#'           bsts::AddSemilocalLinearTrend(ss, y = y)
#'         }, ...
#'     )
#'   },
#'   horizon = 7, samples = 10, min_points = 4
#' ) -> tmp
#' }
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
  samples <- purrr::map_dfr(dates, ~ safe_fit(rts[rts$date <= ., ],
    model = model,
    samples = samples,
    horizon = horizon,
    bound_rt = bound_rt,
    timeout = timeout
  )[[1]],
  .id = "forecast_date"
  )



  return(samples)
}



#' Iteratively Forecast Cases Using an Iterative Rt Forecast
#'
#' @param it_fit_samples Dataframe of iterative forecasts as produced by `iterative_rt_forecast`.
#' @return A dataframe of iterative case forecasts
#' @export
#' @inheritParams forecast_cases
#'
#' @importFrom purrr map_dfr safely
#' @importFrom dplyr filter group_split
#' @examples
#' \dontrun{
#' ## Iterative Rt forecast
#' it_forecast <-
#'   iterative_rt_forecast(EpiSoon::example_obs_rts,
#'     model = function(...) {
#'       EpiSoon::bsts_model(
#'         model =
#'           function(ss, y) {
#'             bsts::AddSemilocalLinearTrend(ss, y = y)
#'           }, ...
#'       )
#'     },
#'     horizon = 7, samples = 10
#'   )
#'
#'
#' ## Iterative case forecast
#' iterative_case_forecast(
#'   it_fit_samples = it_forecast,
#'   cases = EpiSoon::example_obs_cases,
#'   serial_interval = EpiSoon::example_serial_interval
#' )
#' }
iterative_case_forecast <- function(it_fit_samples = NULL, cases = NULL,
                                    serial_interval, rdist = NULL) {
  predictions <- it_fit_samples %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(it_fit_samples$forecast_date)) %>%
    purrr::map_dfr(~ forecast_cases(
      cases,
      fit_samples = dplyr::select(., -forecast_date),
      rdist = rdist,
      serial_interval = serial_interval,
      forecast_date = .$forecast_date[1]
    ), .id = "forecast_date")

  return(predictions)
}


#' Iteratively forecast directly on cases
#'
#' @return A tibble of iterative forecasts
#' @export
#' @inheritParams forecast_cases_directly
#' @inheritParams iterative_rt_forecast
#'
#' @examples
#' \dontrun{
#' iterative_direct_case_forecast(EpiSoon::example_obs_cases,
#'   model = function(...) {
#'     EpiSoon::bsts_model(
#'       model =
#'         function(ss, y) {
#'           bsts::AddSemilocalLinearTrend(ss, y = y)
#'         }, ...
#'     )
#'   },
#'   horizon = 7, samples = 10, min_points = 4
#' )
#' }
iterative_direct_case_forecast <- function(cases,
                                           model = NULL,
                                           horizon = 7,
                                           samples = 1000,
                                           timeout = 30,
                                           bound_rt = TRUE,
                                           min_points = 3) {
  cases$rt <- cases$cases
  cases$cases <- NULL

  samples <- iterative_rt_forecast(
    rts = cases,
    model = model,
    horizon = horizon,
    samples = samples,
    timeout = timeout,
    bound_rt = bound_rt,
    min_points = min_points
  )

  samples$cases <- samples$rt
  samples$rt <- NULL

  return(samples)
}
