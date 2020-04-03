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
#'                         model = function(...) {EpiSoon::bsts_model(model =
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
