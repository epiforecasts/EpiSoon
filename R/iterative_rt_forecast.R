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







