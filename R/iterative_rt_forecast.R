#' Iteratively forecast using a BSTS model
#'
#'
#' @return
#' @export
#' @inheritParams forecast_rt
#'
#' @importFrom purrr map_dfr safely
#' @importFrom dplyr filter
#' @examples
#'
#' rts <- data.frame(rt = 1:10,
#'                  date = as.Date("2020-01-01") + lubridate::days(1:10))
#'
#'
#' iterative_rt_forecast(rts, model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                     horizon = 7, samples = 10)
iterative_rt_forecast <- function(rts,
                          model = NULL,
                          horizon = 7,
                          samples = 1000,
                          timeout = 30,
                          bound_rt = TRUE) {

  safe_fit <- purrr::safely(forecast_rt)

  ## Dates to iterate over - remove first three to allow enough data for modelling
  dates <- rts$date[-c(1:3)]
  names(dates) <- rts$date[-c(1:3)]

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







