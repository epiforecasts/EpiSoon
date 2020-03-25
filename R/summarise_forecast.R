#' Summarise Forecast Rts
#'
#' @param fit_samples A dataframe as produced by `EpiSoon::forecast`.
#'
#' @return A summarised dataframe.
#' @export
#'
#' @importFrom dplyr group_by summarise mutate select
#' @importFrom purrr map_dbl
#' @importFrom HDInterval hdi
#' @examples
#'
#' rts <- data.frame(rt = 1:10,
#'                  date = as.Date("2020-01-01") + lubridate::days(1:10))
#'
#'
#' samples <- forecast_rt(rts, model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                        horizon = 7, samples = 10)
#'
#'
#' summarise_forecast(samples)
summarise_forecast <- function(fit_samples) {

  summarised_fit <- fit_samples %>%
    dplyr::group_by(date, horizon) %>%
    dplyr::summarise(
      hdi_90 = list(HDInterval::hdi(rt, credMass = 0.9)),
      hdi_50 = list(HDInterval::hdi(rt, credMass = 0.5)),
      median =  median(rt, na.rm = TRUE),
      mean = mean(rt, na.rm = TRUE),
      sd = sd(rt, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      bottom = purrr::map_dbl(hdi_90, ~ .[[1]]),
      lower = purrr::map_dbl(hdi_50, ~ .[[1]]),
      upper = purrr::map_dbl(hdi_50, ~ .[[2]]),
      top = purrr::map_dbl(hdi_90, ~ .[[2]])
    ) %>%
  dplyr::select(-hdi_90, -hdi_50)




  return(summarised_fit)
}
