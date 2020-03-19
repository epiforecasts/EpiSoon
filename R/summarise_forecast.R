#' Summarise Forecast Rts
#'
#' @param fit_samples A dataframe as produced by `EpiSoon::fit_model`.
#'
#' @return A summarised dataframe.
#' @export
#'
#' @importFrom dplyr group_by summarise
#' @examples
#'
#' rts <- data.frame(rt = 1:10,
#'                  date = as.Date("2020-01-01") + lubridate::days(1:10))
#'
#'
#' samples <- fit_model(rts, model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                      horizon = 7, samples = 10)
#'
#'
#' summarise_forecast(samples)
summarise_forecast <- function(fit_samples) {

  summarised_fit <- fit_samples %>%
    dplyr::group_by(date, horizon) %>%
    dplyr::summarise(
      bottom = quantile(rt, 0.025, na.rm = TRUE),
      lower =  quantile(rt, 0.25, na.rm = TRUE),
      median =  median(rt, na.rm = TRUE),
      mean = mean(rt, na.rm = TRUE),
      upper = quantile(rt, 0.75, na.rm = TRUE),
      top = quantile(rt, 0.975, na.rm = TRUE),
      sd = sd(rt, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  return(summarised_fit)
}
