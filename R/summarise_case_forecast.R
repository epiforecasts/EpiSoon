#' Summarise Forecast Cases
#'
#' @param fit_samples A dataframe as produced by `EpiSoon::forecast_cases`.
#'
#' @return A summarised dataframe.
#' @export
#'
#' @importFrom dplyr group_by summarise mutate select
#' @importFrom HDInterval hdi
#' @examples
#'
#' ## Observed cases
#' cases <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-10"),
#'                                by = "days"),
#'                     cases = 1:10)
#'
#' ## Forecast Rts
#' rts <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-10"),
#'                                by = "days"),
#'                    rt = rnorm(10, 1.2, 0.01))
#'
#'
#' forecast <- forecast_rt(rts,
#'                         model = function(ss, y) {
#'                         bsts::AddAutoAr(ss, y = y, lags = 7)
#'                         },
#'                         horizon = 7, samples = 10)
#'
#'
#' case_forecast <- forecast_cases(cases, forecast, EpiSoon::example_serial_interval)
#'
#' summarise_case_forecast(case_forecast)
summarise_case_forecast <- function(pred_cases) {


  pred_cases <- pred_cases %>%
    dplyr::rename(rt = cases)

  sum_cases <- summarise_forecast(pred_cases)

  return(sum_cases)
}
