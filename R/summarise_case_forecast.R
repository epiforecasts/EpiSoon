#' Summarise Forecast Cases
#'
#' @param pred_cases A dataframe as produced by `EpiSoon::forecast_cases`.
#'
#' @return A summarised dataframe.
#' @export
#'
#' @importFrom dplyr group_by summarise mutate select
#' @importFrom HDInterval hdi
#' @examples
#'
#' ## Example forecast
#' forecast <- forecast_rt(EpiSoon::example_obs_rts,
#'                         model = function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                         horizon = 7, samples = 10)
#'
#' ## Forecast cases
#' case_forecast <- forecast_cases(EpiSoon::example_obs_cases,
#'                                 forecast,
#'                                 EpiSoon::example_serial_interval)
#' ## Summarise case forecast
#' summarise_case_forecast(case_forecast)
summarise_case_forecast <- function(pred_cases) {


  pred_cases <- pred_cases %>%
    dplyr::rename(rt = cases)

  sum_cases <- summarise_forecast(pred_cases)

  return(sum_cases)
}
