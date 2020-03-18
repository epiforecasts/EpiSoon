#' Calculate Infectiousness
#'
#' @param cases A dataframe containing `date` and `cases` variables
#' @param prediction_date A character string date (format "yyyy-mm-dd") indicating
#' the prediction date.
#' @param horizon Numeric, the time horizon of the forecast.
#'
#' @inheritParams weight_cases_x_days_ago
#' @return The infectiousness at the current time
#' @export
#' @importFrom lubridate days
#' @importFrom dplyr filter
#' @examples
#'
#' cases <- data.frame(date = seq(as.Date("2020-01-01"),
#'                                as.Date("2020-01-10"),
#'                                by = "days"),
#'                     cases = 1:10)
#'
#' infectiousness(cases, prediction_date = "2020-01-10", horizon = 3,
#'                mean_si = 4.7, sd_si = 2.9)
infectiousness <- function(cases, prediction_date = NULL,
                           horizon = NULL, mean_si = NULL,
                           sd_si = NULL) {


  ## Find the last date of observed data used for forecast
  date_to <- as.Date(prediction_date) - lubridate::days(horizon)

  ## Filter cases based on this
  cases <- dplyr::filter(cases, date <= as.Date(date_to))

 infectiousness <- sum(cases$cases * weight_cases_x_days_ago(nrow(cases),
                                                           mean_si = mean_si,
                                                           sd_si = sd_si))

  return(infectiousness)
}
