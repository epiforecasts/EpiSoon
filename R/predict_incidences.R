#' Predict incidence from Rts
#'
#' @param rts A dataframe containing two variables: `rt` and `date`
#' @param days_ahead Numeric the number of days ahead
#'
#' @return
#' @export
#'
#' @examples
#'
#' incidences <- data.frame(inc = 1:10,
#'                   date = as.Date("2020-01-01") + lubridate::days(1:10))
#' rts <- data.frame(rt = 1:15,
#'                   date = as.Date("2020-01-01") + lubridate::days(1:15))
#'
#' ## Code
#' predict_incidences
#'


date <- as.Date(max(rts$date))
date <- as.Date(max(incidences$date)) + lubridate::days(1)

# predict_incidences(rts = rts, incidences = incidences, date = date)

# predict_incidences <- function(rts = NULL, incidences = NULL,
#                                days_ahead = NULL, date) {

  last_obs_date <- as.Date(max(incidences$date))
  days_ahead <- as.numeric(date - last_obs_date)

  distances <- (nrow(incidences) + days_ahead - 1):days_ahead
  weights <- sapply(distances, weight_cases_x_days_ago, mean_si = 4.7, sd_si = 2.7)

  infectiousness_from_data <- sum(incidences$inc * weights)

  if (days_ahead == 1) {
    infectiousness_from_predicted <- 0
    infectiousness <- infectiousness_from_data + infectiousness_from_predicted
    predicted_incidence <- infectiousness * rts$rt[rts$date == date]
    predictions <- data.frame(date = date, predicted_inc = predicted_incidence)
  } else {

    pred_inc <-  data.frame(date = NULL, predicted_inc = NULL)
    for (day in 1:days_ahead) {
      weights <- sapply(day:1, weight_cases_x_days_ago, mean_si = 4.7, sd_si = 2.7)
      if (day == 1) {
        infectiousness_from_predicted <- 0
      } else {
        infectiousness_from_predicted <- weights * pred_inc$predicted_inc
      }
      current_date <- date - lubridate::days(days_ahead - day)
      infectiousness <- infectiousness_from_data + infectiousness_from_predicted
      pred <- data.frame(date = current_date, predicted_inc = infectiousness * weights)
      pred_inc <- rbind(pred_inc, pred)
    }

  }
day = 1




 #
 #
 # For every region:
 #      For all the different day_ahead_predictions separately, starting with 1 day ahead forecasts:
 #          For every date t for which we actually have predictions (here brackets only show dimensions):
 #              infectiousness_t[n,m] = infectiousness_from_observed_data[1,1] + infectiousness_from_predicted_incidences[n,m]
 #              predicted_incidences_t[n,m] = current_predicted_r_t[n,m] * infectiousness_t[n,m]
 #
 #            --> put that all in a data.frame that has all 1 day ahead predictions for all available dates.
 #
 #              (Example for 1 day ahead forecast and 10 actually observed incidences:
 #               last data is available on 10., predictions are available for days 2:11.
 #               For (current_date in 2:11) {
 #                  infectiousness_from_data = sum(weight_case_x_days_ago((current_date-1):1) * observed incidences[1:current_date])
 #                  infectiousness_t = infectiousness_from_observed_data
 #                  predicted_incidences_t = current_predicted_r_t * infectiousness_t)
 #               } --> rbind to data.frame that has all 1 day ahead predictions for all available dates.
 #
 #
 #            (Example for 2 days ahead. last data is on 10, predictions are available for 3:12
 #              For (current_date in 3:12) {
 #                  infectiousness_from_data_t = sum(weight_case_x_days_ago(12:3) * observed incidences)
 #                  infectiousness_from_predicted_incidences_t = weight_case_x_days_ago(1) * predicted_incidences_{t-1}
 #                  infectiousness_t = infectiousness_from_observed_data + infectiousness_from_predicted_incidences
 #                  predicted_incidences_t = current_predicted_r_t * infectiousness_t))
 #
 #            (in general: example for d days ahead. predictions available for (1+d):(10+d)
 #              for (current_date in (1+d):(10+d)) {
 #                  infectiousness_from_data_t = sum(weight_case_x_days_ago((10+d):(1+d)) * observed incidences[(1+d):(10+d)])
 #
 #                  infectiousness_from_predicted_incidences_t <-0
 #                  if (d==1){} else {
 #                    for (days_away in (d-1):1) {
 #                      infectiousness_from_predicted_incidences_t += weight_case_x_days_ago(days_away) * predicted_incidences_{t-days_away}
 #                    }
 #                  }
 #                rest as above
 #
 #              } --> collect as one data.frame



  return(incidences)
}


