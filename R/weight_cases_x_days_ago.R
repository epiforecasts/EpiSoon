#' Weight cases x days ago
#'
#' @param num_days_ago Numeric of days in the past. Defaults to `NULL`.
#' @param mean_si Numeric, the mean of the serial interval
#' @param sd_si Numeric, the standard deviation of the serial interval
#'
#' @return A weighting for cases x days ago (numeric)
#' @export
#'
#' @examples
#'
#'
#' weight_cases_x_days_ago(10, 4.7, 2.9)
weight_cases_x_days_ago <- function(num_days_ago = NULL,
                                   mean_si = NULL,
                                   sd_si = NULL) {


  mu_log <- log(mean_si) - 1/2 * log((sd_si / mean_si)^2 + 1)
  sd_log <- sqrt(log((sd_si/mean_si)^2 + 1) )

  return(plnorm(num_days_ago + 0.5, mu_log, sd_log) -
           plnorm(num_days_ago - 0.5, mu_log, sd_log))

}
