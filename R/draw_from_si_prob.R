#' Draw from the Serial Interval Probability Distribution
#'
#' @param days_ago Numeric vector of days in the past. Defaults to `NULL`.
#' @param serial_interval A numeric vector describing the probability distribution the serial interval.
#' See `EpiNow::covid_serial_interval` for an example of the format.
#'
#' @return A draw from the probability distribution the serial interval.
#' @export
#' @importFrom purrr map_dbl
#' @examples
#'
#' ## Example serial interval
#' mu_log <- log(mean_si) - 1/2 * log((sd_si / mean_si)^2 + 1)
#' sd_log <- sqrt(log((sd_si/mean_si)^2 + 1))
#'
#'
#' serial_interval <- rlnorm(1:100, mu_log, sd_log) %>%
#'    round(0) %>%
#'    table %>%
#'    {. / sum(.)}
#'
#' ## Draw
#' draw_from_si_prob(c(1, 4, 6), serial_interval)
draw_from_si_prob <- function(days_ago = NULL,
                         serial_interval = NULL) {


  draws <- purrr::map_dbl(days_ago, function(day) {
    if (day > length(serial_interval)) {
      draw <- 0
    }else{
      draw <- serial_interval[day]
    }
    return(draw)
  })


  return(draws)
}
