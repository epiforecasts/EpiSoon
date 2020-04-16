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
#' ## Draw
#' draw_from_si_prob(rev(c(1, 2, 4, 10, 1:100)), EpiSoon::example_serial_interval)
draw_from_si_prob <- function(days_ago = NULL,
                         serial_interval = NULL) {


  var_length <- length(serial_interval)
  if (max(days_ago, na.rm = TRUE) > var_length) {
    serial_interval <- c(serial_interval, rep(0, max(days_ago) - var_length))
  }

  draws <- serial_interval[days_ago]


  return(draws)
}
