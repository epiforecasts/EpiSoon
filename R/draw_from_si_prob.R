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
#' draw_from_si_prob(c(1, 4, 6), EpiSoon::example_serial_interval)
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
