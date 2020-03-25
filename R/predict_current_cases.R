#' Predict cases for Rts based on observed data
#'
#' @param cases A dataframe containing `date` and `cases` variables
#' @param rdist A function to be used to sample the number of cases. Must take two
#' arguments with the first specfying the number of samples and the second the mean. Defaults
#' to `rpois` if not supplied
#' @inheritParams predict_cases
#' @inheritParams draw_from_si_prob
#' @return Forecast cases for the current timestep
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_dbl
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
#'                    rt = rep(1.2, 10))
#'
#'
#' predict_current_cases(cases, rts, EpiSoon::example_serial_interval)
predict_current_cases <- function(
  cases = NULL,
  rts = NULL,
  serial_interval = NULL,
  rdist = NULL) {

  ## Set sampling dist
  if (is.null(rdist)) {
    rdist <- rpois
  }

  predictions <- rts %>%
    dplyr::mutate(
      index = 1:dplyr::n(),
      infectiousness =
        purrr::map_dbl(index,
                       ~ sum(cases$cases[1:.] *
                               EpiSoon::draw_from_si_prob(.:1,
                                                          serial_interval))),
      cases = purrr::map2_dbl(rt, infectiousness, ~ rdist(1, .x * .y))
    )

  return(predictions)

}
