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
#' purrr::map_dfr(1:100, ~ predict_current_cases(cases = EpiSoon::example_obs_cases,
#'                       rts = EpiSoon::example_obs_rts,
#'                       serial_interval = EpiSoon::example_serial_interval)) %>%
#'                       dplyr::group_by(date) %>%
#'                       dplyr::summarise(cases = mean(cases))
predict_current_cases <- function(
  cases = NULL,
  rts = NULL,
  serial_interval = NULL,
  rdist = NULL) {

  ## Set sampling dist
  if (is.null(rdist)) {
    rdist <- rpois
  }

  ## Get first and last date
  first_rt_date <- min(rts$date, na.rm = TRUE)
  final_rt_date <- max(rts$date, na.rm = TRUE)

  ## Get early cases
  early_cases <- dplyr::filter(cases, date <= first_rt_date)$cases

  ## Get following cases
  subsequent_cases <- dplyr::filter(cases,
                                    date > first_rt_date,
                                    date <= final_rt_date)$cases

  ## Build an iterative list of cases
  cases <- purrr::map(1:length(subsequent_cases), ~ c(early_cases, subsequent_cases[1:.]))
  cases <- c(list(early_cases), cases)


  predictions <-
    dplyr::mutate(rts,
      infectiousness =
        purrr::map_dbl(cases,
                       function(cases_vect) {
                         inf <- sum(cases_vect * EpiSoon::draw_from_si_prob(
                                                          length(cases_vect):1,
                                                          serial_interval
                                                          ))

                         return(inf)
                       }),
      cases = purrr::map2_dbl(rt, infectiousness, ~ rdist(1, .x * .y))
    )

  return(predictions)

}
