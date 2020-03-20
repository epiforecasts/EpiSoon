#' Plot forecast scores
#'
#' @return A dataframe of summarised scores in a tidy format.
#' @export
#' @examples
#'
#' ## Dummy data
#' observations <- data.frame(rt = 1:20,
#'                             date = as.Date("2020-01-01")
#'                            + lubridate::days(1:20))
#'
#' observations <- observations %>%
#'     dplyr::mutate(timeseries = "Region 1") %>%
#'     dplyr::bind_rows(observations %>%
#'     dplyr::mutate(timeseries = "Region 2"))
#'
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("Sparse AR" =
#'                     function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
#'                "Semi-local linear trend" =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
#'
#'
#'
#' ## Compare models
#' evaluations <- compare_timeseries(observations, models,
#'                                   horizon = 7, samples = 10)
#'
#'
#' scores <- evaluations$scores
#'
#'
#' ## Score across the default groups
#' summarise_scores(scores)
#'
#'
#' ## Also summarise across time horizon
#' summarise_scores(scores, "horizon")
#'
#' ## Instead summarise across region
#' summarise_scores(scores, "timeseries")
#'
plot_scores <- function() {

  ##  Some thought required here as to what the best - most general purpose scoring plot would be.
}
