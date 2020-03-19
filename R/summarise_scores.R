#' Summarise model forecasting scores
#'
#' @param scores A dataframe of model scores as produced by `score_model`
#' @param variables A character vector of variables names to group over. By default score type
#' and model is grouped over if present.
#' @return A dataframe of summarised scores in a tidy format.
#' @export
#' @importFrom tidyr gather
#' @importFrom dplyr group_by summarise ungroup
#' @examples
#'
#' ## Dummy data
#' observations <- data.frame(rt = 1:20,
#'                             date = as.Date("2020-01-01")
#'                            + lubridate::days(1:20))
#'
#' timeseries <- list(observations, observations)
#' names(timeseries) <- c("Region 1", "Region 2")
#'
#' ## List of forecasting bsts models wrapped in functions.
#' models <- list("Sparse AR" = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
#'                "Semi-local linear trend" = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
#'
#'
#'
#' ## Compare models
#' evaluations <- compare_timeseries(timeseries, models,
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
#' summarise_scores(scores, "region")
summarise_scores <- function(scores, variables = NULL) {


  default_groups <- "score"

  if (!is.null(scores[["model"]])) {
    default_groups <- c(default_groups, "model")
  }

  summarised_scores <- scores %>%
    tidyr::gather(key = "score", value = "value", dss, crps, logs, bias, sharpness) %>%
    dplyr::group_by(.dots = c(variables, default_groups)) %>%
    dplyr::summarise(
      bottom = quantile(value, 0.025, na.rm = TRUE),
      lower =  quantile(value, 0.25, na.rm = TRUE),
      median =  median(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      upper = quantile(value, 0.75, na.rm = TRUE),
      top = quantile(value, 0.975, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()


  return(summarised_scores)
}
