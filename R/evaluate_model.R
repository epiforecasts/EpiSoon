
#' Evaluate a Model for Forecasting Rts
#'
#'
#' @inheritParams score_model
#' @inheritParams iterative_model_fit
#' @return
#' @export
#' @importFrom dplyr slice group_split filter
#' @importFrom purrr map_dfr
#' @examples
#' ## Observed data
#' observations <- data.frame(rt = 1:20,
#'                            date = as.Date("2020-01-01")
#'                                   + lubridate::days(1:20))
#'
#' ## Evaluate a model
#' evaluate_model(observations,
#'                model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                horizon = 7, samples = 10)
evaluate_model <- function(observations = NULL,
                           model = NULL,
                           horizon = 7,
                           samples = 1000,
                           bound_rt = TRUE) {


  samples <- iterative_model_fit(observations, model = model, horizon = horizon,
                                 samples = samples, bound_rt = bound_rt)


  summarised_forecasts <- samples %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(samples$forecast_date)) %>%
    purrr::map_dfr(summarise_forecast, .id = "forecast_date")


  samples <- samples %>%
    dplyr::filter(date <= max(observations$date))

  scored_forecasts <- samples %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(samples$forecast_date)) %>%
    purrr::map_dfr(~ dplyr::select(., -forecast_date) %>%
                     score_model(observations), .id = "forecast_date")


  out <- list(summarised_forecasts, scored_forecasts)
  names(out) <- c("forecasts", "scores")

  return(out)
}
