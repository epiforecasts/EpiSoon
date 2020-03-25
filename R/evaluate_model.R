
#' Evaluate a Model for Forecasting Rts
#'
#' @param observations A dataframe of observations to forecast with and score
#' against. Should contain a `date` and `rt` column. If multiple samples are included this
#' should be denoted using a numeric `sample` variable.
#' @inheritParams score_forecast
#' @inheritParams iterative_rt_forecast
#' @return
#' @export
#' @importFrom dplyr slice group_split filter
#' @importFrom purrr map_dfr map2 map2_dfr
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
#'
#'
#' ## Samples of observed data
#' sampled_obs <- observations %>%
#'    dplyr::mutate(sample = 1) %>%
#'    dplyr::bind_rows(observations %>%
#'        dplyr::mutate(sample = 2))
#'
#' ## Evaluate a model across samples
#' evaluate_model(sampled_obs,
#'                model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
#'                horizon = 7, samples = 10)
evaluate_model <- function(observations = NULL,
                           model = NULL,
                           horizon = 7,
                           samples = 1000,
                           timeout = 30,
                           bound_rt = TRUE) {


  ## Split observations into a list if present
  if (!is.null(suppressWarnings(observations$sample))) {
    observations <- observations %>%
      dplyr::group_split(sample)
  }else{
    observations <- list(observations)
  }


  ## Iteratively forecast for each time point
  safe_it <- purrr::safely(iterative_rt_forecast)

  samples <- observations %>%
    purrr::map_dfr(
      ~ safe_it(., model = model, horizon = horizon,
                           samples = samples, bound_rt = bound_rt,
                           timeout = timeout)[[1]],
      .id = "obs_sample")


  ## Summarise the forecast
  summarised_forecasts <- samples %>%
    dplyr::group_split(forecast_date) %>%
    setNames(unique(samples$forecast_date)) %>%
    purrr::map_dfr(summarise_forecast, .id = "forecast_date")


  ## Filter the forecasts to be in line with observed data
  samples <- samples %>%
    dplyr::group_split(obs_sample) %>%
    purrr::map(~ dplyr::select(., -obs_sample)) %>%
    purrr::map2(observations,
                ~ dplyr::filter(.x, date <= max(.y$date)))

  ## Score the forecasts
  scored_forecasts <-
    purrr::map2_dfr(samples, observations,
                    function(sample, obs) {
                      dplyr::group_split(sample, forecast_date) %>%
                        setNames(unique(sample$forecast_date)) %>%
                        purrr::map_dfr(~ dplyr::select(., -forecast_date) %>%
                                         score_forecast(obs), .id = "forecast_date")
                    }, .id = "sample")

  if (length(unique(scored_forecasts$sample)) == 1) {
    scored_forecasts <- scored_forecasts %>%
      dplyr::select(-sample)
  }


  ## Return output
  out <- list(summarised_forecasts, scored_forecasts)
  names(out) <- c("forecasts", "scores")

  return(out)
}
