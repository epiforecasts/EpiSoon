
#' Score a Model Fit
#'
#' @param observations A dataframe of observations against which to score. Should contain a `date` and `rt` column.
#' @param scores Character vector defaulting to "all". Select which scores to return, default is all scores but
#' any subset can be returned.
#' @return A dataframe containing the following scores per forecast timepoint: dss, crps,
#' logs, bias, and sharpness as well as the forecast date and time horizon.
#' @export
#'
#' @importFrom dplyr filter select select_if
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom scoringRules dss_sample crps_sample logs_sample
#' @importFrom scoringutils bias sharpness pit
#' @inheritParams summarise_forecast
#' @examples
#'
#' ## Fit a model (using a subset of observations)
#' samples <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                      model = function(...) {EpiSoon::bsts_model(model =
#'                     function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
#'                      horizon = 7, samples = 10)
#'
#' ## Score the model fit (with observations during the time horizon of the forecast)
#' score_forecast(samples, EpiSoon::example_obs_rts)
#'
#' ## Return just CRPS, bias and sharpness
#' score_forecast(samples, EpiSoon::example_obs_rts, scores = c("crps", "sharpness", "bias"))
#'
#' ## Return just the CRPS
#' score_forecast(samples, EpiSoon::example_obs_rts, scores = "crps")
score_forecast <- function(fit_samples, observations, scores = "all") {

  observations <-
    dplyr::filter(observations,
      date >= min(fit_samples$date),
      date <= max(fit_samples$date)
    )

  fit_samples <-
    dplyr::filter(fit_samples,
      date >= min(observations$date),
      date <= max(observations$date)
    )


  obs <- observations$rt

  samples_matrix <-
    tidyr::spread(fit_samples, key = "sample", value = "rt") %>%
    dplyr::select(-horizon, -date) %>%
    as.matrix

  data_length <- length(observations$date)

  scores <- tibble::tibble(
    date = observations$date,
    horizon = 1:data_length,
    dss = if(any(c("all", "dss") %in% scores)) {
      scoringRules::dss_sample(y = obs, dat = samples_matrix)
      }else{
        NA
        },
    crps = if(any(c("all", "crps") %in% scores)) {
      scoringRules::crps_sample(y = obs, dat = samples_matrix)
    }else{
      NA
    },
    logs = if(any(c("all", "logs") %in% scores)) {
      scoringRules::logs_sample(y = obs, dat = samples_matrix)
    }else{
      NA
    },
    bias = if(any(c("all", "bias") %in% scores)) {
      suppressWarnings(scoringutils::bias(obs, samples_matrix))
    }else{
      NA
    },
    sharpness = if(any(c("all", "sharpness") %in% scores)) {
      suppressWarnings(scoringutils::bias(obs, samples_matrix))
    }else{
      NA
    },
    calibration = if(any(c("all", "calibration") %in% scores)) {
      suppressWarnings(scoringutils::pit(obs, samples_matrix)$calibration)
    }else{
      NA
    })

  scores <- dplyr::select_if(scores, ~ any(!is.na(.)))


  return(scores)
}
