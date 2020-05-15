#' forecastHybrid model wrapper
#'
#' Allows users to forecast using ensembles from the `forecastHybrid` package. Note that
#' whilst weighted ensembles can be created this is not advised when samples > 1 as currently
#' samples are derived assuming a normal distribution using the upper and lower confidence intervals of the ensemble.
#' These confidence intervals are themselves either based on the unweighted mean of the ensembled
#' models or the maximum/minimum from the candiate models. Note that `forecastHybrid` must be installed for this
#' model wrapper to be functional.
#' @param y Numeric vector of time points to forecast
#' @param samples Numeric, number of samples to take.
#' @param horizon Numeric, the time horizon over which to predict.
#' @param model_params List of parameters to pass to `forecastHybrid::hybridModel`.
#' @param forecast_params List of parameters to pass to `forecastHybrid:::forecast.hybridModel`.
#' @return A dataframe of predictions (with columns representing the time horizon and rows representing samples).
#' @export
#' @importFrom purrr map2
#' @importFrom dplyr bind_cols
#' @examples \dontrun{
#'
#' library(forecastHybrid)
#'
#' ## Used on its own
#' forecastHybrid_model(y = EpiSoon::example_obs_rts$rt,
#'                      samples = 10, horizon = 7)
#'
#'
#'## Used with non-default arguments
#'## Note that with the current sampling from maximal confidence intervals model
#'## Weighting using cross-validation will only have an impact when 1 sample is used.
#'forecastHybrid_model(y = EpiSoon::example_obs_rts$rt,
#'                     samples = 1, horizon = 7,
#'                     model_params = list(cvHorizon = 7, windowSize = 7,
#'                                   rolling = TRUE, models = "zeta"))
#'
#'
#' ## Used for forecasting
#'  forecast_rt(EpiSoon::example_obs_rts,
#'                     model = EpiSoon::forecastHybrid_model,
#'                     horizon = 7, samples = 1)
#'
#'## Used for forcasting with non-default arguments
#'forecast_rt(EpiSoon::example_obs_rts,
#'            model = function(...){EpiSoon::forecastHybrid_model(
#'            model_params = list(models = "zte"),
#'            forecast_params = list(PI.combination = "mean"), ...)
#'            },
#'            horizon = 7, samples = 10)
#'}
forecastHybrid_model <- function(y = NULL, samples = NULL,
                                 horizon = NULL, model_params = NULL,
                                 forecast_params = NULL) {


  check_suggests("forecastHybrid")


  ## Fit the model
  fitted_model <- suppressMessages(
    suppressWarnings(
      do.call(forecastHybrid::hybridModel, c(list(y = y, parallel = FALSE,
                                                  num.cores = 1), model_params))
      )
    )

  ## Predict using the model
  prediction <- do.call(forecastHybrid:::forecast.hybridModel,
                        c(list(object = fitted_model, h = horizon),
                          forecast_params))

  ## Extract samples and tidy format
  sample_from_model <- prediction

  if (samples == 1) {
    sample_from_model <- data.frame(t(as.data.frame(sample_from_model$mean)))
    rownames(sample_from_model) <- NULL
  }else{
    mean <- as.numeric(prediction$mean)
    upper <- prediction$upper[, ncol(prediction$upper)]
    lower <-  prediction$lower[, ncol(prediction$lower)]
    sd <- (upper - lower) / 3.92
    sample_from_model <- purrr::map2(mean, sd,
                                     ~ rnorm(samples, mean = .x,  sd = .y))

    sample_from_model <- dplyr::bind_cols(sample_from_model)
  }

  return(sample_from_model)

}
