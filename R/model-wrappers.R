#' BSTS model wrapper
#'
#' @param y Numeric vector of time points to forecast
#' @param samples Numeric, number of samples to take.
#' @param model A `bsts` model object wrapped in a function with an `ss` and `y` argument.
#' @param horizon Numeric, the time horizon over which to predict.
#' @return A dataframe of predictions (with columns representing the time horizon and rows representing samples).
#' @export
#' @examples \dontrun{
#'
#' library(bsts)
#'
#' ## Used on its own
#' bsts_model(y = EpiSoon::example_obs_rts[1:10, ]$rt,
#'            model = function(ss, y){
#'            bsts::AddAr(ss, y = y, lags = 2)},
#'            samples = 10, horizon = 7)
#'
#' ## Used for forecasting
#'  forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'                     model = function(...){EpiSoon::bsts_model(model =
#'                       function(ss, y){
#'                         bsts::AddAr(ss, y = y, lags = 3)}, ...)},
#'                     horizon = 7, samples = 10)
#'}
bsts_model <- function(y = NULL, samples = NULL,
                       horizon = NULL, model = NULL) {


  check_suggests("bsts")


  model <- model(list(), y)

  ## Fit the model
  fitted_model <- bsts::bsts(y,
                             state.specification = model,
                             niter = ifelse(samples < 100, 100 + samples, samples * 2),
                             ping = 0)


  ## Predict using the model
  prediction <- bsts::predict.bsts(fitted_model, horizon = horizon,
                                   burn = ifelse(samples < 100, 100, samples),
                                   quantiles = c(.025, .975))

  ## Extract samples and tidy format
  samples <- as.data.frame(prediction$distribution)

  return(samples)

}

#' brms model wrapper
#'
#' Allows users to specify a model using the [brms::bf()]  wrapper from `brms`
#' Note that `brms` and `tidybayes` must both be installed for this
#' model wrapper to be functional.
#'
#' @param y Numeric vector of time points to forecast
#' @param samples Numeric, number of samples to take.
#' @param model A `brms` model wrapped in the [brms::bf()] function
#' @param horizon Numeric, the time horizon over which to predict.
#' @param n_cores Numeric, the number of cores to use, default of 1
#' @param n_chains Numeric, the number of chains to use, default of 4
#' @param n_iter Numeric, the number of iterations in the sampler to use,
#'     default of 4000
#' @param ... additional arguments passed to `brms` (e.g. priors or family)
#' @return A dataframe of predictions (with columns representing the time horizon and rows representing samples).
#' @export
#' @importFrom data.table `:=`
#' @examples \dontrun{
#'
#' ## Used on its own
#' ## Note: More iterations and chains should be used
#' library(brms)
#'brms_model(y = EpiSoon::example_obs_rts[1:10, ]$rt,
#'           model = brms::bf(y ~ gp(time)),
#'           samples = 10, horizon = 7, n_iter = 40, n_chains = 1, refresh =0)
#'
#' ## Used for forecasting
#' ## Note that the timeout parameter has been increased to allow
#' ## for the time for the code to be compiled
#' ## Note: More iterations and chains should be used
#'
#' forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'             model = function(...){
#'               brms_model(model = brms::bf(y ~ gp(time)), n_iter = 40, n_chains = 1, ...)},
#'                    horizon = 7, samples = 10, timeout = 300)
#'}

brms_model <- function(y = NULL, samples = NULL,
                       horizon = NULL, model = NULL, n_cores = 1,
                       n_chains = 4, n_iter = 2000, ...) {

  check_suggests("brms")
  check_suggests("tidybayes")
  check_suggests("tsibble")

  ## Make input numeric into correct tsibble format
  timeseries <- tsibble::tsibble(y = y, time = 1:length(y), index = time)

  ## Fit the model
  fit <- brms::brm(formula = model, data = timeseries,
                   chains = n_chains, iter = n_iter,
                   cores = n_cores, ...)

  # Create Prediction Data Frame
  dat_new <- data.frame(time = (length(y)+1):(length(y)+horizon))

  prediction <- tidybayes::add_fitted_draws(newdata = dat_new,
                                            model = fit, n = samples)

  prediction <- dplyr::mutate(prediction, row_id = dplyr::row_number())

  prediction <- dplyr::ungroup(prediction)

  prediction <- data.table::as.data.table(prediction)

  prediction <- data.table::dcast(prediction, row_id~time, value.var = ".value")

  prediction <- prediction[,row_id:=NULL]

  ## Extract samples and tidy format
  samples <- as.data.frame(prediction)

  return(samples)

}

#' Fable model wrapper
#'
#'
#' @description Provides an interface for models from the `fable` package.
#' Note the `feasts::ARIMA` model requires the `feast` package. If `future`
#' is being used `fable` will require `future.apply` in
#' order to not silently fail.
#'
#' @param model A `fable` model object. For  models that use a formula interface time
#' can be accessed using `time`.
#' @inheritParams bsts_model
#' @return A dataframe of predictions (with columns representing the
#'  time horizon and rows representing samples).
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @examples \dontrun{
#' ## Used on its own
#' fable_model(y = EpiSoon::example_obs_rts[1:10, ]$rt,
#'            model = fable::ARIMA(y ~ time),
#'            samples = 10, horizon = 7)
#'
#'
#'forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'            model = function(...){
#'            fable_model(model = fable::ARIMA(y ~ time), ...)},
#'            horizon = 7, samples = 10)
#'}
fable_model <- function(y = NULL, samples = NULL,
                        horizon = NULL, model = NULL) {

  check_suggests("tsibble")
  check_suggests("fable")
  check_suggests("fabletools")
  check_suggests("feasts")
  check_suggests("future.apply")

  ## Make input numeric into correct tsibble format
  timeseries <- tsibble::tsibble(y = y, time = 1:length(y), index = time)

  ## Define model with data
  model <- fabletools::model(timeseries, model)

  ## Fit and forecast model
  forecast <- fabletools::forecast(model, h = horizon, times = samples)

  if (samples == 1) {
    ## If only using a single sample use central estimate
    samples <- t(data.frame(forecast$y))
    samples <- data.frame(samples)
  }else{
    ## Pull out distributions
    dist <- forecast$.distribution

    ## If samples are present pull out
    if (length(dist[[1]]) == 2) {
      samples <- purrr::map(dist, ~ .[[1]][[1]])
    }else {
      ## If dist is present sample from it
      samples <-  purrr::map(dist,
                             ~ rnorm(samples,
                                     mean = .$mean,
                                     sd = .$sd))
    }

    ## Bind samples together
    names(samples) <- 1:length(samples)
    samples <- dplyr::bind_cols(samples)
  }

  return(samples)
}

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




#' forecast model wrapper
#'
#' Allows users to forecast using models from the `forecast`
#' Note that `forecast` must be installed for this model wrapper to be functional.
#' @param model A `forecast` model object.
#' @inheritParams bsts_model
#' @export
#' @return A dataframe of predictions (with columns representing the
#' time horizon and rows representing samples).
#'
#' @importFrom stats ts
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#'
#' @examples \dontrun{
#'
#' ## Used on its own
#' forecast_model(y = EpiSoon::example_obs_rts[1:10, ]$rt,
#'                model = forecast::auto.arima,
#'                samples = 10, horizon = 7)
#'
#'
#' forecast_rt(EpiSoon::example_obs_rts[1:10, ],
#'             model = function(...){
#'             forecast_model(model = forecast::ets, ...)},
#'             horizon = 7, samples = 10)
#'}
#'

forecast_model <- function(y = NULL, samples = NULL,
                                 horizon = NULL, model = NULL) {

  check_suggests("forecast")

  # convert to timeseries object
  timeseries <- stats::ts(y)

  # fit and forecast
  fit <- model(timeseries)
  prediction <- forecast::forecast(fit, h = horizon)

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

