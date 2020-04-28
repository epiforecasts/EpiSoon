#' Stack models according to CRPS
#'
#' @description Provides a wrapper for different EpiSoon models and generates
#' a mixture model based on these models
#'
#' @param models A list of models. Models must be analogous to the form
#' `function(...){EpiSoon::fable_model(model = , ...)}` or
#' `function(...){EpiSoon::bsts_model(model = , ...)}`.
#' @inheritParams bsts_model
#' @return A dataframe of predictions (with columns representing the
#'  time horizon and rows representing samples).
#' @export
#' @importFrom tsibble tsibble
#' @importFrom fabletools model forecast
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @examples
#'
#' y <- EpiSoon::example_obs_rts[1:10, ]$rt
#' model = fable::ARIMA(y ~ time)
#' samples = 10
#' horizon = 7
#' weighting_period = 5
#'
#' # make list with models
#' baseline_ensemble <- function(...) {
#' EpiSoon::fable_model(model = fabletools::combination_model(fable::ARIMA(y), fable::ETS(y), fable::NAIVE(y),
#'                                                            fable::RW(y ~ drift()), cmbn_args = list(weights = "inv_var")), ...)
#' }
#' models <- list(
#'   Mixfabl = baseline_ensemble,
#'   "ARIMA" = function(...){EpiSoon::fable_model(model = fable::ARIMA(y), ...)},
#'   "ETS" = function(...){EpiSoon::fable_model(model = fable::ETS(y), ...)},
#'   "Drift" = function(...){EpiSoon::fable_model(model = fable::RW(y ~ drift()), ...)}
#' )
#'
#' # make forecst
#' forecast <- crps_ensemble(y = y,
#'                           models = models,
#'                           samples = 10,
#'                           horizon = 7,
#'                           weighting_period = 5)
#'
#'


crps_ensemble <- function(y = NULL,
                          models = NULL,
                          weighting_period = 5,
                          ...) {


  #### Error Handling
  # check if stackr is installed
  if (!suppressWarnings(require("stackr", quietly = TRUE) == TRUE)) {
    stop("package stackr must be installed. You can install it using devtools::install_github('nikosbosse/stackr')")
  }
  if (length(y) <= weighting_period) {
    stop("not enough observations to do weighting. Adjust weighting_period")
  }

  #### split data into train data and data for weighting
  n <- length(y)
  y_train <- y[1:(n - weighting_period)]
  y_weight <- y[(n - weighting_period + 1):n]

  #### fit models on train data and generate forecasts
  fc_w <- lapply(seq_along(models),
         FUN = function(i) {
           f <- models[[i]]
           out <- f(y = y_train, samples = samples, horizon = weighting_period)

           out %>%
             as_tibble() %>%
             dplyr::mutate(sample_nr = 1:n()) %>%
             tidyr::pivot_longer(names_to = "date",
                                 values_to = "y_pred",
                                 cols = -sample_nr) %>%
             dplyr::group_by(sample_nr) %>%
             dplyr::mutate(y_obs = y_weight) %>%
             dplyr::ungroup() %>%
             dplyr::mutate(model = names(models)[i],
                           geography = "Testland")

         })

  fc_w <- do.call(rbind, fc_w)

  w <- stackr::stack_crps(fc_w)

  #### generate forecasts and use weights to stack
  fc <- lapply(seq_along(models),
               FUN = function(i) {
                 f <- models[[i]]
                 out <- f(y = y, samples = samples, horizon = horizon)

                 out %>%
                   as_tibble() %>%
                   dplyr::mutate(sample_nr = 1:n()) %>%
                   tidyr::pivot_longer(names_to = "date",
                                       values_to = "y_pred",
                                       cols = -sample_nr) %>%
                   dplyr::group_by(sample_nr) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(model = names(models)[i],
                                 geography = "Testland")

               })

  fc <- do.call(rbind, fc)

  mix <- stackr::mixture_from_sample(fc, weights = w)

  mixed_samples <- mix %>%
    dplyr::select(-model, -geography) %>%
    pivot_wider(values_from = y_pred, names_from = date) %>%
    dplyr::select(-sample_nr)


  return(mixed_samples)
}


