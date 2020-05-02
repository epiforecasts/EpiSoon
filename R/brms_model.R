#' brms model wrapper
#'
#' Allows users to specify a model using the [brms::bf()]  wrapper from `brms`
#' Note that `brms` and `tidybayes` must both be installed for this
#' model wrapper to be functional.
#'
#' @param y Numeric vector of time points to forecast
#' @param samples Numeric, number of samples to take.
#' @param model A [brms] model wrapped in the [brms::bf()] function
#' @param horizon Numeric, the time horizon over which to predict.
#' @param n_cores Numeric, the number of cores to use, default of 1
#' @param n_chains Numeric, the number of chains to use, default of 4
#' @param n_iter Numeric, the number of iterations in the sampler to use,
#'     default of 4000
#' @param ... additional arguments passed to `brms` (e.g. priors or family)
#' @return A dataframe of predictions (with columns representing the time horizon and rows representing samples).
#' @export
#' @importFrom brms bf gp
#' @importFrom data.table `:=`
#' @examples
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

brms_model <- function(y = NULL, samples = NULL,
                       horizon = NULL, model = NULL, n_cores = 1,
                       n_chains = 4, n_iter = 2000, ...) {

  check_suggests("brms")

  check_suggests("tidybayes")

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
