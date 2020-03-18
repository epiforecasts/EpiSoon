

# observations <- data.frame(rt = 1:20,
#                             date = as.Date("2020-01-01")
#                                    + lubridate::days(1:20))
#
#
#  models <- list("Sparse AR" = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
#                 "Semi-local linear trend" = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}
#                 )
#  ## Evaluate a model
#  evaluate_model(observations,
#                 model = models[[2]],
#                 horizon = 7, samples = 10)
#
#
#
# compare_models <- function(observations = NULL, models = NULL,
#                            horizon = 7, samples = 1000,
#                            bound_rt = TRUE) {
#
#
#   ## Evaluate each model (potential to swap in furrr here)
#   evaluations <- models %>%
#     purrr::map(
#       ~ evaluate_model(observations,
#                        model = .,
#                        horizon = horizon,
#                        samples = samples,
#                        bound_rt = bound_rt)
#     )
#
#   }
