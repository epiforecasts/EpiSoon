## ------------------------------------------------------------
context("Test of the function 'compare_models'.... testing outputs")

models <- list(
  "AR 3" = function(...) {
    EpiSoon::bsts_model(model = function(ss, y) {
      bsts::AddAr(ss, y = y, lags = 2)
    }, ...)
  },
  "Semi-local linear trend" = function(...) {
    EpiSoon::bsts_model(model = function(ss, y) {
      bsts::AddSemilocalLinearTrend(ss, y = y)
    }, ...)
  }
)

set.seed(1234)

out <- compare_models(EpiSoon::example_obs_rts,
  EpiSoon::example_obs_cases,
  models,
  horizon = 7, samples = 10,
  serial_interval = EpiSoon::example_serial_interval
)

test_that("Outputs have proper lengths and names", {
  expect_length(out, 4)

  expect_named(out, c("forecast_rts", "rt_scores", "forecast_cases", "case_scores"))

  # 'return_raw' version
  out_raw <- compare_models(EpiSoon::example_obs_rts,
    EpiSoon::example_obs_cases,
    models,
    horizon = 7, samples = 10,
    serial_interval = EpiSoon::example_serial_interval,
    return_raw = TRUE
  )

  expect_length(out_raw, 6)

  expect_named(out_raw, c(
    "forecast_rts", "rt_scores", "forecast_cases",
    "case_scores", "raw_rt_forecast", "raw_case_forecast"
  ))
})

test_that("Outputs return results for all models", {
  expect_identical(names(models), unique(out$forecast_rts$model))
  # expect_equal(sum(is.na(out$forecast_rts)), 0)

  expect_identical(names(models), unique(out$rt_scores$model))
  # expect_equal(sum(is.na(out$rt_scores)), 0)

  expect_identical(names(models), unique(out$forecast_cases$model))
  # expect_equal(sum(is.na(out$forecast_cases)), 0)

  expect_identical(names(models), unique(out$case_scores$model))
  # expect_equal(sum(is.na(out$case_scores)), 0)
})
