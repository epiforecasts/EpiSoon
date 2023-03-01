## ------------------------------------------------------------
context("Test of the function 'forecast_rt'.... testing outputs")

mod_test <- function(...) {
  EpiSoon::bsts_model(model = function(ss, y) {
    bsts::AddAutoAr(ss, y = y, lags = 10)
  }, ...)
}
rts_test <- EpiSoon::example_obs_rts[1:10, ]
horizon_test <- 7
samples_test <- 10
run_test <- forecast_rt(
  rts = rts_test,
  model = mod_test,
  horizon = horizon_test,
  samples = samples_test,
  bound_rt = TRUE,
  timeout = 30
)
test_that("Output has proper length, names, class", {
  expect_s3_class(run_test, c("tbl_df", "tbl", "data.frame"))
  expect_named(run_test)
  expect_equal(nrow(run_test), horizon_test * samples_test)
  expect_length(run_test, 4)
  expect_named(run_test, c("sample", "date", "rt", "horizon"))
})
test_that("bound_rt = TRUE has appropriate bounding behavior", {
  expect_gte(min(run_test$rt), 0)
})

## ------------------------------------------------------------
context("Test of the function 'forecast_rt'.... testing inputs")
misspecified_model <- function(...) {
  lm(y ~ x)
}

test_that("Outputs returns error for inappropriately specified model", {
  expect_error(forecast_rt(
    rts = rts_test,
    model = misspecified_model,
    horizon = horizon_test,
    samples = samples_test,
    bound_rt = TRUE,
    timeout = 30
  ))
})
