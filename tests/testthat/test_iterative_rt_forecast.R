##------------------------------------------------------------
context("Test of the function 'iterative_rt_forecast'.... testing outputs")

rts_test <- EpiSoon::example_obs_rts
mod_test <- function(...) {
  EpiSoon::bsts_model(model = function(ss, y) {
    bsts::AddSemilocalLinearTrend(ss, y = y)
  }, ...)
}
horizon_test <- 7
samples_test <- 10
min_points_test <- 4

run_test <- iterative_rt_forecast(rts_test,
                      model = mod_test,
                      horizon = horizon_test,
                      samples = samples_test,
                      min_points = min_points_test)

test_that("Output has proper length, names, class", {
  expect_s3_class(run_test, c("tbl_df", "tbl", "data.frame"))
  expect_named(run_test)
  expect_equal(nrow(run_test), (nrow(rts_test)-min_points_test)*horizon_test*samples_test)
  expect_length(run_test, 5)
  expect_named(run_test, c("forecast_date", "sample", "date", "rt", "horizon"))
})
test_that("bound_rt = TRUE has appropriate bounding behavior", {
  expect_gte(min(run_test$rt), 0)
})
test_that("Earliest forecast date is `min_points`+1 days after the first rts input date", {
  expect_equal(min(rts_test$date)+min_points_test+1, min(run_test$date))
})
