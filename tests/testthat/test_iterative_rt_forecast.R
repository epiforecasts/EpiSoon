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
  expect_type(run_test, "list")
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

## Suggestions:
##
##   add checks on inputs: timeout >= 0, samples >= 1 and integer, horizon >= 1 and integer,
##   min_points is >= 1 and integer, error is thrown if nrow(rts) <= min_points,
##   model input is either bsts_model or fable_model (or at least has arguments of y, samples, and horizon),
##   rts is indeed a data frame with the appropriate variables (rt, being numeric, and date, being a date),
##   bound_rt is logical
##
##   Change the name of the samples object that is created in the function so as not to conflict with the
##   samples argument
