##------------------------------------------------------------
context("Test of the function 'score_forecast'.... testing outputs")

## Fit a model (using a subset of observations)
samples <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
                       model = function(...) {
                         EpiSoon::bsts_model(model = function(ss, y){
                           bsts::AddSemilocalLinearTrend(ss, y = y)
                         }, ...)
                       },
                       horizon = 7, samples = 10)

## Score the model fit (with observations during the time horizon of the forecast)
obs_test <- EpiSoon::example_obs_rts
run_test <- score_forecast(samples,
                           observations = obs_test)

test_that("Output has proper length, names, class", {
  expect_s3_class(run_test, c("tbl_df", "tbl", "data.frame"))
  expect_named(run_test)
  expect_equal(nrow(run_test), length(intersect(samples$date, obs_test$date)))
  expect_named(run_test, c("date", "horizon", "dss", "crps", "logs", "bias",
                           "sharpness", "calibration", "median", "iqr", "ci"))
})

