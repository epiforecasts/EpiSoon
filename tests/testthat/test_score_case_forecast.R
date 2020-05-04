##------------------------------------------------------------
context("Test of the function 'score_case_forecast'.... testing outputs")

rts_test <- EpiSoon::example_obs_rts[1:10, ]
mod_test <- function(...) {
  EpiSoon::bsts_model(model = function(ss, y){
    bsts::AddSemilocalLinearTrend(ss, y = y)
  }, ...)
}
samples_test <- forecast_rt(rts = rts_test,
                       model = mod_test,
                       horizon = 7,
                       samples = 10)

case_test <- EpiSoon::example_obs_cases
serial_interval_test <- EpiSoon::example_serial_interval
pred_cases <- forecast_cases(cases = case_test,
                             fit_samples = samples_test,
                             serial_interval = serial_interval_test)

run_test <- score_case_forecast(pred_cases, case_test)

test_that("Output has proper length, names, class", {
  expect_s3_class(run_test, c("tbl_df", "tbl", "data.frame"))
  expect_named(run_test)
  expect_equal(nrow(run_test), length(unique(pred_cases$date)))
  expect_named(run_test, c("date", "horizon", "dss", "crps", "logs", "bias",
                           "sharpness", "calibration", "median", "iqr", "ci"))
  expect_equal(nrow(run_test), 7)
})
