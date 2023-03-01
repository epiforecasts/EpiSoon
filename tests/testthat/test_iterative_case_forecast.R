
context("Test of iterative_case_forecast()")

# Iterative RT forecast to use (provided example)
it_forecast <- iterative_rt_forecast(
  EpiSoon::example_obs_rts,
  model = function(...) {
    EpiSoon::bsts_model(
      model = function(ss, y) {
        bsts::AddSemilocalLinearTrend(ss, y = y)
      },
      ...
    )
  },
  horizon = 7,
  samples = 10
)



test_that("iterative_case_forecast() output is of expected format", {
  # Provided example
  out <- iterative_case_forecast(
    it_fit_samples = it_forecast,
    cases = EpiSoon::example_obs_cases,
    serial_interval = EpiSoon::example_serial_interval
  )

  expect_s3_class(out, c("tbl_df", "tbl", "data.frame"))
  expect_named(out, c("forecast_date", "sample", "date", "cases", "horizon"))
  expect_equal(nrow(it_forecast), nrow(out))

  # FIXME : recommended to preserve original order
  sorted_forecast <- it_forecast[order(it_forecast$forecast_date, it_forecast$sample, it_forecast$date), ]
  expect_equal(sorted_forecast$sample, out$sample)
  expect_equal(sorted_forecast$date, out$date)
  expect_equal(sorted_forecast$horizon, out$horizon)
  expect_equal(sorted_forecast$forecast_date, out$forecast_date)

  # FIXME : recommended to preserve column classes (sample casted from integer to numeric)
})

# FIXME : no 'horizon' argument to pass to forecast_cases() ?

# 'rdist' tested in forecast_cases() (passed untouched)
