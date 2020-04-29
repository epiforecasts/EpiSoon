# predict_cases
# This function predicts the cases from one sample of forecast Rt. It needs a
# data frame with two columns (date and cases), another data frame with the
# columns rt and date (rt is numerical and date is a date format), a vector of
# numerical values that describe the probability distribution of the interval.
# The other arguments does not seem to be mandatory (to check)
# Things to test:
## In tests
#   - that the function returns the proper result
#   - that the result class is correct

# set.seed(1234)

forecast <- forecast_rt(EpiSoon::example_obs_rts[1:10, ], model = function(...){
  EpiSoon::bsts_model(model = function(ss, y){
    bsts::AddSemilocalLinearTrend(ss, y = y)
    }, ...)
  }, horizon = 7, samples = 1)


predictedCases <- predict_cases(
  cases = EpiSoon::example_obs_cases,
  rts = forecast,
  forecast_date = as.Date("2020-03-10"),
  serial_interval = EpiSoon::example_serial_interval
)

# expectedTable <- tibble::as_tibble(
#   data.frame(
#     date = as.Date(c("2020-03-11", "2020-03-12", "2020-03-13", "2020-03-14",
#                      "2020-03-15", "2020-03-16", "2020-03-17")),
#     cases = c(120, 204, 132, 131, 219, 151, 193)
#   )
# )

test_that("The expected Rt sample forecasts predict cases are obtained", {
  expect_true(is.data.frame(predictedCases))
  expect_named(predictedCases, c("date", "cases"))
  # expect_equal(predictedCases$date, expectedTable$date)
  # expect_gte(cor(predictedCases$cases, expectedTable$cases), .9)
})
