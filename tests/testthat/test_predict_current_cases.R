# predict_current_cases
# same remark as for the tests of 'test_predict_cases'
predictedCurrentCases <- predict_current_cases(
  cases = EpiSoon::example_obs_cases,
  rts = EpiSoon::example_obs_rts,
  serial_interval = EpiSoon::example_serial_interval
)

# expectedCurrentCases <- data.frame(
#   rt = EpiSoon::example_obs_rts$rt,
#   date = EpiSoon::example_obs_rts$date,
#   infectiousness = c(14.145, 14.495, 14.925, 15.435, 16.635, 18.705, 22.805,
#                      27.555, 41.465, 60.965, 74.735, 104.655, 151.405, 158.155,
#                      158.155, 160.445, 162.955, 165.845, 172.575, 184.005,
#                      209.880, 223.155),
#   cases = c(30, 53, 36, 37, 33, 39, 54, 63, 90, 119, 152, 201, 302, 337, 318,
#             282, 320, 296, 298, 311, 359, 349)
# )

test_that("The expected Rts based on observed data predict cases are obtained", {
  expect_s3_class(predictedCurrentCases, "data.frame")
  expect_named(predictedCurrentCases,
               c("rt", "date", "infectiousness", "cases"))
  expect_length(predictedCurrentCases$date, nrow(EpiSoon::example_obs_rts))
  # expect_equal(predictedCurrentCases$rt, expectedCurrentCases$rt)
  # expect_equal(predictedCurrentCases$date, expectedCurrentCases$date)
  # expect_equal(predictedCurrentCases$infectiousness,
  #              expectedCurrentCases$infectiousness)
  # expect_gte(cor(predictedCurrentCases$cases, expectedCurrentCases$cases), .9)
})
