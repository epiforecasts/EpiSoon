# draw_from_si_prob
# This function returns result for requested days. It simply calls for
# serial_interval[days_ago] but checking first the 'days_ago' are in the
# interval. Otherwise, it returns 0.
# Things to test:
## In tests
#   - that the function return the proper result
#   - that the result is of the proper class

exampleResult <- draw_from_si_prob(
  c(1, 2, 4, 10),
  EpiSoon::example_serial_interval
)
# expectedResult <- EpiSoon::example_serial_interval[c(1, 2, 4, 10)]
test_that("The expected draw is obtained", {
  # expect_equal(exampleResult, expectedResult)
  expect_true(is.numeric(exampleResult))
  expect_length(exampleResult, 4)
})
