# draw_from_si_prob
# This function returns result for requested days. It simply calls for
# serial_interval[days_ago] but checking first the 'days_ago' are in the
# interval. Otherwise, it returns 0.
# There is not day 13 in EpiSoon::example_serial_interval.
# Things to test:
## In the function
#   - that days_ago is numerical
#   - that serial_interval is a vector of numerical values
## In tests
#   - that the function return the proper result
#   - that the result is of the proper class

exampleResult <- draw_from_si_prob(c(1, 2, 4, 10),
                                   EpiSoon::example_serial_interval)
test_that("The expected draw is obtained", {
  expect_equal(exampleResult, c(0, 0.03, 0.17, 0.03))
  expect_identical(class(exampleResult), "numeric")
})
