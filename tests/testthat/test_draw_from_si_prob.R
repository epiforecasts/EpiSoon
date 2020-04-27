# draw_from_si_prob
# This function returns result for requested days. It simply calls for
# serial_interval[days_ago] but checking first the 'days_ago' are in the
# interval. Otherwise, it returns 0.
# Things to test:
## In tests
#   - that the function return the proper result
#   - that the result is of the proper class

exampleResult <- draw_from_si_prob(c(1, 2, 4, 10),
                                   EpiSoon::example_serial_interval)
test_that("The expected draw is obtained", {
  expect_equal(exampleResult, c(0, 0.03, 0.17, 0.03))
  expect_identical(class(exampleResult), "numeric")
})
