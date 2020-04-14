##------------------------------------------------------------
context("Check of group 3 functions: input arguments")

mod_test <- function(...) {
  EpiSoon::bsts_model(model = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 10)}, ...)
}
rts_test <- EpiSoon::example_obs_rts[1:10, ]
rts_error <- rts_test[,-1]

test_that("forecast_rt arguments are correct", {
  expect_error(forecast_rt(rts_error, model = mod_test, horizon = 7, samples = 10))
})

