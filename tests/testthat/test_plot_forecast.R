##------------------------------------------------------------
context("Test of the function 'plot_forecast'.... testing output")

test_that("Plot is obtained for summarized forecast", {
  samples <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
                         model = function(...) {
                           EpiSoon::bsts_model(model = function(ss, y) {
                             bsts::AddSemilocalLinearTrend(ss, y = y)
                           }, ...)},
                         horizon = 21, samples = 10)

  summarised_forecast <- summarise_forecast(samples)
  p <- plot_forecast(summarised_forecast, EpiSoon::example_obs_rts)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_silent(plot(p))
})

test_that("Plot is obtained for summarized cases", {
  pred_cases <- forecast_cases(EpiSoon::example_obs_cases, samples,
                               serial_interval = EpiSoon::example_serial_interval)

  summarised_case_forecast <- summarise_case_forecast(pred_cases)
  p <- plot_forecast(summarised_case_forecast, EpiSoon::example_obs_cases)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_silent(plot(p))
})
