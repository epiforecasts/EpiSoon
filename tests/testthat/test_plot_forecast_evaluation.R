##------------------------------------------------------------
context("Test of the function 'plot_forecast_evaluation'.... testing output")

forecast_eval <- evaluate_model(EpiSoon::example_obs_rts,
                                EpiSoon::example_obs_cases,
                                model = function(...) {
                                  EpiSoon::bsts_model(model = function(ss, y) {
                                    bsts::AddSemilocalLinearTrend(ss, y = y)
                                  }, ...)},
                                serial_interval = EpiSoon::example_serial_interval,
                                horizon = 7, samples = 10)

test_that("Plot is obtained for outputs of models", {
  p <- plot_forecast_evaluation(forecast_eval$forecast_rts,
                                EpiSoon::example_obs_rts,
                                horizon_to_plot = 7)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_silent(plot(p))
})
