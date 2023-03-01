
test_that("plot_compare_timeseries produces expected output", {
  obs_rts <- EpiSoon::example_obs_rts %>%
    dplyr::mutate(timeseries = "Region 1") %>%
    dplyr::bind_rows(EpiSoon::example_obs_rts %>%
      dplyr::mutate(timeseries = "Region 2"))

  obs_cases <- EpiSoon::example_obs_cases %>%
    dplyr::mutate(timeseries = "Region 1") %>%
    dplyr::bind_rows(EpiSoon::example_obs_cases %>%
      dplyr::mutate(timeseries = "Region 2"))

  models <- list(
    "AR 3" = function(...) {
      EpiSoon::bsts_model(model = function(ss, y) {
        bsts::AddAr(ss, y = y, lags = 3)
      }, ...)
    },
    "Semi-local linear trend" = function(...) {
      EpiSoon::bsts_model(model = function(ss, y) {
        bsts::AddSemilocalLinearTrend(ss, y = y)
      }, ...)
    }
  )

  forecast_eval <-
    compare_timeseries(obs_rts, obs_cases, models,
      horizon = 10, samples = 10,
      serial_interval = EpiSoon::example_serial_interval
    )

  p <- plot_compare_timeseries(forecast_eval)
  expect_identical(class(p), "list")
  expect_length(p, 8)
  purrr::walk(p, function(x) {
    expect_s3_class(x, c("gg", "ggplot"))
    expect_silent(plot(x))
  })
})
