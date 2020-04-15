##------------------------------------------------------------
context("Test of the function 'evaluate_model'.... testing inputs")

sampled_obs <- EpiSoon::example_obs_rts %>%
  dplyr::mutate(sample = 1) %>%
  dplyr::bind_rows(EpiSoon::example_obs_rts %>%
                     dplyr::mutate(sample = 2))

sampled_cases <- EpiSoon::example_obs_cases %>%
  dplyr::mutate(sample = 1) %>%
  dplyr::bind_rows(EpiSoon::example_obs_cases %>%
                     dplyr::mutate(sample = 2))

a_model <- function(...) {
  EpiSoon::bsts_model(model = function(ss, y) {
    bsts::AddSemilocalLinearTrend(ss, y = y)
  }, ...)
}

test_that("Inputs with unequal lengths return error", {
  sampled_cases_sub <- sampled_cases %>% filter(sample != 2)

  expect_error(
    evaluate_model(sampled_obs,
                   sampled_cases_sub,
                   model = a_model,
                   horizon = 7, samples = 10,
                   serial_interval = EpiSoon::example_serial_interval),
    "Must have the same number of Rt and case samples."
  )
})

test_that("Outputs have proper lenghts and names", {
  out <- evaluate_model(sampled_obs,
                        sampled_cases,
                        model = a_model,
                        horizon = 7, samples = 10,
                        serial_interval = EpiSoon::example_serial_interval)

  expect_length(out, 4)

  expect_named(out, c("forecast_rts", "rt_scores", "forecast_cases", "case_scores"))

  # raw outputs added
  out <- evaluate_model(sampled_obs,
                        sampled_cases,
                        model = a_model,
                        horizon = 7, samples = 10,
                        serial_interval = EpiSoon::example_serial_interval,
                        return_raw = TRUE)

  expect_length(out, 6)

  expect_named(out,
               c("forecast_rts", "rt_scores", "forecast_cases", "case_scores",
                 "raw_rt_forecast", "raw_case_forecast"))

})
