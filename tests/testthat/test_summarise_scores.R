##------------------------------------------------------------
context("Test of the function 'summarise_scores'.... testing outputs")

rts <- EpiSoon::example_obs_rts %>%
  dplyr::mutate(timeseries = "Region 1") %>%
  dplyr::bind_rows(EpiSoon::example_obs_rts %>%
                     dplyr::mutate(timeseries = "Region 2"))

cases <- EpiSoon::example_obs_cases %>%
  dplyr::mutate(timeseries = "Region 1") %>%
  dplyr::bind_rows(EpiSoon::example_obs_cases %>%
                     dplyr::mutate(timeseries = "Region 2"))

models <- list("AR 3" = function(...) {
  EpiSoon::bsts_model(model = function(ss, y) {
    bsts::AddAr(ss, y = y, lags = 3)
  }, ...)},
               "Semi-local linear trend" = function(...) {
  EpiSoon::bsts_model(model = function(ss, y) {
    bsts::AddSemilocalLinearTrend(ss, y = y)
  }, ...)}
)

out <- compare_timeseries(rts, cases, models, horizon = 7, samples = 10,
                          serial_interval = EpiSoon::example_serial_interval)
exp_scores <- setdiff(names(out$rt_scores),
                      c("timeseries", "model", "forecast_date", "date",
                        "horizon"))

test_that("Default outputs have proper lengths and names", {
  summary_out <- summarise_scores(out$rt_scores)

  # test that the summary for all scores are obtained for every model
  expect_equal(nrow(summary_out), length(models) * length(exp_scores))

  expect_named(summary_out, c("score", "model", "bottom", "lower",  "median",
                              "mean", "upper", "top", "sd"))

  expect_s3_class(summary_out, c("tbl_df", "tbl", "data.frame"))
})

test_that("Summary over variables has proper lengths and names", {
  ## horizon
  summary_horizon <- summarise_scores(out$rt_scores, "horizon")

  # summary for all scores are obtained for every horizon and every model
  expect_equal(nrow(summary_horizon),
               length(models) * length(exp_scores) * max(out$rt_scores$horizon))

  expect_named(summary_horizon, c("horizon", "score", "model", "bottom", "lower",
                                  "median", "mean", "upper", "top", "sd"))

  ## timeseries
  summary_timeseries <- summarise_scores(out$rt_scores, "timeseries")

  # summary for all scores are obtained for every region and every model
  nb_timeseries <- length(unique(out$rt_scores$timeseries))
  expect_equal(nrow(summary_timeseries),
               length(models) * length(exp_scores) * nb_timeseries)

  expect_named(summary_timeseries, c("timeseries", "score", "model", "bottom",
                                     "lower", "median", "mean", "upper", "top",
                                     "sd"))

  ## timeseries AND horizon
  summary_variables <- summarise_scores(out$rt_scores, c("timeseries", "horizon"))

  # summary for all scores are obtained for every horizon, every region, every model
  nb_timeseries <- length(unique(out$rt_scores$timeseries))
  expect_equal(nrow(summary_variables),
               length(models) * length(exp_scores) *
                 max(out$rt_scores$horizon) * nb_timeseries)

  expect_named(summary_variables, c("timeseries", "horizon", "score", "model",
                                    "bottom", "lower", "median", "mean", "upper",
                                    "top", "sd"))
})

## Instead summarise across region and summarise case scores
test_that("Specific score summary has proper lengths and names", {
  ## one score
  summary_crps <- summarise_scores(out$case_scores, sel_scores = "crps")

  # summary for all scores are obtained for every horizon and every model
  expect_equal(nrow(summary_crps), length(models))

  ## more than one score
  summary_scores <- summarise_scores(out$case_scores,
                                     sel_scores = c("crps", "dss"))

  # summary for all scores are obtained for every horizon and every model
  expect_equal(nrow(summary_scores), length(models) * 2)
})

test_that("Summary are handled properly when there is just one model or no field 'model'", {
  ## one model
  one_model <- out$rt_scores %>% filter(model == "Semi-local linear trend")

  summary_one_model <- summarise_scores(one_model)

  # test that the summary for all scores are obtained for every model
  expect_equal(nrow(summary_one_model), length(exp_scores))

  ## no field 'model'
  out$rt_scores$model <- NULL

  summary_no_model <- summarise_scores(out$rt_scores)

  # test that the summary for all scores are obtained for every model
  expect_equal(nrow(summary_no_model), length(exp_scores))

  expect_named(summary_no_model, c("score", "bottom", "lower", "median", "mean",
                                   "upper", "top", "sd"))
})

test_that("Summary works for outputs of 'evaluate_model'", {
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

  out <- evaluate_model(sampled_obs, sampled_cases, model = a_model,
                        horizon = 7, samples = 10,
                        serial_interval = EpiSoon::example_serial_interval)

  summary_models <- summarise_scores(out$rt_scores)

  # test that the summary for all scores are obtained for every model
  exp_scores <- setdiff(names(out$rt_scores),
                        c("sample", "model", "forecast_date", "date", "horizon"))
  expect_equal(nrow(summary_models), length(exp_scores))

  expect_named(summary_models, c("score", "bottom", "lower", "median", "mean",
                                 "upper", "top", "sd"))

  expect_s3_class(summary_models, c("tbl_df", "tbl", "data.frame"))
})
