
context("Test of summarise_forecast() and summarise_case_forecast()")

# FIXME : recommended to define classes and corresponding summary() methods to guarantee input class and comply to R standards

# horizon = 7 (provided example)
forecast_h7 <- forecast_rt(
	example_obs_rts,
	model = function(...) {
		EpiSoon::bsts_model(
			model = function(ss, y) { bsts::AddSemilocalLinearTrend(ss, y = y) },
			...
		)
	},
	horizon = 7,
	samples = 10
)
case_forecast_h7 <- forecast_cases(
	EpiSoon::example_obs_cases,
	forecast_h7,
	EpiSoon::example_serial_interval
)
summary_forecast_h7 <- summarise_forecast(forecast_h7)
summary_case_forecast_h7 <- summarise_case_forecast(case_forecast_h7)

# horizon = 1
forecast_h1 <- forecast_rt(
	example_obs_rts,
	model = function(...) {
		EpiSoon::bsts_model(
			model = function(ss, y) { bsts::AddSemilocalLinearTrend(ss, y = y) },
			...
		)
	},
	horizon = 1,
	samples = 10
)
case_forecast_h1 <- forecast_cases(
	EpiSoon::example_obs_cases,
	forecast_h1,
	EpiSoon::example_serial_interval
)
summary_forecast_h1 <- summarise_forecast(forecast_h1)
summary_case_forecast_h1 <- summarise_case_forecast(case_forecast_h1)



test_that("summarise_forecast() output is of expected format", {
	expect_s3_class(summary_forecast_h7, c("tbl_df", "tbl", "data.frame"))
	expect_named(summary_forecast_h7, c("date", "horizon", "median", "mean", "sd", "bottom", "lower", "upper", "top"))
})

test_that("summarise_case_forecast() output is of expected format", {
	expect_s3_class(summary_case_forecast_h7, c("tbl_df", "tbl", "data.frame"))
	expect_named(summary_case_forecast_h7, c("date", "horizon", "median", "mean", "sd", "bottom", "lower", "upper", "top"))
})

test_that("summarise_forecast() rows vary with 'horizon'", {
	# Ascending horizon
	expect_equal(summary_forecast_h1$horizon, 1)
	expect_equal(summary_forecast_h7$horizon, 1:7)
	
	# Ascending date
	expect_equal(summary_forecast_h1$date, sort(unique(forecast_h1$date)))
	expect_equal(summary_forecast_h7$date, sort(unique(forecast_h7$date)))
})

test_that("summarise_case_forecast() rows vary with 'horizon'", {
	# Ascending horizon
	expect_equal(summary_case_forecast_h1$horizon, 1)
	expect_equal(summary_case_forecast_h7$horizon, 1:7)
	
	# Ascending date
	expect_equal(summary_case_forecast_h1$date, sort(unique(forecast_h1$date)))
	expect_equal(summary_case_forecast_h7$date, sort(unique(forecast_h7$date)))
})

