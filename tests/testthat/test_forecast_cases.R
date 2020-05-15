
context("Test of forecast_cases()")

# Fit and forecast to use (provided example)
forecast <- forecast_rt(
	EpiSoon::example_obs_rts[1:10,],
	model = function(...) {
		EpiSoon::bsts_model(
			model = function(ss, y){
				bsts::AddAutoAr(ss, y=y, lags=10)
			},
			...
		)
	},
	horizon = 7,
	samples = 10
)



test_that("forecast_cases() output is of expected format", {
	# Provided example
	out <- forecast_cases(
		EpiSoon::example_obs_cases,
		fit_samples = forecast,
		serial_interval = EpiSoon::example_serial_interval
	)

	expect_s3_class(out, c("tbl_df", "tbl", "data.frame"))
	expect_named(out, c("sample", "date", "cases", "horizon"))
	expect_equal(nrow(forecast), nrow(out))
	
	# FIXME : recommended to preserve original order
	forecast <- forecast[ order(forecast$sample, forecast$date) ,]
	expect_equal(forecast$sample, out$sample)
	expect_equal(forecast$date, out$date)
	expect_equal(forecast$horizon, out$horizon)
	
	# FIXME : recommended to preserve column classes (sample casted from integer to numeric)
})

test_that("forecast_cases() handles missing arguments as expected", {

	expect_error(
		forecast_cases(),
		"is missing"
	)
	
	# FIXME : recommended to print a more explicit message 
	expect_error(
		forecast_cases(
			fit_samples = forecast,
			serial_interval = EpiSoon::example_serial_interval
		)
	)
	
	# FIXME : recommended to print a more explicit message 
	expect_error(
		forecast_cases(
			cases = EpiSoon::example_obs_cases,
			serial_interval = EpiSoon::example_serial_interval
		)
	)
	
	# FIXME : recommended to print a more explicit message 
	expect_error(
		forecast_cases(
			cases = EpiSoon::example_obs_cases,
			fit_samples = forecast
		)
	)
	
	expect_identical(
		{
			set.seed(42)
			forecast_cases(
				EpiSoon::example_obs_cases,
				fit_samples = forecast,
				serial_interval = EpiSoon::example_serial_interval
			)
		},{
			set.seed(42)
			forecast_cases(
				EpiSoon::example_obs_cases,
				fit_samples = forecast,
				serial_interval = EpiSoon::example_serial_interval,
				forecast_date = forecast$date[1]
			)
		}
	)
	
	expect_identical(
		{
			set.seed(42)
			forecast_cases(
				EpiSoon::example_obs_cases,
				fit_samples = forecast,
				serial_interval = EpiSoon::example_serial_interval
			)
		},{
			set.seed(42)
			forecast_cases(
				EpiSoon::example_obs_cases,
				fit_samples = forecast,
				serial_interval = EpiSoon::example_serial_interval,
				horizon = max(forecast$horizon)
			)
		}
	)
	
	expect_identical(
		{
			set.seed(42)
			forecast_cases(
				EpiSoon::example_obs_cases,
				fit_samples = forecast,
				serial_interval = EpiSoon::example_serial_interval
			)
		},{
			set.seed(42)
			forecast_cases(
				EpiSoon::example_obs_cases,
				fit_samples = forecast,
				serial_interval = EpiSoon::example_serial_interval,
				rdist = rpois
			)
		}
	)

})

# FIXME : 'horizon' appears to have no effect <https://github.com/epiforecasts/EpiSoon/issues/42>

test_that("forecast_cases() can handle custom sampling functions", {

	expect <- function(FUN) {
		expect_silent(
			forecast_cases(
				EpiSoon::example_obs_cases,
				fit_samples = forecast,
				serial_interval = EpiSoon::example_serial_interval,
				rdist = FUN
			)
		)
	}

	expect(rpois)
	expect(rnorm)
	expect(function(n,mean) { sample(-5:5+mean, n, replace=TRUE) })

})

