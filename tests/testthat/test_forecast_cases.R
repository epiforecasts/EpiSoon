
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

	forecast <- forecast[ order(forecast$sample, forecast$date) ,]
	expect_identical(
		forecast[, c("sample","date","horizon") ],
		out[, c("sample","date","horizon") ]
	)
})

test_that("forecast_cases() handles missing arguments as expected", {

	expect_error(
		forecast_cases(),
		"is missing"
	)

	expect_error(
		forecast_cases(
			fit_samples = forecast,
			serial_interval = EpiSoon::example_serial_interval
		),
		"cases"
	)

	expect_error(
		forecast_cases(
			cases = EpiSoon::example_obs_cases,
			serial_interval = EpiSoon::example_serial_interval
		),
		"fit_samples"
	)

	expect_error(
		forecast_cases(
			cases = EpiSoon::example_obs_cases,
			fit_samples = forecast
		),
		"serial_interval"
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

})

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

# To be clarified <https://github.com/epiforecasts/EpiSoon/issues/42>
test_that("forecast_cases() handles properly 'horizon'", {

	expect_false(
		identical(
			{
				set.seed(42)
				forecast_cases(
					EpiSoon::example_obs_cases,
					fit_samples = forecast,
					serial_interval = EpiSoon::example_serial_interval,
					horizon = 1
				)
			},{
				set.seed(42)
				forecast_cases(
					EpiSoon::example_obs_cases,
					fit_samples = forecast,
					serial_interval = EpiSoon::example_serial_interval,
					horizon = 5
				)
			}
		)
	)

	expect_error(
		forecast_cases(
			EpiSoon::example_obs_cases,
			fit_samples = forecast,
			serial_interval = EpiSoon::example_serial_interval,
			horizon = 10
		)
	)

})

