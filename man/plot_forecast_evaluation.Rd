% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_forecast_evaluation}
\alias{plot_forecast_evaluation}
\title{Plot a Forecast}
\usage{
plot_forecast_evaluation(
  forecasts = NULL,
  observations = NULL,
  horizon_to_plot = 1
)
}
\arguments{
\item{forecasts}{A dataframe as produced by \code{forecast_rt}
or \code{forecast_cases}}

\item{observations}{A dataframe of observations containing the
following variables:
\itemize{
\item either \code{rt} or \code{cases}
\item and \code{date}.
}}

\item{horizon_to_plot}{Numeric vector, the forecast horizon to plot.}
}
\value{
A \code{ggplot2} object
}
\description{
Plot a Forecast
}
\examples{
\dontrun{
## Evaluate a model
forecast_eval <- evaluate_model(EpiSoon::example_obs_rts,
  EpiSoon::example_obs_cases,
  model = function(...) {
    EpiSoon::bsts_model(
      model =
        function(ss, y) {
          bsts::AddSemilocalLinearTrend(ss, y = y)
        }, ...
    )
  },
  serial_interval = EpiSoon::example_serial_interval,
  horizon = 7, samples = 10
)

## Plot Rt forecast
plot_forecast_evaluation(forecast_eval$forecast_rts,
  EpiSoon::example_obs_rts,
  horizon_to_plot = 7
)


## Plot case forecast
plot_forecast_evaluation(forecast_eval$forecast_cases,
  EpiSoon::example_obs_cases,
  horizon_to_plot = 7
)
}
}
