% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterative.R
\name{iterative_direct_case_forecast}
\alias{iterative_direct_case_forecast}
\title{Iteratively forecast directly on cases}
\usage{
iterative_direct_case_forecast(
  cases,
  model = NULL,
  horizon = 7,
  samples = 1000,
  timeout = 30,
  bound_rt = TRUE,
  min_points = 3
)
}
\arguments{
\item{cases}{A dataframe containing \code{date} and \code{cases} variables}

\item{model}{A model object in the format of \code{bsts_model} or \code{fable_model}. See the corresponding
help files for details.}

\item{horizon}{Numeric, the time horizon over which to predict.}

\item{samples}{Numeric, number of samples to take.}

\item{timeout}{Numeric, timeout of model fitting in seconds. Defaults to 30 seconds.}

\item{bound_rt}{Logical, defaults to \code{TRUE}. Should Rt values be bounded to be greater than or
equal to 0.}

\item{min_points}{Numeric, defaults to 3. The minimum number of time points at which to begin
iteratively evaluating the forecast.}
}
\value{
A tibble of iterative forecasts
}
\description{
Iteratively forecast directly on cases
}
\examples{
\dontrun{
iterative_direct_case_forecast(EpiSoon::example_obs_cases,
  model = function(...) {
    EpiSoon::bsts_model(
      model =
        function(ss, y) {
          bsts::AddSemilocalLinearTrend(ss, y = y)
        }, ...
    )
  },
  horizon = 7, samples = 10, min_points = 4
)
}
}
