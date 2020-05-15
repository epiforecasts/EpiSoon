
# EpiSoon

[![DOI](https://zenodo.org/badge/248311916.svg)](https://zenodo.org/badge/latestdoi/248311916)
[![R build
status](https://github.com/epiforecasts/EpiSoon/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiSoon)
[![Build
Status](https://travis-ci.com/epiforecasts/EpiSoon.svg?branch=master)](https://travis-ci.com/epiforecasts/EpiSoon)

**Aim:** To forecast the time-varying reproduction number and use this
to forecast reported case counts.

## Installation

Install the analysis and all dependencies with:

``` r
remotes::install_github("epiforecasts/EpiSoon", dependencies = TRUE)
```

## Quick start

  - Load packages (`bsts` and `fable` for models, `ggplot2` for
    plotting, and `cowplot` for theming)

<!-- end list -->

``` r
library(EpiSoon)
library(bsts)
library(fable)
library(future)
library(cowplot)
library(dplyr)
```

  - Set up example data (using `EpiSoon::example_obs_rts` and
    `EpiSoon::example_obs_cases` as starting data sets). When generating
    timeseries with `EpiNow` use `get_timeseries` to extract the
    required data.

<!-- end list -->

``` r
obs_rts <- EpiSoon::example_obs_rts %>%
   dplyr::mutate(timeseries = "Region 1") %>%
   dplyr::bind_rows(EpiSoon::example_obs_rts %>%
  dplyr::mutate(timeseries = "Region 2"))

obs_cases <- EpiSoon::example_obs_cases %>%
   dplyr::mutate(timeseries = "Region 1") %>%
   dplyr::bind_rows(EpiSoon::example_obs_cases %>%
   dplyr::mutate(timeseries = "Region 2"))
```

  - Define the list of models to be compared.

<!-- end list -->

``` r
models <- list("AR 3" =
                function(...) {EpiSoon::bsts_model(model =
                     function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)},
               "Semi-local linear trend" =
                function(...) {EpiSoon::bsts_model(model =
                    function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
               "ARIMA" = 
                    function(...){EpiSoon::fable_model(model = fable::ARIMA(y ~ time), ...)})
```

  - Compare models across timeseries (change the `future::plan` to do
    this in parallel).

<!-- end list -->

``` r
future::plan("sequential")

## Compare models
forecasts <- EpiSoon::compare_timeseries(obs_rts, obs_cases, models,
                                         horizon = 7, samples = 10,
                                         serial_interval = EpiSoon::example_serial_interval)

forecasts
#> $forecast_rts
#> # A tibble: 798 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  0     1.08  2.15       0
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  0.203 0.955 1.59       0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  0.232 0.790 1.26       0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  0.259 1.70  2.69       0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  0.238 1.40  2.35       0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  0.862 0.830 0.880      0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  1.22  1.92  2.95       0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1  0.378 1.18  1.88       0
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2  0.536 1.87  3.64       0
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3  0.758 1.87  3.44       0
#> # … with 788 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $rt_scores
#> # A tibble: 630 x 14
#>    timeseries model forecast_date date       horizon   dss  crps  logs   bias
#>    <chr>      <chr> <chr>         <date>       <int> <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  1.76 1.42  13.8  -0.6  
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  1.49 1.14   2.48 -0.8  
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  1.65 1.15   2.49 -0.8  
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  1.90 0.914  2.04 -0.400
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  1.70 0.999  2.45 -0.400
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  1.70 0.851  1.80 -0.8  
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  2.06 0.564  1.62 -0.400
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1  1.48 1.18   6.29 -0.6  
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2  2.48 1.00   2.29 -0.6  
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3  2.37 0.779  1.75 -0.6  
#> # … with 620 more rows, and 5 more variables: sharpness <dbl>,
#> #   calibration <dbl>, median <dbl>, iqr <dbl>, ci <dbl>
#> 
#> $forecast_cases
#> # A tibble: 630 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1    0    32.3  66.2      0
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2    7.5  36.3  62.3      0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3    6    33.9  70.3      0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4    6.5  60.3 129.       0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5    4.5  67.7 170.       0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6   16    38.7  50.0      0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7   38.5  53.9  64.1      0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1   13    46.4  74.3      0
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2   17.5  88.6 178.       0
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3   30.5  67.7 107.       0
#> # … with 620 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $case_scores
#> # A tibble: 630 x 15
#>    timeseries model sample forecast_date date       horizon   dss  crps  logs
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-04    2020-03-05       1  8.52  40.8 35.8 
#>  2 Region 1   AR 3  1      2020-03-04    2020-03-06       2  8.54  36.3  5.79
#>  3 Region 1   AR 3  1      2020-03-04    2020-03-07       3  9.06  55.9  8.89
#>  4 Region 1   AR 3  1      2020-03-04    2020-03-08       4  9.74  58.9  6.74
#>  5 Region 1   AR 3  1      2020-03-04    2020-03-09       5 10.3   77.9 10.3 
#>  6 Region 1   AR 3  1      2020-03-04    2020-03-10       6 12.4   78.2  6.29
#>  7 Region 1   AR 3  1      2020-03-04    2020-03-11       7 11.7   91.1  7.27
#>  8 Region 1   AR 3  1      2020-03-05    2020-03-06       1  8.65  38.1  7.27
#>  9 Region 1   AR 3  1      2020-03-05    2020-03-07       2 10.3   43.1  6.13
#> 10 Region 1   AR 3  1      2020-03-05    2020-03-08       3  9.36  38.6  5.66
#> # … with 620 more rows, and 6 more variables: bias <dbl>, sharpness <dbl>,
#> #   calibration <dbl>, median <dbl>, iqr <dbl>, ci <dbl>
```

  - Plot an evaluation of Rt forecasts using iterative
fitting.

<!-- end list -->

``` r
EpiSoon::plot_forecast_evaluation(forecasts$forecast_rts, obs_rts, c(7)) +
   ggplot2::facet_grid(model ~ timeseries) +
   cowplot::panel_border()
```

<img src="man/figures/unnamed-chunk-7-1.png" width="60%" />

  - Plot an evaluation of case forecasts using iterative
fitting

<!-- end list -->

``` r
EpiSoon::plot_forecast_evaluation(forecasts$forecast_cases, obs_cases, c(7)) +
   ggplot2::facet_grid(model ~ timeseries, scales = "free") +
   cowplot::panel_border()
```

<img src="man/figures/unnamed-chunk-8-1.png" width="60%" />

  - Summarise the forecasts by model scored against observed cases

<!-- end list -->

``` r
EpiSoon::summarise_scores(forecasts$case_scores)
#> # A tibble: 27 x 9
#>    score  model      bottom    lower   median     mean    upper      top      sd
#>    <chr>  <chr>       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
#>  1 bias   AR 3     -1.00e+0 -8.75e-1 -8.00e-1 -6.94e-1 -6.00e-1   -0.200 2.65e-1
#>  2 bias   ARIMA    -1.00e+0  2.00e-1  8.00e-1  5.33e-1  1.00e+0    1     5.70e-1
#>  3 bias   Semi-l…  -1.00e+0  3.00e-1  8.00e-1  5.37e-1  1.00e+0    1     5.63e-1
#>  4 calib… AR 3      8.57e-5  8.57e-5  8.57e-5  2.83e-2  2.00e-3    0.504 1.26e-1
#>  5 calib… ARIMA     8.57e-5  8.57e-5  8.57e-5  3.59e-2  1.50e-4    0.509 1.15e-1
#>  6 calib… Semi-l…   8.57e-5  8.57e-5  8.57e-5  4.30e-2  1.50e-4    0.587 1.28e-1
#>  7 ci     AR 3      1.41e+2  3.76e+2  6.91e+2  1.60e+3  1.63e+3 8488.    2.28e+3
#>  8 ci     ARIMA     2.28e+1  4.62e+1  8.40e+1  1.04e+3  1.57e+3 6341.    1.72e+3
#>  9 ci     Semi-l…   2.51e+1  5.67e+1  1.26e+2  1.05e+3  1.62e+3 5913.    1.75e+3
#> 10 crps   AR 3      3.63e+1  9.19e+1  1.40e+2  1.58e+2  2.00e+2  429.    9.54e+1
#> # … with 17 more rows
```

## Docker

This package was developed in a docker container based on the
`rocker/geospatial` docker image.

To build the docker image run (from the `EpiSoon` directory):

``` bash
docker build . -t episoon
```

To run the docker image
run:

``` bash
docker run -d -p 8787:8787 --name episoon -e USER=episoon -e PASSWORD=episoon episoon
```

The rstudio client can be found on port :8787 at your local machines ip.
The default username:password is epinow:epinow, set the user with -e
USER=username, and the password with - e PASSWORD=newpasswordhere. The
default is to save the analysis files into the user directory.

To mount a folder (from your current working directory - here assumed to
be `tmp`) in the docker container to your local system use the following
in the above docker run command (as given mounts the whole `episoon`
directory to `tmp`).

``` bash
--mount type=bind,source=$(pwd)/tmp,target=/home/EpiSoon
```

To access the command line run the following:

``` bash
docker exec -ti episoon bash
```
