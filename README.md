
# EpiSoon

[![DOI](https://zenodo.org/badge/248311916.svg)](https://zenodo.org/badge/latestdoi/248311916)
[![Build
Status](https://travis-ci.com/epiforecasts/EpiSoon.svg?branch=master)](https://travis-ci.com/epiforecasts/EpiSoon)
[![codecov](https://codecov.io/gh/tuxette/EpiSoon/branch/master/graph/badge.svg)](https://codecov.io/gh/tuxette/EpiSoon)

*Warning: This package is a work in progress and is currently developed
solely with the COVID-19 outbreak in mind. Breaking changes may occur
and the authors cannot guarantee support.*

**Aim:** To forecast the time-varying reproduction number and using this
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
#> # A tibble: 763 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-06    2020-03-07       1   2.14  2.13 0.0234
#>  2 Region 1   AR 3  2020-03-06    2020-03-08       2   2.07  2.07 0.0113
#>  3 Region 1   AR 3  2020-03-06    2020-03-09       3   2.01  2.01 0.0197
#>  4 Region 1   AR 3  2020-03-06    2020-03-10       4   1.94  1.93 0.0212
#>  5 Region 1   AR 3  2020-03-06    2020-03-11       5   1.89  1.89 0.0136
#>  6 Region 1   AR 3  2020-03-06    2020-03-12       6   1.84  1.83 0.0171
#>  7 Region 1   AR 3  2020-03-06    2020-03-13       7   1.78  1.78 0.0260
#>  8 Region 1   AR 3  2020-03-07    2020-03-08       1   2.08  2.08 0.0154
#>  9 Region 1   AR 3  2020-03-07    2020-03-09       2   2.01  2.02 0.0204
#> 10 Region 1   AR 3  2020-03-07    2020-03-10       3   1.96  1.96 0.0235
#> # … with 753 more rows, and 4 more variables: bottom <dbl>, lower <dbl>,
#> #   upper <dbl>, top <dbl>
#> 
#> $rt_scores
#> # A tibble: 601 x 14
#>    timeseries model forecast_date date       horizon    dss    crps    logs
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl>   <dbl>   <dbl>
#>  1 Region 1   AR 3  2020-03-06    2020-03-07       1 -7.14  0.00798  -2.59 
#>  2 Region 1   AR 3  2020-03-06    2020-03-08       2  3.78  0.0325    4.06 
#>  3 Region 1   AR 3  2020-03-06    2020-03-09       3  0.452 0.0438    1.12 
#>  4 Region 1   AR 3  2020-03-06    2020-03-10       4 13.5   0.0823   33.8  
#>  5 Region 1   AR 3  2020-03-06    2020-03-11       5 84.5   0.118    61.9  
#>  6 Region 1   AR 3  2020-03-06    2020-03-12       6 98.6   0.160   289.   
#>  7 Region 1   AR 3  2020-03-06    2020-03-13       7 53.3   0.178    59.3  
#>  8 Region 1   AR 3  2020-03-07    2020-03-08       1 -6.03  0.0151   -2.43 
#>  9 Region 1   AR 3  2020-03-07    2020-03-09       2 -2.68  0.0334   -1.34 
#> 10 Region 1   AR 3  2020-03-07    2020-03-10       3  1.65  0.0555   -0.150
#> # … with 591 more rows, and 6 more variables: bias <dbl>, sharpness <dbl>,
#> #   calibration <dbl>, median <dbl>, iqr <dbl>, ci <dbl>
#> 
#> $forecast_cases
#> # A tibble: 601 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  2020-03-06    2020-03-07       1    92   93.9  9.34
#>  2 Region 1   AR 3  2020-03-06    2020-03-08       2   112. 110.   8.92
#>  3 Region 1   AR 3  2020-03-06    2020-03-09       3   131  127.  10.9 
#>  4 Region 1   AR 3  2020-03-06    2020-03-10       4   150  150.  11.4 
#>  5 Region 1   AR 3  2020-03-06    2020-03-11       5   163  167.  13.5 
#>  6 Region 1   AR 3  2020-03-06    2020-03-12       6   194. 194   11.1 
#>  7 Region 1   AR 3  2020-03-06    2020-03-13       7   227  230   16.9 
#>  8 Region 1   AR 3  2020-03-07    2020-03-08       1   117  118.  10.8 
#>  9 Region 1   AR 3  2020-03-07    2020-03-09       2   128  125.  11.2 
#> 10 Region 1   AR 3  2020-03-07    2020-03-10       3   151  150.  12.6 
#> # … with 591 more rows, and 4 more variables: bottom <dbl>, lower <dbl>,
#> #   upper <dbl>, top <dbl>
#> 
#> $case_scores
#> # A tibble: 601 x 15
#>    timeseries model sample forecast_date date       horizon   dss  crps
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-06    2020-03-07       1  4.81  3.39
#>  2 Region 1   AR 3  1      2020-03-06    2020-03-08       2  5.26  5.5 
#>  3 Region 1   AR 3  1      2020-03-06    2020-03-09       3  5.87  7.87
#>  4 Region 1   AR 3  1      2020-03-06    2020-03-10       4  5.46  5.33
#>  5 Region 1   AR 3  1      2020-03-06    2020-03-11       5  5.10  3.83
#>  6 Region 1   AR 3  1      2020-03-06    2020-03-12       6  4.71  3.86
#>  7 Region 1   AR 3  1      2020-03-06    2020-03-13       7  7.44 13.5 
#>  8 Region 1   AR 3  1      2020-03-07    2020-03-08       1  7.20 10.6 
#>  9 Region 1   AR 3  1      2020-03-07    2020-03-09       2  5.51  7.48
#> 10 Region 1   AR 3  1      2020-03-07    2020-03-10       3  5.53  6.19
#> # … with 591 more rows, and 7 more variables: logs <dbl>, bias <dbl>,
#> #   sharpness <dbl>, calibration <dbl>, median <dbl>, iqr <dbl>, ci <dbl>
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
#>    score  model      bottom   lower  median    mean   upper     top      sd
#>    <chr>  <chr>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 bias   AR 3     -1.00e+0 2.00e-1 6.00e-1 4.88e-1 1.00e+0 1.00e+0 5.67e-1
#>  2 bias   ARIMA    -1.00e+0 2.00e-1 8.00e-1 5.10e-1 1.00e+0 1.00e+0 6.17e-1
#>  3 bias   Semi-l…  -1.00e+0 2.00e-1 8.00e-1 5.19e-1 1.00e+0 1.00e+0 5.76e-1
#>  4 calib… AR 3      8.57e-5 8.57e-5 8.57e-5 3.22e-2 1.50e-4 5.67e-1 1.13e-1
#>  5 calib… ARIMA     8.57e-5 8.57e-5 8.57e-5 3.29e-2 1.50e-4 3.75e-1 9.60e-2
#>  6 calib… Semi-l…   8.57e-5 8.57e-5 8.57e-5 7.96e-2 2.00e-4 7.80e-1 1.99e-1
#>  7 ci     AR 3      2.89e+1 4.71e+1 1.10e+2 1.14e+3 1.92e+3 5.50e+3 1.79e+3
#>  8 ci     ARIMA     2.50e+1 4.54e+1 1.13e+2 1.04e+3 1.56e+3 6.01e+3 1.71e+3
#>  9 ci     Semi-l…   2.69e+1 5.14e+1 1.14e+2 9.40e+2 1.16e+3 5.93e+3 1.58e+3
#> 10 crps   AR 3      2.97e+0 6.26e+0 1.72e+1 4.47e+1 7.62e+1 1.73e+2 5.53e+1
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
