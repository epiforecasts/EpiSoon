
# EpiSoon

[![DOI](https://zenodo.org/badge/248311916.svg)](https://zenodo.org/badge/latestdoi/248311916)
[![Build
Status](https://travis-ci.com/epiforecasts/EpiSoon.svg?branch=master)](https://travis-ci.com/epiforecasts/EpiSoon)

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
#> # A tibble: 784 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-05    2020-03-06       1   2.20  2.20 0.0422
#>  2 Region 1   AR 3  2020-03-05    2020-03-07       2   2.15  2.15 0.0382
#>  3 Region 1   AR 3  2020-03-05    2020-03-08       3   2.09  2.08 0.0611
#>  4 Region 1   AR 3  2020-03-05    2020-03-09       4   2.01  2.01 0.0541
#>  5 Region 1   AR 3  2020-03-05    2020-03-10       5   1.95  1.95 0.0331
#>  6 Region 1   AR 3  2020-03-05    2020-03-11       6   1.91  1.91 0.0444
#>  7 Region 1   AR 3  2020-03-05    2020-03-12       7   1.85  1.85 0.0375
#>  8 Region 1   AR 3  2020-03-06    2020-03-07       1   2.09  2.10 0.0364
#>  9 Region 1   AR 3  2020-03-06    2020-03-08       2   2.02  2.02 0.0158
#> 10 Region 1   AR 3  2020-03-06    2020-03-09       3   1.94  1.94 0.0261
#> # … with 774 more rows, and 4 more variables: bottom <dbl>, lower <dbl>,
#> #   upper <dbl>, top <dbl>
#> 
#> $rt_scores
#> # A tibble: 616 x 14
#>    timeseries model forecast_date date       horizon    dss    crps   logs
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl>   <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-05    2020-03-06       1 -6.43  0.00837 -2.50 
#>  2 Region 1   AR 3  2020-03-05    2020-03-07       2 -6.61  0.00733 -2.74 
#>  3 Region 1   AR 3  2020-03-05    2020-03-08       3 -5.44  0.0117  -2.43 
#>  4 Region 1   AR 3  2020-03-05    2020-03-09       4 -5.16  0.0337   1.22 
#>  5 Region 1   AR 3  2020-03-05    2020-03-10       5 -0.766 0.0601  -0.295
#>  6 Region 1   AR 3  2020-03-05    2020-03-11       6  0.361 0.0853   0.418
#>  7 Region 1   AR 3  2020-03-05    2020-03-12       7 10.5   0.128   15.8  
#>  8 Region 1   AR 3  2020-03-06    2020-03-07       1 -4.82  0.0378   0.634
#>  9 Region 1   AR 3  2020-03-06    2020-03-08       2 23.8   0.0774  25.4  
#> 10 Region 1   AR 3  2020-03-06    2020-03-09       3 14.5   0.102    7.65 
#> # … with 606 more rows, and 6 more variables: bias <dbl>, sharpness <dbl>,
#> #   calibration <dbl>, median <dbl>, iqr <dbl>, ci <dbl>
#> 
#> $forecast_cases
#> # A tibble: 616 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  2020-03-05    2020-03-06       1   84    81.7 12.0 
#>  2 Region 1   AR 3  2020-03-05    2020-03-07       2  101   101.   7.16
#>  3 Region 1   AR 3  2020-03-05    2020-03-08       3  114.  114.  13.8 
#>  4 Region 1   AR 3  2020-03-05    2020-03-09       4  130.  133.  17.5 
#>  5 Region 1   AR 3  2020-03-05    2020-03-10       5  152   156.  18.0 
#>  6 Region 1   AR 3  2020-03-05    2020-03-11       6  187   184   24.0 
#>  7 Region 1   AR 3  2020-03-05    2020-03-12       7  202.  212.  33.1 
#>  8 Region 1   AR 3  2020-03-06    2020-03-07       1   93    91.2  8.98
#>  9 Region 1   AR 3  2020-03-06    2020-03-08       2   94.5  97.3  8.71
#> 10 Region 1   AR 3  2020-03-06    2020-03-09       3  118   116.  10.7 
#> # … with 606 more rows, and 4 more variables: bottom <dbl>, lower <dbl>,
#> #   upper <dbl>, top <dbl>
#> 
#> $case_scores
#> # A tibble: 616 x 15
#>    timeseries model sample forecast_date date       horizon   dss  crps
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-05    2020-03-06       1  5.45  6.13
#>  2 Region 1   AR 3  1      2020-03-05    2020-03-07       2  7.38  8.96
#>  3 Region 1   AR 3  1      2020-03-05    2020-03-08       3  5.96  7.5 
#>  4 Region 1   AR 3  1      2020-03-05    2020-03-09       4  6.69  9.02
#>  5 Region 1   AR 3  1      2020-03-05    2020-03-10       5  6.41  7.77
#>  6 Region 1   AR 3  1      2020-03-05    2020-03-11       6  6.81 12.4 
#>  7 Region 1   AR 3  1      2020-03-05    2020-03-12       7  7.21  7.99
#>  8 Region 1   AR 3  1      2020-03-06    2020-03-07       1  4.43  2.98
#>  9 Region 1   AR 3  1      2020-03-06    2020-03-08       2  4.55  3.51
#> 10 Region 1   AR 3  1      2020-03-06    2020-03-09       3  4.64  2.86
#> # … with 606 more rows, and 7 more variables: logs <dbl>, bias <dbl>,
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
#>  1 bias   AR 3     -1.00e+0 1.00e-1 7.00e-1 4.81e-1 1.00e+0 1.00e+0 6.01e-1
#>  2 bias   ARIMA    -1.00e+0 2.00e-1 8.00e-1 5.37e-1 1.00e+0 1.00e+0 5.79e-1
#>  3 bias   Semi-l…  -1.00e+0 2.00e-1 6.00e-1 4.87e-1 1.00e+0 1.00e+0 5.79e-1
#>  4 calib… AR 3      8.57e-5 8.57e-5 8.57e-5 2.83e-2 1.20e-4 3.16e-1 8.06e-2
#>  5 calib… ARIMA     8.57e-5 8.57e-5 8.57e-5 3.80e-2 2.00e-4 4.12e-1 9.34e-2
#>  6 calib… Semi-l…   8.57e-5 8.57e-5 9.29e-5 7.22e-2 6.04e-3 9.11e-1 2.01e-1
#>  7 ci     AR 3      2.64e+1 5.07e+1 9.99e+1 1.18e+3 2.05e+3 6.79e+3 1.94e+3
#>  8 ci     ARIMA     2.25e+1 4.55e+1 9.51e+1 1.09e+3 2.02e+3 6.14e+3 1.77e+3
#>  9 ci     Semi-l…   2.61e+1 5.30e+1 9.60e+1 1.07e+3 1.76e+3 5.29e+3 1.71e+3
#> 10 crps   AR 3      2.84e+0 6.37e+0 1.66e+1 4.55e+1 7.54e+1 2.03e+2 5.76e+1
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
