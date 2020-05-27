
# EpiSoon

[![R build
status](https://github.com/epiforecasts/EpiSoon/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiSoon)
[![Docker
Pulls](https://img.shields.io/docker/pulls/seabbs/episoon)](https://hub.docker.com/repository/docker/seabbs/episoon)
[![DOI](https://zenodo.org/badge/248311916.svg)](https://zenodo.org/badge/latestdoi/248311916)

This package provides tooling to forecast the time-varying reproduction
number and use this to forecast reported case counts via a branching
process. It supports a range of time series modelling packages including
`bsts`, `forecast`, and `fable`. It also supports ensembles via `stackr`
and `forecastHyrbid`. Forecasts can be assessed by iteractively fitting
and then using proper scoring rules (via `scoringutils` and
`scoringRules`) to compare to both observed case counts and estimated
reproduction numbers. Whilst `EpiSoon` is primarily developed to be used
in tandem with `EpiNow` it can also be used as a standalone package.

## Installation

Install the stable version of the package using
[`{drat}`](https://epiforecasts.io/drat/):

``` r
install.packages("drat")
drat::add("epiforecasts")
install.packages("EpiSoon")
```

Install the development version of the package with:

``` r
remotes::install_github("epiforecasts/EpiSoon")
```

## Quick start

  - Load packages (`bsts` and `fable` for models, `ggplot2` for
    plotting, and `cowplot` for theming)

<!-- end list -->

``` r
library(EpiSoon)
library(bsts)
library(fable)
#> Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
#> had status 1
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
#> Warning: Unknown or uninitialised column: `sample`.

#> Warning: Unknown or uninitialised column: `sample`.

forecasts
#> $forecast_rts
#> # A tibble: 532 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1 0      0.548 0.966      0
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2 0.0701 1.53  2.10       0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3 0      0.830 1.54       0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4 0.283  1.23  1.73       0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5 0      0.874 1.58       0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6 0      0.492 0.913      0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7 0.822  2.39  3.51       0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1 0.149  0.547 0.956      0
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2 0.835  2.22  3.45       0
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3 0      0.475 0.788      0
#> # … with 522 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $rt_scores
#> # A tibble: 420 x 14
#>    timeseries model forecast_date date       horizon   dss  crps  logs   bias
#>    <chr>      <chr> <chr>         <date>       <int> <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  3.34 1.41   2.62 -0.8  
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  1.49 0.943  2.20 -0.400
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  1.57 1.17   2.23 -0.6  
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  1.28 0.906  2.26 -0.6  
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  1.43 1.17   3.92 -0.6  
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  2.86 1.31   3.80 -0.8  
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  2.42 0.779  1.97 -0.200
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1  3.10 1.43   6.29 -0.8  
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2  2.37 0.804  2.00 -0.200
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3  4.17 1.28   1.46 -1    
#> # … with 410 more rows, and 5 more variables: sharpness <dbl>,
#> #   calibration <dbl>, median <dbl>, iqr <dbl>, ci <dbl>
#> 
#> $forecast_cases
#> # A tibble: 420 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1    0    17.3  32.0      0
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2    1.5  55.6  77.4      0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3    0    33.4  62.4      0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4    7    61.6  93.4      0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5    0    26.9  49.5      0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6    0    25.4  48.6      0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7   12.5  86.9 150.       0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1    3.5  17.7  32.3      0
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2   43    93.7 142.       0
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3    0    16    27.1      0
#> # … with 410 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $case_scores
#> # A tibble: 420 x 15
#>    timeseries model sample forecast_date date       horizon   dss  crps  logs
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-04    2020-03-05       1  9.09  39.0  6.68
#>  2 Region 1   AR 3  1      2020-03-04    2020-03-06       2  8.65  32.3  5.74
#>  3 Region 1   AR 3  1      2020-03-04    2020-03-07       3  9.01  48.7  5.94
#>  4 Region 1   AR 3  1      2020-03-04    2020-03-08       4  9.18  48.8  6.29
#>  5 Region 1   AR 3  1      2020-03-04    2020-03-09       5 11.3   72.1  6.18
#>  6 Region 1   AR 3  1      2020-03-04    2020-03-10       6 14.0   98    6.13
#>  7 Region 1   AR 3  1      2020-03-04    2020-03-11       7 10.2   93.1  8.60
#>  8 Region 1   AR 3  1      2020-03-05    2020-03-06       1 10.1   48.4  9.74
#>  9 Region 1   AR 3  1      2020-03-05    2020-03-07       2  9.80  32.1  5.68
#> 10 Region 1   AR 3  1      2020-03-05    2020-03-08       3 17.7   74.1 11.6 
#> # … with 410 more rows, and 6 more variables: bias <dbl>, sharpness <dbl>,
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

<img src="man/figures/unnamed-chunk-6-1.png" width="60%" />

  - Plot an evaluation of case forecasts using iterative
fitting

<!-- end list -->

``` r
EpiSoon::plot_forecast_evaluation(forecasts$forecast_cases, obs_cases, c(7)) +
   ggplot2::facet_grid(model ~ timeseries, scales = "free") +
   cowplot::panel_border()
```

<img src="man/figures/unnamed-chunk-7-1.png" width="60%" />

  - Summarise the forecasts by model scored against observed cases

<!-- end list -->

``` r
EpiSoon::summarise_scores(forecasts$case_scores)
#> # A tibble: 18 x 9
#>    score  model   bottom    lower   median      mean    upper      top        sd
#>    <chr>  <chr>    <dbl>    <dbl>    <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#>  1 bias   AR 3  -1.00e+0 -8.00e-1 -8.00e-1   -0.662  -6.00e-1    0        0.263 
#>  2 bias   Semi… -1.00e+0  3.00e-1  8.00e-1    0.561   1.00e+0    1        0.548 
#>  3 calib… AR 3   8.57e-5  8.57e-5  8.57e-5    0.0240  3.68e-3    0.222    0.0860
#>  4 calib… Semi…  8.57e-5  8.57e-5  8.57e-5    0.0202  2.00e-4    0.340    0.0639
#>  5 ci     AR 3   1.51e+2  3.35e+2  5.95e+2 1382.      1.12e+3 9342.    2232.    
#>  6 ci     Semi…  2.59e+1  5.15e+1  1.41e+2 1162.      2.07e+3 5469.    1795.    
#>  7 crps   AR 3   3.21e+1  8.91e+1  1.40e+2  152.      1.84e+2  416.      93.7   
#>  8 crps   Semi…  2.79e+0  6.46e+0  1.67e+1   44.7     7.90e+1  177.      54.3   
#>  9 dss    AR 3   9.01e+0  1.07e+1  1.18e+1   14.8     1.33e+1   53.4     11.2   
#> 10 dss    Semi…  4.66e+0  6.03e+0  7.88e+0   14.8     1.57e+1   54.6     18.8   
#> 11 iqr    AR 3   1.16e+2  3.54e+2  6.03e+2  681.      8.63e+2 1894.     467.    
#> 12 iqr    Semi…  1.19e+1  2.81e+1  7.75e+1  191.      3.38e+2  716.     228.    
#> 13 logs   AR 3   5.70e+0  6.60e+0  7.23e+0  Inf       9.86e+0  Inf      Inf     
#> 14 logs   Semi…  3.27e+0  4.07e+0  4.82e+0   12.2     8.38e+0   71.4     30.2   
#> 15 median AR 3   6.72e+1  2.60e+2  4.16e+2  463.      5.92e+2 1176.     272.    
#> 16 median Semi…  2.00e+0  1.70e+1  5.00e+1  108.      1.86e+2  386.     124.    
#> 17 sharp… AR 3   0.       0.       1.85e+1   64.6     8.52e+1  348.     108.    
#> 18 sharp… Semi…  5.36e+0  1.26e+1  1.85e+1   24.5     3.02e+1   76.4     18.4
```

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiSoon/issues) if
you have identified an issue with the package. Please note that due to
operational constraints priority will be given to users informing
government policy or offering methodological insights. We welcome all
contributions, in particular those that improve the approach or the
robustness of the code base.

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
