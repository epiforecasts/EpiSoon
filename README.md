
# EpiSoon

[![R build
status](https://github.com/epiforecasts/EpiSoon/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiSoon)
![Docker Pulls](https://img.shields.io/docker/pulls/seabbs/episoon)
[![DOI](https://zenodo.org/badge/248311916.svg)](https://zenodo.org/badge/latestdoi/248311916)

**Aim:** To forecast the time-varying reproduction number and use this
to forecast reported case counts.

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
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  0.186 0.467 0.595      0
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  0     1.76  2.35       0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  0.403 3.38  8.90       0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  0.731 1.20  1.48       0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  0     1.18  1.88       0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  1.43  2.86  5.20       0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  0     0.415 0.810      0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1  0     0.596 0.972      0
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2  0     0.861 1.68       0
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3  0.923 2.22  2.75       0
#> # … with 522 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $rt_scores
#> # A tibble: 420 x 14
#>    timeseries model forecast_date date       horizon   dss  crps  logs   bias
#>    <chr>      <chr> <chr>         <date>       <int> <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  9.03 1.50   3.35 -1    
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  1.64 1.06   2.31 -0.200
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  4.29 1.25   2.22 -0.8  
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  1.09 0.835  2.17 -0.6  
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  1.40 0.932  2.13 -0.6  
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  3.22 0.767  1.85  0    
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  3.81 1.37   3.71 -0.8  
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1  2.84 1.20   1.81 -0.8  
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2  1.58 1.28   3.54 -0.8  
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3  1.92 0.891  2.17 -0.200
#> # … with 410 more rows, and 5 more variables: sharpness <dbl>,
#> #   calibration <dbl>, median <dbl>, iqr <dbl>, ci <dbl>
#> 
#> $forecast_cases
#> # A tibble: 420 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1    5.5  15.7   20.6      0
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2    0    63.1   84.3      0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3   16   114.   301.       0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4   15    47.5   82.4      0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5    0    43.2   94.1      0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6   27   356.  1043.       0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7    0    40.9  115.       0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1    0    23.1   38.3      0
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2    0    34.2   68.7      0
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3   44    76     88.9      0
#> # … with 410 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $case_scores
#> # A tibble: 420 x 15
#>    timeseries model sample forecast_date date       horizon   dss  crps   logs
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  1      2020-03-04    2020-03-05       1 11.8   37.2   5.62
#>  2 Region 1   AR 3  1      2020-03-04    2020-03-06       2  8.78  37.1   5.85
#>  3 Region 1   AR 3  1      2020-03-04    2020-03-07       3 11.3   55.6   6.52
#>  4 Region 1   AR 3  1      2020-03-04    2020-03-08       4  9.20  55.7   7.09
#>  5 Region 1   AR 3  1      2020-03-04    2020-03-09       5  9.65  76    10.7 
#>  6 Region 1   AR 3  1      2020-03-04    2020-03-10       6 13.8  114.   10.2 
#>  7 Region 1   AR 3  1      2020-03-04    2020-03-11       7 10.7  130.  190.  
#>  8 Region 1   AR 3  1      2020-03-05    2020-03-06       1  9.07  38.2   5.46
#>  9 Region 1   AR 3  1      2020-03-05    2020-03-07       2  9.04  53.9   7.69
#> 10 Region 1   AR 3  1      2020-03-05    2020-03-08       3  8.96  33.6   5.80
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
#>    score model   bottom    lower   median      mean    upper       top        sd
#>    <chr> <chr>    <dbl>    <dbl>    <dbl>     <dbl>    <dbl>     <dbl>     <dbl>
#>  1 bias  AR 3  -1.00e+0 -8.00e-1 -8.00e-1   -0.666  -6.00e-1   -0.0450    0.254 
#>  2 bias  Semi… -1.00e+0  2.00e-1  7.00e-1    0.507   1.00e+0    1         0.557 
#>  3 cali… AR 3   8.57e-5  8.57e-5  8.57e-5    0.0168  3.68e-3    0.204     0.0602
#>  4 cali… Semi…  8.57e-5  8.57e-5  8.57e-5    0.0295  2.00e-4    0.298     0.0772
#>  5 ci    AR 3   1.41e+2  3.77e+2  6.20e+2 1504.      1.06e+3 9395.     2998.    
#>  6 ci    Semi…  2.82e+1  5.27e+1  1.19e+2  918.      1.28e+3 5204.     1557.    
#>  7 crps  AR 3   3.65e+1  8.41e+1  1.35e+2  157.      1.96e+2  421.      103.    
#>  8 crps  Semi…  3.01e+0  7.75e+0  1.69e+1   41.4     6.77e+1  166.       48.5   
#>  9 dss   AR 3   9.04e+0  1.07e+1  1.18e+1   15.3     1.32e+1   38.9      17.6   
#> 10 dss   Semi…  4.64e+0  6.34e+0  7.96e+0   12.9     1.39e+1   44.9      13.9   
#> 11 iqr   AR 3   1.44e+2  3.27e+2  5.53e+2  706.      8.78e+2 2024.      510.    
#> 12 iqr   Semi…  1.21e+1  3.36e+1  7.54e+1  182.      3.15e+2  742.      209.    
#> 13 logs  AR 3   5.77e+0  6.69e+0  7.25e+0  Inf       1.07e+1  Inf       Inf     
#> 14 logs  Semi…  3.39e+0  4.19e+0  4.81e+0    9.53    7.41e+0   34.8      18.2   
#> 15 medi… AR 3   5.64e+1  2.79e+2  4.18e+2  479.      6.56e+2 1211       280.    
#> 16 medi… Semi…  3.00e+0  1.90e+1  4.90e+1  103.      1.75e+2  396.      114.    
#> 17 shar… AR 3   0.       0.       1.37e+1   51.5     7.54e+1  247.       75.3   
#> 18 shar… Semi…  6.67e+0  1.33e+1  2.22e+1   26.3     3.48e+1   67.5      17.2
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
