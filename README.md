
# EpiSoon

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
                  function(...){fable_model(model = fable::ARIMA(y ~ time), ...)})
```

  - Compare models across timeseries (change the `future::plan` to do
    this in parallel).

<!-- end list -->

``` r
future::plan("sequential")

## Compare models
forecasts <- compare_timeseries(obs_rts, obs_cases, models,
                                   horizon = 7, samples = 10,
                                   serial_interval = EpiSoon::example_serial_interval)
#> Warning: Unknown or uninitialised column: 'sample'.

#> Warning: Unknown or uninitialised column: 'sample'.

forecasts
#> $forecast_rts
#> # A tibble: 504 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-05    2020-03-06       1   2.19  2.19 0.0156   2.17
#>  2 Region 1   AR 3  2020-03-05    2020-03-07       2   2.13  2.13 0.0331   2.07
#>  3 Region 1   AR 3  2020-03-05    2020-03-08       3   2.08  2.07 0.0450   1.96
#>  4 Region 1   AR 3  2020-03-05    2020-03-09       4   2.02  2.01 0.0604   1.90
#>  5 Region 1   AR 3  2020-03-05    2020-03-10       5   1.96  1.95 0.0685   1.81
#>  6 Region 1   AR 3  2020-03-05    2020-03-11       6   1.92  1.90 0.0738   1.77
#>  7 Region 1   AR 3  2020-03-05    2020-03-12       7   1.84  1.83 0.0790   1.69
#>  8 Region 1   AR 3  2020-03-06    2020-03-07       1   2.13  2.13 0.0224   2.09
#>  9 Region 1   AR 3  2020-03-06    2020-03-08       2   2.06  2.05 0.0174   2.01
#> 10 Region 1   AR 3  2020-03-06    2020-03-09       3   2.00  2.00 0.0273   1.97
#> # … with 494 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $rt_scores
#> # A tibble: 398 x 10
#>    timeseries model forecast_date date       horizon     dss    crps   logs
#>    <chr>      <chr> <chr>         <date>       <int>   <dbl>   <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-05    2020-03-06       1 -8.42   0.00415 -2.96 
#>  2 Region 1   AR 3  2020-03-05    2020-03-07       2 -6.70   0.00906 -2.37 
#>  3 Region 1   AR 3  2020-03-05    2020-03-08       3 -5.55   0.0177  -2.11 
#>  4 Region 1   AR 3  2020-03-05    2020-03-09       4 -5.06   0.0284  -1.59 
#>  5 Region 1   AR 3  2020-03-05    2020-03-10       5 -4.14   0.0482  -1.09 
#>  6 Region 1   AR 3  2020-03-05    2020-03-11       6 -2.55   0.0824  -0.439
#>  7 Region 1   AR 3  2020-03-05    2020-03-12       7 -0.0455 0.129    0.147
#>  8 Region 1   AR 3  2020-03-06    2020-03-07       1 -7.29   0.0105  -1.89 
#>  9 Region 1   AR 3  2020-03-06    2020-03-08       2  2.29   0.0453   3.59 
#> 10 Region 1   AR 3  2020-03-06    2020-03-09       3 -2.05   0.0491  -0.826
#> # … with 388 more rows, and 2 more variables: bias <dbl>, sharpness <dbl>
#> 
#> $forecast_cases
#> # A tibble: 398 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-05    2020-03-06       1   66.5  66.9  8.46     54
#>  2 Region 1   AR 3  2020-03-05    2020-03-07       2   80.5  78.9 11.5      59
#>  3 Region 1   AR 3  2020-03-05    2020-03-08       3   88    89.3  9.84     76
#>  4 Region 1   AR 3  2020-03-05    2020-03-09       4  101    98.6 10.5      82
#>  5 Region 1   AR 3  2020-03-05    2020-03-10       5  110.  110.  14.0      93
#>  6 Region 1   AR 3  2020-03-05    2020-03-11       6  131   132.  16.8     112
#>  7 Region 1   AR 3  2020-03-05    2020-03-12       7  132.  134.  12.3     119
#>  8 Region 1   AR 3  2020-03-06    2020-03-07       1   76.5  78.6  9.56     63
#>  9 Region 1   AR 3  2020-03-06    2020-03-08       2   88.5  88.9  5.67     82
#> 10 Region 1   AR 3  2020-03-06    2020-03-09       3  104   105.   8.95     95
#> # … with 388 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $case_scores
#> # A tibble: 398 x 11
#>    timeseries model sample forecast_date date       horizon   dss  crps  logs
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-05    2020-03-06       1  4.74  3.93  3.44
#>  2 Region 1   AR 3  1      2020-03-05    2020-03-07       2  5.48  4.21  3.57
#>  3 Region 1   AR 3  1      2020-03-05    2020-03-08       3  6.32  9.21  4.28
#>  4 Region 1   AR 3  1      2020-03-05    2020-03-09       4  7.65 11.8   4.30
#>  5 Region 1   AR 3  1      2020-03-05    2020-03-10       5 10.7  23.8   5.35
#>  6 Region 1   AR 3  1      2020-03-05    2020-03-11       6 10.5  26.8   5.29
#>  7 Region 1   AR 3  1      2020-03-05    2020-03-12       7 31.2  53.7  21.2 
#>  8 Region 1   AR 3  1      2020-03-06    2020-03-07       1  5.48  6.18  3.87
#>  9 Region 1   AR 3  1      2020-03-06    2020-03-08       2  9.30 10.1   4.52
#> 10 Region 1   AR 3  1      2020-03-06    2020-03-09       3  5.84  6.82  3.75
#> # … with 388 more rows, and 2 more variables: bias <dbl>, sharpness <dbl>
```

  - Plot an evaluation of Rt forecasts using iterative fitting.

<!-- end list -->

``` r
plot_forecast_evaluation(forecasts$forecast_rts, obs_rts, c(7)) +
   ggplot2::facet_grid(model ~ timeseries) +
   cowplot::panel_border()
```

<img src="man/figures/unnamed-chunk-7-1.png" width="60%" />

  - Plot an evaluation of case forecasts using iterative fitting

<!-- end list -->

``` r
plot_forecast_evaluation(forecasts$forecast_cases, obs_cases, c(7)) +
   ggplot2::facet_grid(model ~ timeseries, scales = "free") +
   cowplot::panel_border()
```

<img src="man/figures/unnamed-chunk-8-1.png" width="60%" />

  - Summarise the forecasts by model scored against observed cases

<!-- end list -->

``` r
summarise_scores(forecasts$case_scores)
#> # A tibble: 10 x 9
#>    score     model                 bottom lower median   mean upper   top     sd
#>    <chr>     <chr>                  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>
#>  1 bias      AR 3                    0     0     0.100  0.279  0.4    1    0.370
#>  2 bias      Semi-local linear tr…   0     0     0.100  0.290  0.5    1    0.346
#>  3 crps      AR 3                    3.88 10.3  20.3   35.2   40.9  158.  39.2  
#>  4 crps      Semi-local linear tr…   2.76  9.03 18.3   32.4   38.3  159.  38.6  
#>  5 dss       AR 3                    4.96  6.99  9.16  15.1   15.0   72.3 18.1  
#>  6 dss       Semi-local linear tr…   4.54  6.52  8.20  12.3   11.8   47.6 12.9  
#>  7 logs      AR 3                    3.45  4.43  5.23  11.1    8.35  59.5 23.5  
#>  8 logs      Semi-local linear tr…   3.24  4.32  5.03   9.89   6.54  80.3 17.9  
#>  9 sharpness AR 3                    5.19 11.1  14.8   18.1   23.7   43.0 10.7  
#> 10 sharpness Semi-local linear tr…   5.19 11.9  17.0   20.2   25.0   54.1 12.7
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
