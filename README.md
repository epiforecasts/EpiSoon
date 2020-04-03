
# EpiSoon

[![Travis-CI Build
Status](https://travis-ci.org/epiforecasts/EpiSoon.svg?branch=master)](https://travis-ci.org/epiforecasts/EpiSoon)

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

  - Load packages (`bsts` for models, `ggplot2` for plotting, and
    `cowplot` for theming)

<!-- end list -->

``` r
library(EpiSoon)
library(bsts)
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
                    function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)})
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

forecasts
#> $forecast_rts
#> # A tibble: 497 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  2.22  2.21  1.07     0   
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  1.54  1.73  1.52     0   
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  1.78  1.73  1.46     0   
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  1.25  1.51  1.79     0   
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  1.06  1.16  1.11     0   
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  0.946 0.864 0.845    0   
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  0.640 0.969 1.06     0   
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1  2.22  2.22  0.0287   2.19
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2  2.14  2.14  0.0136   2.12
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3  2.07  2.08  0.0251   2.02
#> # … with 487 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $rt_scores
#> # A tibble: 385 x 10
#>    timeseries model forecast_date date       horizon     dss    crps   logs
#>    <chr>      <chr> <chr>         <date>       <int>   <dbl>   <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  0.0257 0.209    0.972
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  0.836  0.422    1.26 
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  0.746  0.408    1.47 
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  1.18   0.585    1.57 
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  0.836  0.618    1.55 
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  1.66   0.796    1.56 
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  1.09   0.774    1.79 
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1 -6.31   0.0135  -2.43 
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2 -8.53   0.00358 -3.23 
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3 -6.01   0.0176  -2.24 
#> # … with 375 more rows, and 2 more variables: bias <dbl>, sharpness <dbl>
#> 
#> $forecast_cases
#> # A tibble: 385 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1   58.5  57.8  27.2       0
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2   44.5  54.7  48.1       0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3   70.5  66.2  55.9       0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4   65    80.1 106.        0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5   62    56.1  49.7       0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6   39.5  47.2  51.7       0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7   57    65.9  71.7       0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1   65.5  66.4   6.79     56
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2   78.5  77.7   6.91     64
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3   86.5  89.3   9.49     76
#> # … with 375 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $case_scores
#> # A tibble: 385 x 11
#>    timeseries model sample forecast_date date       horizon   dss  crps  logs
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-04    2020-03-05       1  6.54  5.18  4.02
#>  2 Region 1   AR 3  1      2020-03-04    2020-03-06       2  7.80 15.0   4.92
#>  3 Region 1   AR 3  1      2020-03-04    2020-03-07       3  8.11 16.9   5.16
#>  4 Region 1   AR 3  1      2020-03-04    2020-03-08       4  9.26 28.7   5.36
#>  5 Region 1   AR 3  1      2020-03-04    2020-03-09       5  9.32 38.2   5.48
#>  6 Region 1   AR 3  1      2020-03-04    2020-03-10       6 11.5  69.6   6.21
#>  7 Region 1   AR 3  1      2020-03-04    2020-03-11       7 10.7  71.0   6.17
#>  8 Region 1   AR 3  1      2020-03-05    2020-03-06       1  4.78  4.18  3.39
#>  9 Region 1   AR 3  1      2020-03-05    2020-03-07       2  6.23  6.91  3.86
#> 10 Region 1   AR 3  1      2020-03-05    2020-03-08       3  6.39  8.49  3.96
#> # … with 375 more rows, and 2 more variables: bias <dbl>, sharpness <dbl>
#> 
#> $raw_rt_forecast
#> # A tibble: 4,970 x 8
#>    timeseries model obs_sample forecast_date sample date          rt horizon
#>    <chr>      <chr> <chr>      <chr>          <int> <date>     <dbl>   <int>
#>  1 Region 1   AR 3  1          2020-03-04         1 2020-03-05  2.29       1
#>  2 Region 1   AR 3  1          2020-03-04         2 2020-03-05  2.15       1
#>  3 Region 1   AR 3  1          2020-03-04         3 2020-03-05  1.59       1
#>  4 Region 1   AR 3  1          2020-03-04         4 2020-03-05  2.54       1
#>  5 Region 1   AR 3  1          2020-03-04         5 2020-03-05  3.03       1
#>  6 Region 1   AR 3  1          2020-03-04         6 2020-03-05  3.03       1
#>  7 Region 1   AR 3  1          2020-03-04         7 2020-03-05  0          1
#>  8 Region 1   AR 3  1          2020-03-04         8 2020-03-05  1.92       1
#>  9 Region 1   AR 3  1          2020-03-04         9 2020-03-05  1.61       1
#> 10 Region 1   AR 3  1          2020-03-04        10 2020-03-05  3.96       1
#> # … with 4,960 more rows
#> 
#> $raw_case_forecast
#> # A tibble: 3,850 x 8
#>    timeseries model obs_sample forecast_date sample date       cases horizon
#>    <chr>      <chr> <chr>      <chr>          <dbl> <date>     <int>   <int>
#>  1 Region 1   AR 3  1          2020-03-04         1 2020-03-05    52       1
#>  2 Region 1   AR 3  1          2020-03-04         1 2020-03-06    84       2
#>  3 Region 1   AR 3  1          2020-03-04         1 2020-03-07    95       3
#>  4 Region 1   AR 3  1          2020-03-04         1 2020-03-08   100       4
#>  5 Region 1   AR 3  1          2020-03-04         1 2020-03-09   120       5
#>  6 Region 1   AR 3  1          2020-03-04         1 2020-03-10   151       6
#>  7 Region 1   AR 3  1          2020-03-04         1 2020-03-11   195       7
#>  8 Region 1   AR 3  1          2020-03-04         2 2020-03-05    62       1
#>  9 Region 1   AR 3  1          2020-03-04         2 2020-03-06    72       2
#> 10 Region 1   AR 3  1          2020-03-04         2 2020-03-07    65       3
#> # … with 3,840 more rows
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
#>  1 bias      AR 3                    0     0     0.100  0.321  0.7    1    0.375
#>  2 bias      Semi-local linear tr…   0     0     0.200  0.311  0.6    1    0.352
#>  3 crps      AR 3                    4.42 11.5  21.1   37.1   46.0  160.  40.5  
#>  4 crps      Semi-local linear tr…   2.38  8.91 17.8   32.1   37.9  158.  38.5  
#>  5 dss       AR 3                    5.16  7.10  8.99  14.6   16.1   64.5 15.1  
#>  6 dss       Semi-local linear tr…   4.27  6.54  8.13  12.9   11.7   62.5 14.9  
#>  7 logs      AR 3                    3.57  4.48  5.02  10.2    8.36  45.6 16.9  
#>  8 logs      Semi-local linear tr…   3.13  4.28  4.92   9.23   6.34  66.3 15.0  
#>  9 sharpness AR 3                    5.93 11.9  18.5   21.5   27.4   57.0 13.4  
#> 10 sharpness Semi-local linear tr…   5.19 11.1  17.0   20.5   25.8   57.5 13.5
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
