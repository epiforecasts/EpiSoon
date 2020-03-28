
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

  - Define the list of `bsts` models to be compared.

<!-- end list -->

``` r
models <- list("AR 3" =
                  function(ss, y){bsts::AddAr(ss, y = y, lags = 3)},
               "Semi-local linear trend" =
                  function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
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
#> # A tibble: 525 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  1.74   1.61 0.618   0.802
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  1.25   1.20 1.07    0    
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  0.965  1.07 1.15    0    
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  1.18   1.23 1.26    0    
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  1.28   1.41 1.61    0    
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  1.78   1.67 1.32    0    
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  2.03   1.96 1.40    0    
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1  2.20   2.20 0.0402  2.15 
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2  2.13   2.13 0.0432  2.04 
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3  2.08   2.08 0.0371  2.03 
#> # … with 515 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $rt_scores
#> # A tibble: 415 x 10
#>    timeseries model forecast_date date       horizon    dss    crps  logs  bias
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl>   <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1  0.189 0.436    1.05 0.100
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2  0.981 0.631    1.47 0.200
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3  1.15  0.786    1.75 0.200
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4  0.885 0.616    1.63 0.200
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5  1.03  0.677    1.84 0.200
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  0.529 0.416    1.51 0.5  
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  0.571 0.240    1.04 0.5  
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1 -6.43  0.00756 -2.74 0.6  
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2 -6.28  0.00974 -2.52 0.3  
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3 -6.27  0.0167  -1.93 0.3  
#> # … with 405 more rows, and 1 more variable: sharpness <dbl>
#> 
#> $forecast_cases
#> # A tibble: 415 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1   40    40.5 17.8      15
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2   36    40   36.2       0
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3   37.5  39.7 41.9       0
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4   38.5  47.4 49.1       0
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5   49.5  67   82.5       0
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6   68    83.8 86.5       0
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7   87.5  94.8 84.1       0
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1   65.5  65    7.09     52
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2   78    78.3  3.86     72
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3   92.5  93.4  9.25     75
#> # … with 405 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $case_scores
#> # A tibble: 415 x 11
#>    timeseries model sample forecast_date date       horizon   dss  crps  logs
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-04    2020-03-05       1  7.43 15.5   4.81
#>  2 Region 1   AR 3  1      2020-03-04    2020-03-06       2  8.00 21.5   5.05
#>  3 Region 1   AR 3  1      2020-03-04    2020-03-07       3  8.84 33.5   5.42
#>  4 Region 1   AR 3  1      2020-03-04    2020-03-08       4  9.06 34.5   5.38
#>  5 Region 1   AR 3  1      2020-03-04    2020-03-09       5  9.11 46.9   6.35
#>  6 Region 1   AR 3  1      2020-03-04    2020-03-10       6  9.31 42.9   5.88
#>  7 Region 1   AR 3  1      2020-03-04    2020-03-11       7  9.58 47.2   5.86
#>  8 Region 1   AR 3  1      2020-03-05    2020-03-06       1  5.23  5.04  3.51
#>  9 Region 1   AR 3  1      2020-03-05    2020-03-07       2  9.61  7.67  5.37
#> 10 Region 1   AR 3  1      2020-03-05    2020-03-08       3  5.30  4.96  3.54
#> # … with 405 more rows, and 2 more variables: bias <dbl>, sharpness <dbl>
#> 
#> $raw_rt_forecast
#> # A tibble: 5,250 x 8
#>    timeseries model obs_sample forecast_date sample date          rt horizon
#>    <chr>      <chr> <chr>      <chr>          <int> <date>     <dbl>   <int>
#>  1 Region 1   AR 3  1          2020-03-04         1 2020-03-05 1.76        1
#>  2 Region 1   AR 3  1          2020-03-04         2 2020-03-05 1.71        1
#>  3 Region 1   AR 3  1          2020-03-04         3 2020-03-05 0.802       1
#>  4 Region 1   AR 3  1          2020-03-04         4 2020-03-05 2.09        1
#>  5 Region 1   AR 3  1          2020-03-04         5 2020-03-05 1.88        1
#>  6 Region 1   AR 3  1          2020-03-04         6 2020-03-05 2.78        1
#>  7 Region 1   AR 3  1          2020-03-04         7 2020-03-05 1.91        1
#>  8 Region 1   AR 3  1          2020-03-04         8 2020-03-05 1.17        1
#>  9 Region 1   AR 3  1          2020-03-04         9 2020-03-05 1.05        1
#> 10 Region 1   AR 3  1          2020-03-04        10 2020-03-05 0.929       1
#> # … with 5,240 more rows
#> 
#> $raw_case_forecast
#> # A tibble: 4,150 x 8
#>    timeseries model obs_sample forecast_date sample date       cases horizon
#>    <chr>      <chr> <chr>      <chr>          <dbl> <date>     <int>   <int>
#>  1 Region 1   AR 3  1          2020-03-04         1 2020-03-05    59       1
#>  2 Region 1   AR 3  1          2020-03-04         1 2020-03-06     9       2
#>  3 Region 1   AR 3  1          2020-03-04         1 2020-03-07     0       3
#>  4 Region 1   AR 3  1          2020-03-04         1 2020-03-08     0       4
#>  5 Region 1   AR 3  1          2020-03-04         1 2020-03-09    41       5
#>  6 Region 1   AR 3  1          2020-03-04         1 2020-03-10    60       6
#>  7 Region 1   AR 3  1          2020-03-04         1 2020-03-11   115       7
#>  8 Region 1   AR 3  1          2020-03-04         2 2020-03-05    43       1
#>  9 Region 1   AR 3  1          2020-03-04         2 2020-03-06    51       2
#> 10 Region 1   AR 3  1          2020-03-04         2 2020-03-07    59       3
#> # … with 4,140 more rows
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
#>  1 bias      AR 3                    0     0     0.100  0.287  0.5    1    0.371
#>  2 bias      Semi-local linear tr…   0     0     0.100  0.298  0.5    1    0.355
#>  3 crps      AR 3                    3.54 10.7  22.1   34.2   39.3  157.  37.5  
#>  4 crps      Semi-local linear tr…   2.93  9.39 18.6   32.9   37.2  164.  39.5  
#>  5 dss       AR 3                    5.02  7.06  9.50  14.1   15.4   52.4 14.2  
#>  6 dss       Semi-local linear tr…   4.52  6.77  8.46  13.0   13.6   40.1 15.0  
#>  7 logs      AR 3                    3.46  4.43  5.30  11.6    7.77  75.1 21.6  
#>  8 logs      Semi-local linear tr…   3.32  4.31  5.04   8.88   7.25  39.5 13.6  
#>  9 sharpness AR 3                    4.45 10.4  16.3   19.4   25.2   55.4 13.8  
#> 10 sharpness Semi-local linear tr…   5.19 11.1  15.6   19.8   26.7   51.9 12.3
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
