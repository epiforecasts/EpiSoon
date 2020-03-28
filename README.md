
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
#> # A tibble: 532 x 12
#>    timeseries model forecast_date date       horizon median  mean     sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl>  <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1   2.25  2.25 0.0442   2.17
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2   2.17  2.18 0.0746   2.03
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3   2.09  2.10 0.0749   1.99
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4   2.03  2.05 0.0814   1.92
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5   1.95  1.98 0.138    1.82
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6   1.89  1.93 0.144    1.76
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7   1.84  1.86 0.150    1.66
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1   2.06  1.99 0.225    1.46
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2   2.02  2.08 0.296    1.74
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3   1.78  1.55 0.695    0   
#> # … with 522 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $rt_scores
#> # A tibble: 420 x 10
#>    timeseries model forecast_date date       horizon    dss   crps    logs  bias
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl>  <dbl>   <dbl> <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1 -6.16  0.0122 -2.00   0.200
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2 -5.28  0.0187 -1.62   0.4  
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3 -4.88  0.0348 -1.15   0.3  
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4 -4.56  0.0413 -1.05   0.200
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5 -3.70  0.0624 -0.671  0.200
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6 -3.45  0.0806 -0.295  0.200
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7 -2.78  0.107   0.0971 0.200
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1 -2.17  0.103  -0.539  0.100
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2 -2.49  0.0813 -0.286  0.3  
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3 -0.117 0.234   0.233  0.100
#> # … with 410 more rows, and 1 more variable: sharpness <dbl>
#> 
#> $forecast_cases
#> # A tibble: 420 x 12
#>    timeseries model forecast_date date       horizon median  mean    sd bottom
#>    <chr>      <chr> <chr>         <date>       <int>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Region 1   AR 3  2020-03-04    2020-03-05       1   57    58    9.72     46
#>  2 Region 1   AR 3  2020-03-04    2020-03-06       2   67.5  66.6  8.63     56
#>  3 Region 1   AR 3  2020-03-04    2020-03-07       3   78.5  77.8  6.83     66
#>  4 Region 1   AR 3  2020-03-04    2020-03-08       4   83    86.6 12.3      70
#>  5 Region 1   AR 3  2020-03-04    2020-03-09       5   91    97.5 17.0      77
#>  6 Region 1   AR 3  2020-03-04    2020-03-10       6  106   114.  13.0     102
#>  7 Region 1   AR 3  2020-03-04    2020-03-11       7  126.  122.  16.6      90
#>  8 Region 1   AR 3  2020-03-05    2020-03-06       1   57.5  59.4  9.89     43
#>  9 Region 1   AR 3  2020-03-05    2020-03-07       2   76.5  75.3 16.2      50
#> 10 Region 1   AR 3  2020-03-05    2020-03-08       3   77.5  66.8 30.7       0
#> # … with 410 more rows, and 3 more variables: lower <dbl>, upper <dbl>,
#> #   top <dbl>
#> 
#> $case_scores
#> # A tibble: 420 x 11
#>    timeseries model sample forecast_date date       horizon   dss  crps  logs
#>    <chr>      <chr> <chr>  <chr>         <date>       <int> <dbl> <dbl> <dbl>
#>  1 Region 1   AR 3  1      2020-03-04    2020-03-05       1  4.74  4.68  4.13
#>  2 Region 1   AR 3  1      2020-03-04    2020-03-06       2  4.82  4.44  3.52
#>  3 Region 1   AR 3  1      2020-03-04    2020-03-07       3  6.22  6.56  3.58
#>  4 Region 1   AR 3  1      2020-03-04    2020-03-08       4  6.66 11.4   4.71
#>  5 Region 1   AR 3  1      2020-03-04    2020-03-09       5  6.88 11.9   4.37
#>  6 Region 1   AR 3  1      2020-03-04    2020-03-10       6 10.2  21.3   5.01
#>  7 Region 1   AR 3  1      2020-03-04    2020-03-11       7 13.9  37.2  15.5 
#>  8 Region 1   AR 3  1      2020-03-05    2020-03-06       1  6.58  8.34  3.81
#>  9 Region 1   AR 3  1      2020-03-05    2020-03-07       2  6.15  9.05  4.44
#> 10 Region 1   AR 3  1      2020-03-05    2020-03-08       3  8.20 20.4   4.86
#> # … with 410 more rows, and 2 more variables: bias <dbl>, sharpness <dbl>
#> 
#> $raw_rt_forecast
#> # A tibble: 5,320 x 8
#>    timeseries model obs_sample forecast_date sample date          rt horizon
#>    <chr>      <chr> <chr>      <chr>          <int> <date>     <dbl>   <int>
#>  1 Region 1   AR 3  1          2020-03-04         1 2020-03-05  2.26       1
#>  2 Region 1   AR 3  1          2020-03-04         2 2020-03-05  2.24       1
#>  3 Region 1   AR 3  1          2020-03-04         3 2020-03-05  2.25       1
#>  4 Region 1   AR 3  1          2020-03-04         4 2020-03-05  2.25       1
#>  5 Region 1   AR 3  1          2020-03-04         5 2020-03-05  2.25       1
#>  6 Region 1   AR 3  1          2020-03-04         6 2020-03-05  2.26       1
#>  7 Region 1   AR 3  1          2020-03-04         7 2020-03-05  2.18       1
#>  8 Region 1   AR 3  1          2020-03-04         8 2020-03-05  2.17       1
#>  9 Region 1   AR 3  1          2020-03-04         9 2020-03-05  2.31       1
#> 10 Region 1   AR 3  1          2020-03-04        10 2020-03-05  2.30       1
#> # … with 5,310 more rows
#> 
#> $raw_case_forecast
#> # A tibble: 4,200 x 8
#>    timeseries model obs_sample forecast_date sample date       cases horizon
#>    <chr>      <chr> <chr>      <chr>          <dbl> <date>     <int>   <int>
#>  1 Region 1   AR 3  1          2020-03-04         1 2020-03-05    58       1
#>  2 Region 1   AR 3  1          2020-03-04         1 2020-03-06    69       2
#>  3 Region 1   AR 3  1          2020-03-04         1 2020-03-07    79       3
#>  4 Region 1   AR 3  1          2020-03-04         1 2020-03-08    82       4
#>  5 Region 1   AR 3  1          2020-03-04         1 2020-03-09    79       5
#>  6 Region 1   AR 3  1          2020-03-04         1 2020-03-10   104       6
#>  7 Region 1   AR 3  1          2020-03-04         1 2020-03-11    95       7
#>  8 Region 1   AR 3  1          2020-03-04         2 2020-03-05    77       1
#>  9 Region 1   AR 3  1          2020-03-04         2 2020-03-06    68       2
#> 10 Region 1   AR 3  1          2020-03-04         2 2020-03-07    82       3
#> # … with 4,190 more rows
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
#>  1 bias      AR 3                    0     0     0.100  0.273  0.4    1    0.357
#>  2 bias      Semi-local linear tr…   0     0     0.100  0.303  0.5    1    0.366
#>  3 crps      AR 3                    3.38  9.66 20.4   33.4   39.8  153.  37.6  
#>  4 crps      Semi-local linear tr…   3.02  9.24 19.1   32.8   35.2  155.  39.3  
#>  5 dss       AR 3                    4.91  6.75  8.83  13.6   13.7   56.8 16.9  
#>  6 dss       Semi-local linear tr…   4.48  6.83  8.47  13.7   12.5   62.5 18.1  
#>  7 logs      AR 3                    3.33  4.34  4.99   9.13   7.27  36.6 16.8  
#>  8 logs      Semi-local linear tr…   3.37  4.25  5.00  12.0    6.70  60.3 32.1  
#>  9 sharpness AR 3                    5.93 11.1  16.3   18.7   23.0   53.1 11.2  
#> 10 sharpness Semi-local linear tr…   4.45 10.4  17.0   19.6   24.5   56.5 12.3
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
