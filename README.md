
# EpiSoon

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

  - Load the package (`bsts` for models, `ggplot2` for plotting, and
    `cowplot` for theming)

<!-- end list -->

``` r
library(EpiSoon)
library(bsts)
library(cowplot)
```

  - Define example observations.

<!-- end list -->

``` r
observations <- data.frame(rt = 1:20,
                           date = as.Date("2020-01-01")
                            + lubridate::days(1:20))
```

  - Forecast a timeseries using a semi-local trend model and summarise
    it.

<!-- end list -->

``` r
samples <- fit_model(observations[1:10, ],
                      model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
                      horizon = 7, samples = 10)

 ## Summarise forecast
 summarised_forecast <- summarise_forecast(samples)
 
 summarised_forecast
```

    ## # A tibble: 7 x 9
    ##   date       horizon bottom lower median  mean upper   top    sd
    ##   <date>       <int>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 2020-01-12       1   10.3  10.7   10.8  10.8  11.0  11.1 0.280
    ## 2 2020-01-13       2   11.4  11.8   11.9  11.9  12.1  12.2 0.264
    ## 3 2020-01-14       3   12.6  12.8   12.9  12.9  13.1  13.3 0.244
    ## 4 2020-01-15       4   13.1  13.8   14.0  13.9  14.1  14.3 0.380
    ## 5 2020-01-16       5   13.9  14.6   14.7  14.8  15.1  15.4 0.477
    ## 6 2020-01-17       6   14.7  15.5   15.9  15.8  16.1  16.6 0.607
    ## 7 2020-01-18       7   16.1  16.7   16.9  16.8  17.1  17.4 0.415

  - Score the forecast

<!-- end list -->

``` r
scores <- score_model(samples, observations)

summarise_scores(scores)
```

    ## # A tibble: 5 x 8
    ##   score      bottom   lower median    mean  upper    top     sd
    ##   <chr>       <dbl>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 bias       0.215   0.3     0.3    0.357   0.45   0.5   0.113 
    ## 2 crps       0.0683  0.0770  0.108  0.108   0.126  0.166 0.0378
    ## 3 dss       -2.73   -2.35   -1.98  -1.92   -1.52  -1.00  0.658 
    ## 4 logs      -0.320  -0.240  -0.150 -0.0225  0.199  0.403 0.304 
    ## 5 sharpness  0.163   0.262   0.287  0.314   0.382  0.463 0.108

  - Plot the forecast

<!-- end list -->

``` r
 ## Plot forecast
 plot_forecast(summarised_forecast, observations)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

  - Iteratively fit the forecast and plot this to visualise the forecast
    quality

<!-- end list -->

``` r
forecast_eval <- evaluate_model(observations,
                                 model = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)},
                                horizon = 7, samples = 10)

forecasts <- forecast_eval$forecasts

 ## Plot forecast
 plot_forecast_evaluation(forecasts, observations, horizon_to_plot = c(1, 3, 7)) +
   ggplot2::facet_wrap(~ horizon, ncol = 1) +
   cowplot::panel_border()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Evaluate across models

  - Define a list of models.

<!-- end list -->

``` r
## List of forecasting bsts models wrapped in functions.
models <- list("Sparse AR" = function(ss, y){bsts::AddAutoAr(ss, y = y, lags = 7)},
                "Semi-local linear trend" = function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)})
```

  - Compare across
models.

<!-- end list -->

``` r
evaluations <- compare_models(observations, models, horizon = 7, samples = 10)
```

  - Plot evaluation of models over a set of time
horizons.

<!-- end list -->

``` r
plot_forecast_evaluation(evaluations$forecast, observations, c(1, 3, 7)) +
   ggplot2::facet_grid(model ~ horizon) +
   cowplot::panel_border()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

  - Score across models

<!-- end list -->

``` r
summarise_scores(evaluations$scores)
```

    ## # A tibble: 10 x 9
    ##    score    model            bottom   lower median    mean  upper    top      sd
    ##    <chr>    <chr>             <dbl>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>   <dbl>
    ##  1 bias     Semi-local lin…  0.200   0.4     0.5    0.493  0.6     0.758  0.150 
    ##  2 bias     Sparse AR        0       0       0      0.0745 0.100   0.3    0.0945
    ##  3 crps     Semi-local lin…  0.0568  0.102   0.154  0.204  0.202   0.690  0.173 
    ##  4 crps     Sparse AR        0.584   1.66    3.27   3.66   5.39    9.13   2.40  
    ##  5 dss      Semi-local lin… -3.03   -2.05   -1.11  -0.898  0.0469  2.31   1.46  
    ##  6 dss      Sparse AR        0.909   2.97    4.96   7.62   7.62   39.5   10.1   
    ##  7 logs     Semi-local lin… -0.449   0.0193  0.315  0.481  0.745   1.91   0.671 
    ##  8 logs     Sparse AR        1.52    2.45    3.37   5.85   4.34   26.9   14.6   
    ##  9 sharpne… Semi-local lin…  0.152   0.300   0.456  0.662  0.733   2.44   0.574 
    ## 10 sharpne… Sparse AR        0       1.29    1.89   2.09   2.79    4.46   1.13

### Evaluate across regions and models

  - Define a list of timeseries

<!-- end list -->

``` r
timeseries <- list(observations, observations)
names(timeseries) <- c("Region 1", "Region 2")
```

  - Compare across regions and models

<!-- end list -->

``` r
evaluations <- compare_timeseries(timeseries, models,
                                   horizon = 7, samples = 10)
```

  - Plot comparison

<!-- end list -->

``` r
plot_forecast_evaluation(evaluations$forecast, observations, c(7)) +
   ggplot2::facet_grid(model ~ region) +
   cowplot::panel_border()
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

  - Summarise CRPS by region

<!-- end list -->

``` r
summarise_scores(evaluations$scores, "region", sel_scores = "crps")
```

    ## # A tibble: 4 x 10
    ##   region   score model              bottom  lower median  mean upper   top    sd
    ##   <chr>    <chr> <chr>               <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Region 1 crps  Semi-local linear… 0.0519 0.0941  0.150 0.265 0.252 1.25  0.317
    ## 2 Region 1 crps  Sparse AR          0.410  1.31    3.30  3.47  5.05  7.96  2.37 
    ## 3 Region 2 crps  Semi-local linear… 0.0468 0.0925  0.133 0.197 0.231 0.711 0.177
    ## 4 Region 2 crps  Sparse AR          0.461  1.29    2.96  3.47  4.87  9.15  2.58

  - Summarise logs by horizon

<!-- end list -->

``` r
summarise_scores(evaluations$scores, "horizon", sel_scores = "logs")
```

    ## # A tibble: 14 x 10
    ##    horizon score model     bottom   lower  median     mean  upper    top      sd
    ##      <int> <chr> <chr>      <dbl>   <dbl>   <dbl>    <dbl>  <dbl>  <dbl>   <dbl>
    ##  1       1 logs  Semi-loc… -0.862 -0.349  -0.123   -0.0766 0.0695   1.17   0.519
    ##  2       1 logs  Sparse AR  1.11   1.32    1.62     1.75   1.94     2.80   0.528
    ##  3       2 logs  Semi-loc… -0.517 -0.153   0.0935   0.212  0.431    1.58   0.547
    ##  4       2 logs  Sparse AR  1.49   2.05    2.34     3.91   2.74    14.5    7.83 
    ##  5       3 logs  Semi-loc… -0.738 -0.110   0.176    0.253  0.475    1.92   0.663
    ##  6       3 logs  Sparse AR  1.97   2.33    3.06     4.22   3.63    18.2    4.23 
    ##  7       4 logs  Semi-loc… -0.388  0.0524  0.499    0.502  0.738    2.24   0.694
    ##  8       4 logs  Sparse AR  2.35   2.82    3.28     6.14   4.13    25.3    8.19 
    ##  9       5 logs  Semi-loc… -0.352  0.314   0.585    0.655  0.860    2.18   0.664
    ## 10       5 logs  Sparse AR  2.83   3.09    3.42    17.9    6.21   139.    49.9  
    ## 11       6 logs  Semi-loc… -0.310  0.185   0.632    0.751  0.992    2.37   0.744
    ## 12       6 logs  Sparse AR  3.00   3.42    3.71    11.3    5.30    63.2   18.9  
    ## 13       7 logs  Semi-loc…  0.113  0.519   0.852    0.946  1.20     2.38   0.674
    ## 14       7 logs  Sparse AR  3.20   3.68    4.01   Inf      7.11   Inf    Inf

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
