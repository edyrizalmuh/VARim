
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VARim

<!-- badges: start -->
<!-- badges: end -->

VARim is a multivariate time series imputation method based on Vector
Autoregression (VAR) model. This imputation method was proposed by
[Bashir and Wei
(2018)](https://www.sciencedirect.com/science/article/abs/pii/S0925231217315515).
The package also includes improved methods that can be used on data with
seasonality and non-stationary data. An evaluation of this package,
specifically for weather data, can be accessed
[here](https://ojs3.unpatti.ac.id/index.php/barekeng/article/view/6494).

## Installation

You can install the development version of VARim like so:

``` r
# install.packages("devtools")
library(devtools)
install_github("edyrizalmuh/VARim")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(VARim)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
miss_data = omit(daily_weather, seed = 1, num_miss = 200)
colSums(is.na(miss_data)) # NA before imputation
#>     min_temp     max_temp     avg_temp avg_humidity avg_rainfall     sunshine 
#>          200            0            0            0            0            0
imp_data = VARim(miss_data, init_method = "na_ma")
colSums(is.na(imp_data$imputed_data)) # NA after imputation
#>     min_temp     max_temp     avg_temp avg_humidity avg_rainfall     sunshine 
#>            0            0            0            0            0            0
```
