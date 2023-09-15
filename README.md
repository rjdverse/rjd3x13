
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjd3x13

rjd3x13 offers full acces to options and outputs of X-13
(`rjd3x13::x13()`), including RegARIMA modelling (`rjd3x13::regarima()`)
and X-11 decomposition (`rjd3x13::x11()`).

The specification can be created with the functions
`rjd3x13::spec_regarima()`, `rjd3x13::spec_x11()` or
`rjd3x13::spec_x13()` and can be modified with the function:

- for the pre-processing: `rjd3toolkit::set_arima()`,
  `rjd3toolkit::set_automodel()`, `rjd3toolkit::set_basic()`,
  `rjd3toolkit::set_easter()`, `rjd3toolkit::set_estimate()`,
  `rjd3toolkit::set_outlier()`, `rjd3toolkit::set_tradingdays()`,
  `rjd3toolkit::set_transform()`, `rjd3toolkit::add_outlier()`,
  `rjd3toolkit::remove_outlier()`, `rjd3toolkit::add_ramp()`,
  `rjd3toolkit::remove_ramp()`, `rjd3toolkit::add_usrdefvar()`;

- for the decomposition: `rjd3x13::set_x11()`;

- for the benchmarking: `rjd3toolkit::set_benchmarking()`.

## Installation

``` r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit")
remotes::install_github("rjdemetra/rjd3x13")
```

## Usage

``` r
library(rjd3x13)
y <- rjd3toolkit::ABS$X0.2.09.10.M
x13_model <- x13(y) 
summary(x13_model$result$preprocessing) # Summary of regarima model
#> Log-transformation: yes 
#> SARIMA model:  (2,1,1) (0,1,1)
#> 
#> Coefficients
#>           Estimate Std. Error  T-stat Pr(>|t|)    
#> phi(1)     0.34740    0.06502   5.343 1.53e-07 ***
#> phi(2)     0.21733    0.06000   3.622 0.000329 ***
#> theta(1)  -0.69937    0.05115 -13.672  < 2e-16 ***
#> btheta(1) -0.48038    0.06993  -6.869 2.45e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Regression model:
#>                   Estimate Std. Error T-stat Pr(>|t|)    
#> td               0.0023233  0.0006844  3.395 0.000755 ***
#> easter           0.0520113  0.0084894  6.127 2.13e-09 ***
#> TC (2000-06-01)  0.1590340  0.0288578  5.511 6.37e-08 ***
#> AO (2000-07-01) -0.2900774  0.0400551 -7.242 2.25e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Number of observations:  425 , Number of effective observations:  412 , Number of parameters:  9 
#> Loglikelihood:  746.7517, Adjusted loglikelihood:  -2120.875
#> Standard error of the regression (ML estimate):  0.03927991 
#> AIC:  4259.75 , AICc:  4260.198 , BIC:  4295.939

plot(x13_model) # Plot of the final decomposition
```

<img src="man/figures/README-x-13-final-1.png" style="display: block; margin: auto;" />

To get the final components you can use the function
`rjd3toolkit::sa.decomposition()`:

``` r
rjd3toolkit::sa.decomposition(x13_model)
#> Last values
#>          series       sa    trend      seas       irr
#> Sep 2016 1393.5 1556.806 1548.194 0.8951022 1.0055624
#> Oct 2016 1497.4 1549.534 1546.816 0.9663552 1.0017571
#> Nov 2016 1684.3 1530.373 1546.901 1.1005815 0.9893151
#> Dec 2016 2850.4 1567.050 1548.130 1.8189590 1.0122211
#> Jan 2017 1428.5 1508.199 1549.876 0.9471559 0.9731099
#> Feb 2017 1092.4 1557.942 1551.664 0.7011816 1.0040459
#> Mar 2017 1370.3 1563.584 1553.159 0.8763842 1.0067116
#> Apr 2017 1522.6 1581.406 1553.982 0.9628142 1.0176473
#> May 2017 1452.4 1553.376 1553.920 0.9349956 0.9996498
#> Jun 2017 1557.2 1556.929 1553.056 1.0001743 1.0024936
#> Jul 2017 1445.5 1527.804 1551.625 0.9461293 0.9846474
#> Aug 2017 1303.1 1546.399 1550.102 0.8426674 0.9976109
```
