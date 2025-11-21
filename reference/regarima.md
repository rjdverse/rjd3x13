# RegARIMA model, pre-adjustment in X13

RegARIMA model, pre-adjustment in X13

## Usage

``` r
regarima(
  ts,
  spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
  context = NULL,
  userdefined = NULL
)

regarima_fast(
  ts,
  spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
  context = NULL,
  userdefined = NULL
)
```

## Arguments

- ts:

  an univariate time series.

- spec:

  the model specification. Can be either the name of a predefined
  specification or a user-defined specification.

- context:

  list of external regressors (calendar or other) to be used for
  estimation

- userdefined:

  a vector containing additional output variables (see
  [`x13_dictionary()`](https://rjdverse.github.io/rjd3x13/reference/x13_dictionary.md)).

## Value

the `regarima()` function returns a list with the results
(`"JD3_REGARIMA_RSLTS"` object), the estimation specification and the
result specification, while `regarima_fast()` is a faster function that
only returns the results.

## Examples

``` r
y <- rjd3toolkit::ABS$X0.2.09.10.M
sp <- regarima_spec("rg5c")
sp <- rjd3toolkit::add_outlier(sp,
    type = c("AO"), c("2015-01-01", "2010-01-01")
)
regarima_fast(y, spec = sp)
#> Log-transformation: yes 
#> SARIMA model: (0,1,1) (1,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1)   bphi(1) btheta(1) 
#>   -0.8161   -0.4373   -0.8251 
#> 
#> Regression model:
#>             mon             tue             wed             thu             fri 
#>       -0.008747        0.004488       -0.001471        0.013886       -0.001944 
#>             sat          easter AO (2010-01-01) AO (2015-01-01) TC (2000-06-01) 
#>        0.015368        0.051130        0.035349       -0.020385        0.162169 
#> AO (2000-07-01) 
#>       -0.306536 
#> 
#> For a more detailed output, use the 'summary()' function.
sp <- rjd3toolkit::set_transform(
    rjd3toolkit::set_tradingdays(
        rjd3toolkit::set_easter(sp, enabled = FALSE),
        option = "workingdays"
    ),
    fun = "None"
)
regarima_fast(y, spec = sp)
#> Log-transformation: no 
#> SARIMA model: (3,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>    phi(1)    phi(2)    phi(3)  theta(1) btheta(1) 
#>    0.1656    0.0878   -0.1129   -0.8608   -0.2292 
#> 
#> Regression model:
#>              td              lp AO (2010-01-01) AO (2015-01-01) AO (2000-06-01) 
#>           1.002          29.639          37.048          27.995         199.754 
#> AO (2000-07-01) LS (2005-04-01) LS (2015-07-01) 
#>        -194.689         -82.306          81.333 
#> 
#> For a more detailed output, use the 'summary()' function.
sp <- rjd3toolkit::set_outlier(sp, outliers.type = c("AO"))
regarima_fast(y, spec = sp)
#> Log-transformation: no 
#> SARIMA model: (3,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>    phi(1)    phi(2)    phi(3)  theta(1) btheta(1) 
#>   0.11808   0.03364  -0.15061  -0.83611  -0.24114 
#> 
#> Regression model:
#>              td              lp AO (2010-01-01) AO (2015-01-01) AO (2000-06-01) 
#>           1.001          30.898          37.280           6.562         194.616 
#> AO (2000-07-01) AO (2005-04-01) 
#>        -201.192        -150.277 
#> 
#> For a more detailed output, use the 'summary()' function.
```
