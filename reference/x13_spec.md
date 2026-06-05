# RegARIMA/X-13 Default Specifications

Set of functions to create default specification objects associated with
X-13ARIMA seasonal adjustment method.

Specification setting of sheer X-11 decomposition method (without
reg-arima pre-adjustment) is supported by `x11_spec()` function only and
doesn't appear among possible X13-Arima default specifications.

Specification setting can be restricted to the reg-arima part with
`regarima_spec()` function, without argument `regarima_spec()` yields a
RG5c specification.

Setting a complete X13-Arima spec, `x13_spec()` without argument yields
a RSA5c specification.

## Usage

``` r
regarima_spec(name = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"))

x13_spec(name = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"))

x11_spec()
```

## Arguments

- name:

  name of a predefined specification.

## Value

an object of class `"JD3_X13_SPEC"` (`x13_spec()`),
`"JD3_REGARIMA_SPEC"` (`regarima_spec()`) or `"JD3_X11_SPEC"`
(`x11_spec()`).

## Details

Available predefined 'JDemetra+' model specifications are described in
the table below:

|  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|
| **Identifier** \| | **Log/level detection** \| | **Outliers detection** \| | **Calendar effects** \| | **ARIMA** | RSA0/RG0 \| | *NA* \| |
| *NA* \| | *NA* \| | Airline(+mean) | RSA1/RG1 \| | automatic \| | AO/LS/TC \| | *NA* \| |
| Airline(+mean) | RSA2c/RG2c \| | automatic \| | AO/LS/TC \| | 2 td vars + Easter \| | Airline(+mean) | RSA3/RG3 \| |
| automatic \| | AO/LS/TC \| | *NA* \| | automatic | RSA4c/RG4c \| | automatic \| | AO/LS/TC \| |
| 2 td vars + Easter \| | automatic | RSA5c/RG5c \| | automatic \| | AO/LS/TC \| | 7 td vars + Easter \| | automatic |

## See also

- To set the pre-processing parameters:
  [`rjd3toolkit::set_arima()`](https://rjdverse.github.io/rjd3toolkit/reference/set_arima.html),
  [`rjd3toolkit::set_automodel()`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.html),
  [`rjd3toolkit::set_basic()`](https://rjdverse.github.io/rjd3toolkit/reference/set_basic.html),
  [`rjd3toolkit::set_easter()`](https://rjdverse.github.io/rjd3toolkit/reference/set_easter.html),
  [`rjd3toolkit::set_estimate()`](https://rjdverse.github.io/rjd3toolkit/reference/set_estimate.html),
  [`rjd3toolkit::set_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/set_outlier.html),
  [`rjd3toolkit::set_tradingdays()`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.html),
  [`rjd3toolkit::set_transform()`](https://rjdverse.github.io/rjd3toolkit/reference/set_transform.html),
  [`rjd3toolkit::add_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
  [`rjd3toolkit::remove_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
  [`rjd3toolkit::add_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
  [`rjd3toolkit::remove_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
  [`rjd3toolkit::add_usrdefvar()`](https://rjdverse.github.io/rjd3toolkit/reference/add_usrdefvar.html).

- To set the decomposition parameters:
  [`set_x11()`](https://rjdverse.github.io/rjd3x13/reference/x11_spec.md).

- To set the benchmarking parameters:
  [`rjd3toolkit::set_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/set_benchmarking.html).

## Examples

``` r
init_spec <- x11_spec()
init_spec
#> Specification X11
#> Seasonal component: Yes
#> Length of the Henderson filter: 0
#> Seasonal filter: FILTER_MSR
#> Boundaries used for extreme values correction : 
#>   lower_sigma:  1.5 
#>   upper_sigma:  2.5
#> Nb of forecasts: 0
#> Nb of backcasts: 0
#> Calendar sigma: NONE
init_spec <- regarima_spec("rg4")
init_spec
#> Specification
#> 
#> Series
#> Serie span: All 
#> Preliminary Check: Yes
#> 
#> Estimate
#> Model span: All 
#> 
#> Tolerance: 1e-07
#> 
#> Transformation
#> Function: AUTO
#> AIC difference: -2
#> Adjust: NONE
#> 
#> Regression
#> Calendar regressor: WorkingDays
#> with Leap Year: Yes
#> AutoAdjust: TRUE
#> Test: REMOVE
#> 
#> Easter: STANDARD 
#> Duration: 8 (Auto) 
#> Test: ADD (Auto) 
#> 
#> Pre-specified outliers: 0
#> Ramps: No
#> 
#> Outliers
#> Detection span: All 
#> Outliers type: 
#>  - AO, critical value : 0 (Auto)
#>  - LS, critical value : 0 (Auto)
#>  - TC, critical value : 0 (Auto)
#> TC rate: 0.7 (Auto)
#> Method: ADDONE (Auto)
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>         0         0 
init_spec <- x13_spec("rsa5c")
init_spec
#> Specification
#> 
#> Series
#> Serie span: All 
#> Preliminary Check: Yes
#> 
#> Estimate
#> Model span: All 
#> 
#> Tolerance: 1e-07
#> 
#> Transformation
#> Function: AUTO
#> AIC difference: -2
#> Adjust: NONE
#> 
#> Regression
#> Calendar regressor: TradingDays
#> with Leap Year: Yes
#> AutoAdjust: TRUE
#> Test: REMOVE
#> 
#> Easter: STANDARD 
#> Duration: 8 (Auto) 
#> Test: ADD (Auto) 
#> 
#> Pre-specified outliers: 0
#> Ramps: No
#> 
#> Outliers
#> Detection span: All 
#> Outliers type: 
#>  - AO, critical value : 0 (Auto)
#>  - LS, critical value : 0 (Auto)
#>  - TC, critical value : 0 (Auto)
#> TC rate: 0.7 (Auto)
#> Method: ADDONE (Auto)
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>         0         0 
#> 
#> Specification X11
#> Seasonal component: Yes
#> Length of the Henderson filter: 0
#> Seasonal filter: FILTER_MSR
#> Boundaries used for extreme values correction : 
#>   lower_sigma:  1.5 
#>   upper_sigma:  2.5
#> Nb of forecasts: -1
#> Nb of backcasts: 0
#> Calendar sigma: NONE
#> 
#> Benchmarking
#> Is enabled: No
```
