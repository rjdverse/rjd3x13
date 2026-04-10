# Refresh a specification with constraints

Functions `x13_refresh` and `regarima_refresh` allow to create a new
specification by updating an existing one. Some selected parameters will
be kept fixed while others will be freed within the boundaries of a
reference specification. In practice each freed parameter of the
specification to be updated (`spec`) is replaced by the corresponding
parameter of the reference specification (`refspec`). See details and
examples.

## Usage

``` r
regarima_refresh(
  spec,
  refspec = NULL,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"),
  period = 0,
  start = NULL,
  end = NULL
)

x13_refresh(
  spec,
  refspec = NULL,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"),
  period = 0,
  start = NULL,
  end = NULL
)
```

## Arguments

- spec:

  specification to be refreshed Object of class "JD3_X13_SPEC" or
  "JD3_REGARIMA_SPEC", can be obtained as an output of `x13_spec` or
  `regarima_spec` and customised with `set_` functions, see `x13_spec`
  documentation

- refspec:

  reference specification By default `"RG4c"` or `"rsa4"` specification.
  Object of class "JD3_X13_SPEC" or "JD3_REGARIMA_SPEC", can be obtained
  as an output of `x13_spec` or `regarima_spec` and customised with
  `set_` functions, see `x13_spec` documentation

- policy:

  refresh policy to apply (see details)

- period, start, end:

  additional parameters used to specify the span on which additive
  outliers (AO) are introduced when `policy = "Current"` or to specify
  the span on which outliers will be re-detected when
  `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`, in
  this last case `end` is unused.

  If `start` is not specified, outliers will be re-identified on the
  whole series. Span definition: `period`: numeric, number of
  observations in a year (12, 4...). `start` and `end`: defined as
  arrays of two elements: year and first period (for example,
  `period = 12` and `c(1980, 1)` stands for January 1980) The dates
  corresponding to `start` and `end` are included in the span
  definition.

## Value

a new specification, an object of class `"JD3_X13_SPEC"` or
`"JD3_REGARIMA_SPEC"`.

## Details

A particular selection of parameters to be kept fixed or re-estimated is
called a revision policy.

Available refresh policies are:

1.  **Current**: applying the current pre-adjustment reg-arima model and
    handling the new raw data points, or any sub-span of the series as
    Additive Outliers (defined as new intervention variables); X11 and
    Benchmarking part parameters are untouched.

2.  **Fixed**: applying the current pre-adjustment reg-arima model and
    replacing forecasts by new raw data points; X11 and Benchmarking
    part parameters are untouched.

3.  **FixedParameters**: pre-adjustment reg-arima model is partially
    modified: regression coefficients will be re-estimated but
    regression variables, Arima orders and coefficients are unchanged;

4.  **FixedAutoRegressiveParameters**: same as FixedParameters but Arima
    Moving Average coefficients (MA) are also re-estimated,
    Auto-regressive (AR) coefficients are kept fixed; X11 and
    Benchmarking part parameters are untouched.

5.  **FreeParameters**: all regression and Arima model coefficients are
    re-estimated, regression variables and Arima orders are kept fixed;
    X11 and Benchmarking part parameters are untouched.

6.  **Outliers**: regression variables and Arima orders are kept fixed,
    but outliers will be re-detected on the defined span, thus all
    regression and Arima model coefficients are re-estimated; X11 and
    Benchmarking part parameters are untouched.

7.  **Outliers_StochasticComponent**: same as "Outliers" but Arima model
    orders (p,d,q)(P,D,Q) can also be re-identified; X11 and
    Benchmarking part parameters are untouched.

8.  **Complete**: All the parameters are re-identified and re-estimated,
    unless constrained in the domain spec. X11 and Benchmarking part
    parameters are entirely reset to values in the reference spec.

## References

More information on revision policies in JDemetra+ documentation:
<https://jdemetra-new-documentation.netlify.app/a-rev-policies>

## Examples

``` r
library("rjd3toolkit")
#> 
#> Attaching package: ‘rjd3toolkit’
#> The following objects are masked from ‘package:stats’:
#> 
#>     aggregate, mad
# \donttest{
# Example 1 : refresh mechanism
# Create reference spec, here the default "rsa3"
rsa3<- x13_spec("rsa3")
# Customize this spec
## Reg-Arima part
### For example, disable automatic arima modelling
user_spec <- set_automodel(rsa3, enabled = FALSE)
### set a user-defined arima model
user_spec <- set_arima(
   user_spec,
   mean = 0.2,
   mean.type = "Fixed",
   p = 1,
   d = 2,
   q = 0,
   bp = 1,
   bd = 1,
   bq = 0,
   coef = c(0.6, 0.7),
   coef.type = c("Initial", "Fixed")
)
#print(user_spec)

## Customize the x11 part
user_spec<-set_x11(user_spec,
                  lsigma = 2,
                  usigma = 3,
                  fcasts = -2,
                  bcasts = -1)
#print(user_spec)
user_spec<- set_benchmarking(
   user_spec,
   enabled = TRUE,
   target = "Original",
   rho = 0.7,
   lambda = 0.5,
   forecast = TRUE,
   bias = "Multiplicative")
#print(user_spec)
# Use policy: "Outliers_StochasticComponent"
x13_spec_ref <- x13_refresh(spec= user_spec,
                           refspec= rsa3,
                           policy = "Outliers_StochasticComponent"
)
# print(x13_spec_ref)
# user defined reg-arima model is reset and outliers will be re-identified
# on the whole series as no start and end specified, X11 and Benchmarking parameters
# are left unchanged
# Use policy: "Complete"
x13_spec_ref <- x13_refresh(spec= user_spec,
                           refspec= rsa3,
                           policy = "Complete"
)
# print(x13_spec_ref)
# all user defined parameters are reset and replaced with "rsa3" parameters,
# including for X11 and Benchmarking parameters

# Example 2 : practical re-estimation use-case
y <- rjd3toolkit::ABS$X0.2.08.10.M
# raw series for first estimation
y_raw <- window(y, end = c(2016, 12))
# raw series for second (refreshed) estimation: new data points
y_new <- window(y, end = c(2017, 6))
sa_x13 <- x13(y_raw, user_spec)
# refreshing the specification resulting from the first estimation
# to partially adapt it to new data
spec_to_refresh <- sa_x13$result_spec
reference_spec <- sa_x13$estimation_spec
# policy = "Fixed"
spec_x13_ref <- x13_refresh(spec_to_refresh,
                            reference_spec,
                            policy = "Fixed"
)
# 2nd estimation with refreshed specification
sa_x13_ref <- x13(y_new, spec_x13_ref)
# policy = "Outliers"
spec_x13_ref <- x13_refresh(spec_to_refresh,
                            reference_spec,
                            policy = "Outliers",
                            period = 12,
                            start = c(2017, 1)
)
# outliers will be re-detected from January 2017 included
# 2nd estimation with refreshed specification
sa_x13_ref <- x13(y_new, spec_x13_ref)

# policy = "Current"

spec_x13_ref <- x13_refresh(spec_to_refresh,
                            reference_spec,
                            policy = "Current",
                            period = 12,
                            start = c(2017, 1),
                            end = end(y_new)
)
# Points from January 2017 (included) until the end of the series will be
# treated as Additive Outliers, the previous reg-Arima model being otherwise
# kept fixed 2nd estimation with refreshed specification
sa_x13_ref <- x13(y_new, spec_x13_ref)

# }
```
