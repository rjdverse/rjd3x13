# Outlier Detection with a RegARIMA Model

Outlier Detection with a RegARIMA Model

## Usage

``` r
regarima_outliers(
  y,
  order = c(0L, 1L, 1L),
  seasonal = c(0L, 1L, 1L),
  mean = FALSE,
  X = NULL,
  X.td = NULL,
  ao = TRUE,
  ls = TRUE,
  tc = FALSE,
  so = FALSE,
  cv = 0,
  clean = FALSE
)
```

## Arguments

- y:

  the dependent variable (a `ts` object).

- order, seasonal:

  the orders of the ARIMA model.

- mean:

  Boolean to include or not the mean.

- X:

  user defined regressors (other than calendar).

- X.td:

  calendar regressors.

- ao, ls, so, tc:

  Boolean to indicate which type of outliers should be detected.

- cv:

  `numeric`. The entered critical value for the outlier detection
  procedure. If equal to 0 the critical value for the outlier detection
  procedure is automatically determined by the number of observations.

- clean:

  Clean missing values at the beginning/end of the series. Regression
  variables are automatically resized, if need be.

## Value

a `"JD3_REGARIMA_OUTLIERS"` object, containing input variables and
results

## Examples

``` r
# estimate model
model<- regarima_outliers(rjd3toolkit::ABS$X0.2.09.10.M)
# print outliers
model$model$variables
#> [1] "AO.220" "AO.219" "AO.277" "LS.400" "LS.280"
```
