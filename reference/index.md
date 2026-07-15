# Package index

## Defining specifications

Prior settings for seasonal adjustment with X-13ARIMA or Reg-Arima
modelling

- [`regarima_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.md)
  [`x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.md)
  [`x11_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.md)
  : RegARIMA/X-13 Default Specifications
- [`set_x11()`](https://rjdverse.github.io/rjd3x13/reference/x11_spec.md)
  : Set X-11 Specification

## Seasonal adjustment

Functions to perform seasonal adjustment with X-13ARIMA

- [`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md)
  [`x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.md)
  [`jx13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) :
  Seasonal Adjustment with X13-ARIMA
- [`x11()`](https://rjdverse.github.io/rjd3x13/reference/x11.md) : X-11
  Decomposition Algorithm

## RegARIMA

Functions to perform regARIMA estimation only

- [`regarima()`](https://rjdverse.github.io/rjd3x13/reference/regarima.md)
  [`regarima_fast()`](https://rjdverse.github.io/rjd3x13/reference/regarima.md)
  : RegARIMA model, pre-adjustment in X13

## Outlier Detection

Outlier Detection with a RegARIMA Model

- [`regarima_outliers()`](https://rjdverse.github.io/rjd3x13/reference/regarima_outliers.md)
  : Outlier Detection with a RegARIMA Model

## Refresh policies

Functions to update a specification within a domain of constraints

- [`regarima_refresh()`](https://rjdverse.github.io/rjd3x13/reference/refresh.md)
  [`x13_refresh()`](https://rjdverse.github.io/rjd3x13/reference/refresh.md)
  : Refresh a specification with constraints

## Revision History

Monitoring revisions in estimation when adding data

- [`x13_revisions()`](https://rjdverse.github.io/rjd3x13/reference/x13_revisions.md)
  : Revisions History

## Output description

Displaying names and description of all available output objects,
defining customized output

- [`x13_dictionary()`](https://rjdverse.github.io/rjd3x13/reference/x13_dictionary.md)
  [`x13_full_dictionary()`](https://rjdverse.github.io/rjd3x13/reference/x13_dictionary.md)
  : X-13 Dictionary
- [`spec_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`spec_regarima()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`spec_x11()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`fast_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`fast_regarima()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`.jx13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`userdefined_variables_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  : Deprecated functions

## Option for rjdverse

Evironment specific to JDemetra+

- [`get_x13_option()`](https://rjdverse.github.io/rjd3x13/reference/get_x13_option.md)
  : Set an option for x13
- [`x13_option()`](https://rjdverse.github.io/rjd3x13/reference/x13_option.md)
  : Set an option for x13

## Wrangling Java objects

Functions to easily interact between R and Java objects

- [`.x13_rslts()`](https://rjdverse.github.io/rjd3x13/reference/jd3_utilities.md)
  [`.jd2r_spec_x11()`](https://rjdverse.github.io/rjd3x13/reference/jd3_utilities.md)
  [`.r2jd_spec_x11()`](https://rjdverse.github.io/rjd3x13/reference/jd3_utilities.md)
  [`.r2jd_spec_regarima()`](https://rjdverse.github.io/rjd3x13/reference/jd3_utilities.md)
  [`.jd2r_spec_regarima()`](https://rjdverse.github.io/rjd3x13/reference/jd3_utilities.md)
  [`.r2jd_spec_x13()`](https://rjdverse.github.io/rjd3x13/reference/jd3_utilities.md)
  [`.jd2r_spec_x13()`](https://rjdverse.github.io/rjd3x13/reference/jd3_utilities.md)
  : Java Utility Functions
- [`spec_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`spec_regarima()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`spec_x11()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`fast_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`fast_regarima()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`.jx13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`userdefined_variables_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  : Deprecated functions

## Deprecated functions

Use new version

- [`spec_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`spec_regarima()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`spec_x11()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`fast_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`fast_regarima()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`.jx13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  [`userdefined_variables_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.md)
  : Deprecated functions
