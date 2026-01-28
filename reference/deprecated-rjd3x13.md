# Deprecated functions

Deprecated functions

## Usage

``` r
spec_x13(name = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"))

spec_regarima(name = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"))

spec_x11()

fast_x13(
  ts,
  spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
  context = NULL,
  userdefined = NULL
)

fast_regarima(
  ts,
  spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
  context = NULL,
  userdefined = NULL
)

.jx13(
  ts,
  spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
  context = NULL,
  userdefined = NULL
)

userdefined_variables_x13(x = c("X-13", "RegArima", "X-11"))
```

## Arguments

- name:

  the name of a predefined specification.

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

- x:

  useless parameter

## Value

All these functions are deprecated and return the same value as the
function that replaces them:

- `spec_x13()` returns the same value as
  [`x13_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.md)

- `spec_regarima()` returns the same value as
  [`regarima_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.md)

- `spec_x11()` returns the same value as
  [`x11_spec()`](https://rjdverse.github.io/rjd3x13/reference/x13_spec.md)

- `fast_x13()` returns the same value as
  [`x13_fast()`](https://rjdverse.github.io/rjd3x13/reference/x13.md)

- `fast_regarima()` returns the same value as
  [`regarima_fast()`](https://rjdverse.github.io/rjd3x13/reference/regarima.md)

- `.jx13()` returns the same value as
  [`jx13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md)

- `userdefined_variables_x13()` returns the same value as
  [`x13_dictionary()`](https://rjdverse.github.io/rjd3x13/reference/x13_dictionary.md)
