# Set an option for x13

Set an option for x13

## Usage

``` r
get_x13_option(name)
```

## Arguments

- name:

  Name of the option

## Value

The requested option or NULL if it doesn't exist

## Examples

``` r
x13_option("test", "DUMMY")
get_x13_option("test")
#> [1] "DUMMY"
```
