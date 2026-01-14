# X-13 Full Dictionary

Function listing the format and description for all output objects
(series, diagnostics, parameters) available with
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) function.
Can be used to generate an output non available by default with
userdefined option in
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md)function
(see examples).

## Usage

``` r
x13_full_dictionary()
```

## Value

returns a data frame containing format and description, for all output
objects (series, diagnostics, parameters) available with
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md)function

## See also

`x13_dictionary` for an abbreviated version of the output description

## Examples

``` r
# visualize the dictionary
# first 10 lines
x13_full_dictionary()[1:10,]
#>           name                                               description
#> 1       period                                      period of the series
#> 2   span.start                  start of the considered (partial) series
#> 3     span.end                    end of the considered (partial) series
#> 4       span.n      number of periods in the considered (partial) series
#> 5 span.missing number of missing values in the considered (partial) s...
#> 6          log                                         log-transformtion
#> ...
#> 
#>  For a complete list of all outputs, please call summary()
#> 
#>  For more informations about the type, the java class of the output or additive details, call `View()`.
# for more structured information call `View(x13_full_dictionary())`
# extract names of output of interest
user_defined_output <- x13_full_dictionary()[135,1]
user_defined_output
#> [1] "sa_f"
# generate the corresponding output in an estimation
library(rjd3toolkit)
y <- rjd3toolkit::ABS$X0.2.09.10.M
m<-x13(y,"rsa3", userdefined=user_defined_output)
# retrieve user defined output
m$user_defined$sa_f
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 2017                                                                        
#> 2018 1545.102 1550.995 1544.872 1558.419 1556.812 1546.513 1552.880 1555.686
#>           Sep      Oct      Nov      Dec
#> 2017 1559.713 1541.278 1551.031 1550.678
#> 2018                                    
```
