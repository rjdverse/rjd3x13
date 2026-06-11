#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass .jfield
#' @import RProtoBuf
#' @import rjd3toolkit
NULL

#' @title Java Utility Functions
#'
#' @description
#' These functions are used in all JDemetra+ 3.0 packages to easily interact between R and Java objects.
#'
#' @param spec,jspec,jrslts parameters.
#'
#' @name jd3_utilities
#'
#' @returns These functions return specification in Java, proto or R.
#'
NULL
#> NULL


identical_na <- function(x) {
    identical(x, NA) ||
        identical(x, NA_character_) ||
        identical(x, NA_complex_) ||
        identical(x, NA_integer_) ||
        identical(x, NA_real_) ||
        identical(x, NaN)
}
