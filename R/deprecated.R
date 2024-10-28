#' Deprecated functions
#'
#'
#' @param ts,spec,context,userdefined,name Parameters.
#' @name deprecated-rjd3x13
#' @export
spec_x13 <- function(name = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c")) {
    .Deprecated("x13_spec")
    x13_spec(name)
}
#' @name deprecated-rjd3x13
#' @export
spec_regarima <- function(name = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c")) {
    .Deprecated("regarima_spec")
    regarima_spec(name)
}
#' @name deprecated-rjd3x13
#' @export
spec_x11 <- function() {
    .Deprecated("x11_spec")
    x11_spec()
}
#' @name deprecated-rjd3x13
#' @export
fast_x13 <- function(ts,
                     spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
                     context = NULL,
                     userdefined = NULL) {
    .Deprecated("x13_fast")
    x13_fast(ts, spec, context, userdefined)
}
#' @name deprecated-rjd3x13
#' @export
fast_regarima <- function(ts,
                          spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
                          context = NULL,
                          userdefined = NULL) {
    .Deprecated("regarima_fast")
    regarima_fast(ts, spec, context, userdefined)
}
