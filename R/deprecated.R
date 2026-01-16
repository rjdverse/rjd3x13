#' @title Deprecated functions
#'
#' @inheritParams x13
#' @inheritParams x13_spec
#'
#' @name deprecated-rjd3x13

#' @rdname deprecated-rjd3x13
#' @export
spec_x13 <- function(name = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c")) {
    .Deprecated("x13_spec")
    x13_spec(name)
}
#' @rdname deprecated-rjd3x13
#' @export
spec_regarima <- function(name = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c")) {
    .Deprecated("regarima_spec")
    regarima_spec(name)
}
#' @rdname deprecated-rjd3x13
#' @export
spec_x11 <- function() {
    .Deprecated("x11_spec")
    x11_spec()
}
#' @rdname deprecated-rjd3x13
#' @export
fast_x13 <- function(ts,
                     spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
                     context = NULL,
                     userdefined = NULL) {
    .Deprecated("x13_fast")
    x13_fast(ts, spec, context, userdefined)
}
#' @rdname deprecated-rjd3x13
#' @export
fast_regarima <- function(ts,
                          spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
                          context = NULL,
                          userdefined = NULL) {
    .Deprecated("regarima_fast")
    regarima_fast(ts, spec, context, userdefined)
}

#' @rdname deprecated-rjd3x13
#' @export
.jx13 <- function(ts,
                  spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
                  context = NULL,
                  userdefined = NULL) {
    .Deprecated("jx13")
    jx13(ts, spec, context, userdefined)
}

#' @rdname deprecated-rjd3x13
#' @export
userdefined_variables_x13 <- function(x = c("X-13", "RegArima", "X-11"))  {
    .Deprecated("x13_dictionary")
    x13_dictionary
}
