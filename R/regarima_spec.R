#' @importFrom rjd3toolkit add_outlier
#' @export
add_outlier.JD3_X13_SPEC <- function(x,
                                     ...) {
    x$regarima <- add_outlier(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit remove_outlier
#' @export
remove_outlier.JD3_X13_SPEC <- function(x,
                                        ...) {
    x$regarima <- remove_outlier(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit add_ramp
#' @export
add_ramp.JD3_X13_SPEC <- function(x,
                                  ...) {
    x$regarima <- add_ramp(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit remove_ramp
#' @export
remove_ramp.JD3_X13_SPEC <- function(x,
                                     ...) {
    x$regarima <- remove_ramp(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_arima
#' @export
set_arima.JD3_X13_SPEC <- function(x,
                                   ...) {
    x$regarima <- set_arima(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_automodel
#' @export
set_automodel.JD3_X13_SPEC <- function(x,
                                       ...) {
    x$regarima <- set_automodel(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_easter
#' @export
set_easter.JD3_X13_SPEC <- function(x,
                                    ...) {
    x$regarima <- set_easter(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_estimate
#' @export
set_estimate.JD3_X13_SPEC <- function(x,
                                      ...) {
    x$regarima <- set_estimate(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_basic
#' @export
set_basic.JD3_X13_SPEC <- function(x,
                                   ...) {
    x$regarima <- set_basic(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_outlier
#' @export
set_outlier.JD3_X13_SPEC <- function(x,
                                     ...) {
    x$regarima <- set_outlier(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_tradingdays
#' @export
set_tradingdays.JD3_X13_SPEC <- function(x,
                                         ...) {
    x$regarima <- set_tradingdays(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_transform
#' @export
set_transform.JD3_X13_SPEC <- function(x,
                                       ...) {
    x$regarima <- set_transform(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit add_usrdefvar
#' @export
add_usrdefvar.JD3_X13_SPEC <- function(x,
                                       ...) {
    x$regarima <- add_usrdefvar(
        x$regarima,
        ...
    )
    x
}
#' @importFrom rjd3toolkit set_benchmarking
#' @export
set_benchmarking.JD3_X13_SPEC <- function(x, ...) {
    x$benchmarking <- set_benchmarking(x$benchmarking, ...)

    x
}
