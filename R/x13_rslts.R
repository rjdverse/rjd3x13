#' @include utils.R
#' @importFrom rjd3toolkit sa_decomposition
NULL

.regarima_rslts <- function(jrslts) {
    if (is.jnull(jrslts)) {
        return(NULL)
    }
    q <- .jcall("jdplus/x13/base/r/RegArima", "[B", "toBuffer", jrslts)
    rq <- RProtoBuf::read(regarima.RegArimaModel, q)
    return(rjd3toolkit::.p2r_regarima_rslts(rq))
}

#' @export
#' @rdname jd3_utilities
.x13_rslts <- function(jrslts) {
    if (is.jnull(jrslts)) {
        return(NULL)
    }
    q <- .jcall("jdplus/x13/base/r/X13", "[B", "toBuffer", jrslts)
    rq <- RProtoBuf::read(x13.X13Results, q)
    return(.p2r_x13_rslts(rq))
}

.x11_rslts <- function(jrslts) {
    if (is.jnull(jrslts)) {
        return(NULL)
    }
    q <- .jcall("jdplus/x13/base/r/X11", "[B", "toBuffer", jrslts)
    rq <- RProtoBuf::read(x13.X11Results, q)
    return(.p2r_x11_rslts(rq))
}

.p2r_x13_rslts <- function(p) {
    return(structure(
        list(
            preprocessing = rjd3toolkit::.p2r_regarima_rslts(p$preprocessing),
            preadjust = .p2r_x13_preadjust(p$preadjustment),
            decomposition = .p2r_x11_rslts(p$decomposition),
            final = .p2r_x13_final(p$final),
            mstats = p$diagnostics_x13$mstatistics$as.list(),
            diagnostics = rjd3toolkit::.p2r_sa_diagnostics(p$diagnostics_sa)
        ),
        class = "JD3_X13_RSLTS"
    ))
}

.p2r_x11_rslts <- function(p) {
    return(structure(
        list(
            d1 = rjd3toolkit::.p2r_tsdata(p$d1),
            d2 = rjd3toolkit::.p2r_tsdata(p$d2),
            d4 = rjd3toolkit::.p2r_tsdata(p$d4),
            d5 = rjd3toolkit::.p2r_tsdata(p$d5),
            d6 = rjd3toolkit::.p2r_tsdata(p$d6),
            d7 = rjd3toolkit::.p2r_tsdata(p$d7),
            d8 = rjd3toolkit::.p2r_tsdata(p$d8),
            d9 = rjd3toolkit::.p2r_tsdata(p$d9),
            d10 = rjd3toolkit::.p2r_tsdata(p$d10),
            d11 = rjd3toolkit::.p2r_tsdata(p$d11),
            d12 = rjd3toolkit::.p2r_tsdata(p$d12),
            d13 = rjd3toolkit::.p2r_tsdata(p$d13),
            final_seasonal = p$final_seasonal_filters,
            final_henderson = p$final_henderson_filter
        ),
        class = "JD3X11"
    ))
}


.p2r_x13_final <- function(p) {
    return(list(
        d11final = rjd3toolkit::.p2r_tsdata(p$d11final),
        d12final = rjd3toolkit::.p2r_tsdata(p$d12final),
        d13final = rjd3toolkit::.p2r_tsdata(p$d13final),
        d16 = rjd3toolkit::.p2r_tsdata(p$d16),
        d18 = rjd3toolkit::.p2r_tsdata(p$d18),
        d11a = rjd3toolkit::.p2r_tsdata(p$d11a),
        d12a = rjd3toolkit::.p2r_tsdata(p$d12a),
        d16a = rjd3toolkit::.p2r_tsdata(p$d16a),
        d18a = rjd3toolkit::.p2r_tsdata(p$d18a),
        e1 = rjd3toolkit::.p2r_tsdata(p$e1),
        e2 = rjd3toolkit::.p2r_tsdata(p$e2),
        e3 = rjd3toolkit::.p2r_tsdata(p$e3),
        e11 = rjd3toolkit::.p2r_tsdata(p$e11)
    ))
}

.p2r_x13_preadjust <- function(p) {
    return(list(
        a1 = rjd3toolkit::.p2r_tsdata(p$a1),
        a1a = rjd3toolkit::.p2r_tsdata(p$a1a),
        a1b = rjd3toolkit::.p2r_tsdata(p$a1b),
        a6 = rjd3toolkit::.p2r_tsdata(p$a6),
        a7 = rjd3toolkit::.p2r_tsdata(p$a7),
        a8 = rjd3toolkit::.p2r_tsdata(p$a8),
        a9 = rjd3toolkit::.p2r_tsdata(p$a9)
    ))
}


############################# Generics

#' @export
#' @importFrom rjd3toolkit sa_decomposition
sa_decomposition.JD3_X13_RSLTS <- function(x, ...) {
    if (is.null(x)) {
        return(NULL)
    }
    return(rjd3toolkit::sadecomposition(
        x$preadjust$a1, # y
        x$final$d11final, # sa
        x$final$d12final, # t
        x$final$d16, # s
        x$final$d13final, # i
        x$preprocessing$description$log
    ))
}

#' @export
sa_decomposition.JD3_X13_OUTPUT <- function(x, ...) {
    return(rjd3toolkit::sa_decomposition(x$result, ...))
}
