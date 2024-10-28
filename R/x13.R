#' @include utils.R x13_spec.R x13_rslts.R
NULL

#' RegARIMA model, pre-adjustment in X13
#'
#' @param ts an univariate time series.
#' @param spec the model specification. Can be either the name of a predefined
#' specification or a user-defined specification.
#' @param context list of external regressors (calendar or other) to be used for
#' estimation
#' @param userdefined a vector containing additional output variables
#' (see [x13_dictionary()]).
#'
#' @return the `regarima()` function returns a list with the results
#' (`"JD3_REGARIMA_RSLTS"` object), the estimation specification and the result
#' specification, while `regarima_fast()` is a faster function that only returns
#' the results.
#'
#' @examples
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' sp <- regarima_spec("rg5c")
#' sp <- rjd3toolkit::add_outlier(sp,
#'     type = c("AO"), c("2015-01-01", "2010-01-01")
#' )
#' regarima_fast(y, spec = sp)
#' sp <- rjd3toolkit::set_transform(
#'     rjd3toolkit::set_tradingdays(
#'         rjd3toolkit::set_easter(sp, enabled = FALSE),
#'         option = "workingdays"
#'     ),
#'     fun = "None"
#' )
#' regarima_fast(y, spec = sp)
#' sp <- rjd3toolkit::set_outlier(sp, outliers.type = c("AO"))
#' regarima_fast(y, spec = sp)
#' @export
regarima <- function(ts, spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
                     context = NULL, userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("sa", "g", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rg0", "rg1", "rg2c", "rg3", "rg4", "rg5c")
        )
        jrslt <- .jcall("jdplus/x13/base/r/RegArima",
                        "Ljdplus/x13/base/core/x13/regarima/RegArimaOutput;",
                        "fullProcess", jts, spec)
    } else {
        jspec <- .r2jd_spec_regarima(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/x13/base/r/RegArima",
                        "Ljdplus/x13/base/core/x13/regarima/RegArimaOutput;",
                        "fullProcess", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .regarima_output(jrslt)
        return(.add_ud_var(res, jrslt, userdefined = userdefined))
    }
}
#' @export
#' @rdname regarima
regarima_fast <- function(ts,
                          spec = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c"),
                          context = NULL,
                          userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("sa", "g", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rg0", "rg1", "rg2c", "rg3", "rg4", "rg5c")
        )
        jrslt <- .jcall("jdplus/x13/base/r/RegArima",
                        "Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;",
                        "process", jts, spec)
    } else {
        jspec <- .r2jd_spec_regarima(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/x13/base/r/RegArima",
                        "Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;",
                        "process", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .regarima_rslts(jrslt)
        return(.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}

.regarima_output <- function(jq) {
    if (is.jnull(jq)) {
        return(NULL)
    }
    q <- .jcall("jdplus/x13/base/r/RegArima", "[B", "toBuffer", jq)
    p <- RProtoBuf::read(x13.RegArimaOutput, q)
    return(structure(
        list(
            result = rjd3toolkit::.p2r_regarima_rslts(p$result),
            estimation_spec = .p2r_spec_regarima(p$estimation_spec),
            result_spec = .p2r_spec_regarima(p$result_spec)
        ),
        class = "JD3_REGARIMA_OUTPUT"
    ))
}

#' Seasonal Adjustment with  X13-ARIMA
#'
#' @inheritParams regarima
#'
#'
#'
#' @examples
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' x13_fast(y, "rsa3")
#' x13(y, "rsa5c")
#' regarima_fast(y, "rg0")
#' regarima(y, "rg3")
#'
#' sp <- x13_spec("rsa5c")
#' sp <- rjd3toolkit::add_outlier(sp,
#'     type = c("AO"), c("2015-01-01", "2010-01-01")
#' )
#' sp <- rjd3toolkit::set_transform(
#'     rjd3toolkit::set_tradingdays(
#'         rjd3toolkit::set_easter(sp, enabled = FALSE),
#'         option = "workingdays"
#'     ),
#'     fun = "None"
#' )
#' x13(y, spec = sp)
#' sp <- set_x11(sp,
#'     henderson.filter = 13
#' )
#' x13_fast(y, spec = sp)
#'
#' @return the `x13()` function returns a list with the results, the estimation
#' specification and the result specification, while `x13_fast()` is a faster
#' function that only returns the results. The `.jx13()` functions only returns
#' results in a java object which will allow to customize outputs in other
#' packages (use [rjd3toolkit::dictionary()] to get the list of variables and
#' [rjd3toolkit::result()] to get a specific variable). In the estimation
#' functions `x13()` and `x13_fast()` you can directly use a specification name
#' (string). If you want to customize a specification you have to create a
#' specification object first.
#'
#' @export
#'
x13 <- function(ts,
                spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
                context = NULL,
                userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("g", "sa", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c")
        )
        jrslt <- .jcall("jdplus/x13/base/r/X13",
                        "Ljdplus/x13/base/core/x13/X13Output;",
                        "fullProcess", jts, spec)
    } else {
        jspec <- .r2jd_spec_x13(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/x13/base/r/X13",
                        "Ljdplus/x13/base/core/x13/X13Output;",
                        "fullProcess", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .x13_output(jrslt)
        return(.add_ud_var(res, jrslt, userdefined = userdefined, out_class = "Ljdplus/x13/base/core/x13/X13Results;"))
    }
}


#' @export
#' @rdname x13
x13_fast <- function(ts,
                     spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
                     context = NULL,
                     userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("g", "sa", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c")
        )
        jrslt <- .jcall("jdplus/x13/base/r/X13", "Ljdplus/x13/base/core/x13/X13Results;", "process", jts, spec)
    } else {
        jspec <- .r2jd_spec_x13(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/x13/base/r/X13",
                        "Ljdplus/x13/base/core/x13/X13Results;",
                        "process", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .x13_rslts(jrslt)
        return(.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}

#' @export
#' @rdname x13
.jx13 <- function(ts, spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"), context = NULL, userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("g", "sa", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c")
        )
        jrslt <- .jcall("jdplus/x13/base/r/X13", "Ljdplus/x13/base/core/x13/X13Output;", "fullProcess", jts, spec)
    } else {
        jspec <- .r2jd_spec_x13(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/x13/base/r/X13",
                        "Ljdplus/x13/base/core/x13/X13Output;",
                        "fullProcess", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        jrslt <- .jcall(jrslt, "Ljdplus/x13/base/core/x13/X13Results;", "getResult")
        res <- rjd3toolkit::.jd3_object(jrslt, result = TRUE)
        return(res)
    }
}

.x13_output <- function(jq) {
    if (is.jnull(jq)) {
        return(NULL)
    }
    q <- .jcall("jdplus/x13/base/r/X13", "[B", "toBuffer", jq)
    p <- RProtoBuf::read(x13.X13Output, q)
    return(structure(
        list(
            result = .p2r_x13_rslts(p$result),
            estimation_spec = .p2r_spec_x13(p$estimation_spec),
            result_spec = .p2r_spec_x13(p$result_spec)
        ),
        class = "JD3_X13_OUTPUT"
    ))
}

#' X-11 Decomposition Algorithm
#'
#' @inheritParams x13
#' @param spec the specification.
#'
#' @examples
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' x11_spec <- x11_spec()
#' x11(y, x11_spec)
#' x11_spec <- set_x11(x11_spec, henderson.filter = 13)
#' x11(y, x11_spec)
#' @export
x11 <- function(ts, spec = x11_spec(), userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    jspec <- .r2jd_spec_x11(spec)
    jrslt <- .jcall("jdplus/x13/base/r/X11", "Ljdplus/x13/base/core/x11/X11Results;", "process", jts, jspec)
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .x11_rslts(jrslt)
        return(.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}

#' Refresh a specification with constraints
#'
#' @description
#' Function allowing to create a new specification by updating a specification
#' used for a previous estimation. Some selected parameters will be kept fixed
#' (previous estimation results) while others will be freed for re-estimation in
#' a domain of constraints. See details and examples.
#'
#' @details
#' The selection of constraints to be kept fixed or re-estimated is called a
#' revision policy. User-defined parameters are always copied to the new
#' refreshed specifications. In X-13 only the reg-arima part can be refreshed.
#' X-11 decomposition will be completely re-run, keeping all the user-defined
#' parameters from the original specification.
#'
#' Available refresh policies are:
#'
#' \strong{Current}: applying the current pre-adjustment reg-arima model and
#' handling the new raw data points, or any sub-span of the series as Additive
#' Outliers (defined as new intervention variables)
#'
#' \strong{Fixed}: applying the current pre-adjustment reg-arima model and
#' replacing forecasts by new raw data points.
#'
#' \strong{FixedParameters}: pre-adjustment reg-arima model is partially
#' modified: regression coefficients will be re-estimated but regression
#' variables, Arima orders and coefficients are unchanged.
#'
#' \strong{FixedAutoRegressiveParameters}: same as FixedParameters but Arima
#' Moving Average coefficients (MA) are also re-estimated, Auto-regressive (AR)
#' coefficients are kept fixed.
#'
#' \strong{FreeParameters}: all regression and Arima model coefficients are
#' re-estimated, regression variables and Arima orders are kept fixed.
#'
#' \strong{Outliers}: regression variables and Arima orders are kept fixed, but
#' outliers will be re-detected on the defined span, thus all regression and
#' Arima model coefficients are re-estimated
#'
#' \strong{Outliers_StochasticComponent}: same as "Outliers" but Arima model
#' orders (p,d,q)(P,D,Q) can also be re-identified.
#'
#' @param spec the current specification to be refreshed (`"result_spec"`).
#' @param refspec the reference specification used to define the domain
#' considered for re-estimation (`"domain_spec"`).
#' By default this is the `"RG5c"` or `"RSA5"` specification.
#' @param policy the refresh policy to apply (see details).
#'
#' @param period,start,end  additional parameters used to specify the span on
#' which additive outliers (AO) are introduced when `policy = "Current"` or to
#' specify the span on which outliers will be re-detected when
#' `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`, is this
#' case \code{end} is unused.
#' If \code{start} is not specified, outliers will be re-identified on the whole
#' series.
#' Span definition: \code{period}: numeric, number of observations in a year
#' (12, 4...).
#' \code{start} and \code{end}: defined as arrays of two elements: year and
#' first period (for example, `period = 12` and `c(1980, 1)` stands for January
#' 1980)
#' The dates corresponding \code{start} and \code{end} are included in the span
#' definition.
#'
#' @return a new specification, an object of class `"JD3_X13_SPEC"` or
#' `"JD3_REGARIMA_SPEC"`.
#'
#' @references
#' More information on revision policies in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/t-rev-policies-production}
#'
#' @examples
#' y <- rjd3toolkit::ABS$X0.2.08.10.M
#' # raw series for first estimation
#' y_raw <- window(y, end = c(2016, 12))
#' # raw series for second (refreshed) estimation
#' y_new <- window(y, end = c(2017, 6))
#' # specification for first estimation
#' spec_x13_1 <- x13_spec("rsa5c")
#' # first estimation
#' sa_x13 <- x13(y_raw, spec_x13_1)
#' # refreshing the specification
#' current_result_spec <- sa_x13$result_spec
#' current_domain_spec <- sa_x13$estimation_spec
#' # policy = "Fixed"
#' spec_x13_ref <- x13_refresh(current_result_spec, # point spec to be refreshed
#'     current_domain_spec, # domain spec (set of constraints)
#'     policy = "Fixed"
#' )
#' # 2nd estimation with refreshed specification
#' sa_x13_ref <- x13(y_new, spec_x13_ref)
#' # policy = "Outliers"
#' spec_x13_ref <- x13_refresh(current_result_spec,
#'     current_domain_spec,
#'     policy = "Outliers",
#'     period = 12,
#'     start = c(2017, 1)
#' ) # outliers will be re-detected from January 2017 included
#' # 2nd estimation with refreshed specification
#' sa_x13_ref <- x13(y_new, spec_x13_ref)
#'
#' # policy = "Current"
#' spec_x13_ref <- x13_refresh(current_result_spec,
#'     current_domain_spec,
#'     policy = "Current",
#'     period = 12,
#'     start = c(2017, 1),
#'     end = end(y_new)
#' )
#' # points from January 2017 (included) until the end of the series will be treated
#' # as Additive Outliers, the previous reg-Arima model being otherwise kept fixed
#' # 2nd estimation with refreshed specification
#' sa_x13_ref <- x13(y_new, spec_x13_ref)
#'
#' @name refresh
#' @rdname refresh
#' @export
regarima_refresh <- function(spec,
                             refspec = NULL,
                             policy = c("FreeParameters", "Complete",
                                        "Outliers_StochasticComponent",
                                        "Outliers", "FixedParameters",
                                        "FixedAutoRegressiveParameters",
                                        "Fixed", "Current"),
                             period = 0,
                             start = NULL,
                             end = NULL) {
    policy <- match.arg(policy)
    if (!inherits(spec, "JD3_REGARIMA_SPEC")) {
        stop("Invalid specification type")
    }
    jspec <- .r2jd_spec_regarima(spec)
    if (is.null(refspec)) {
        jrefspec <- .jcall("jdplus/x13/base/api/regarima/RegArimaSpec",
                           "Ljdplus/x13/base/api/regarima/RegArimaSpec;", "fromString", "rg4")
    } else {
        if (!inherits(refspec, "JD3_REGARIMA_SPEC")) {
            stop("Invalid specification type")
        }
        jrefspec <- .r2jd_spec_regarima(refspec)
    }
    if (policy == "Current") {
        if (end[2] == period) end <- c(end[1] + 1, 1) else end <- c(end[1], end[2] + 1)
        jdom <- rjd3toolkit::.jdomain(period, start, end)
    } else if (policy == "Outliers") {
        jdom <- rjd3toolkit::.jdomain(period, NULL, start)
    } else {
        jdom <- jdom <- rjd3toolkit::.jdomain(0, NULL, NULL)
    }
    jnspec <- .jcall("jdplus/x13/base/r/RegArima",
                     "Ljdplus/x13/base/api/regarima/RegArimaSpec;",
                     "refreshSpec", jspec, jrefspec, jdom, policy)
    return(.jd2r_spec_regarima(jnspec))
}

#' @rdname refresh
#' @export
x13_refresh <- function(spec,
                        refspec = NULL,
                        policy = c("FreeParameters", "Complete",
                                   "Outliers_StochasticComponent", "Outliers",
                                   "FixedParameters",
                                   "FixedAutoRegressiveParameters", "Fixed",
                                   "Current"),
                        period = 0,
                        start = NULL,
                        end = NULL) {
    policy <- match.arg(policy)
    if (!inherits(spec, "JD3_X13_SPEC")) {
        stop("Invalid specification type")
    }
    jspec <- .r2jd_spec_x13(spec)
    if (is.null(refspec)) {
        jrefspec <- .jcall("jdplus/x13/base/api/x13/X13Spec", "Ljdplus/x13/base/api/x13/X13Spec;", "fromString", "rsa4")
    } else {
        if (!inherits(refspec, "JD3_X13_SPEC")) {
            stop("Invalid specification type")
        }
        jrefspec <- .r2jd_spec_x13(refspec)
    }
    if (policy == "Current") {
        if (end[2] == period) end <- c(end[1] + 1, 1) else end <- c(end[1], end[2] + 1)
        jdom <- rjd3toolkit::.jdomain(period, start, end)
    } else if (policy %in% c("Outliers", "Outliers_StochasticComponent")) {
        jdom <- rjd3toolkit::.jdomain(period, NULL, start)
    } else {
        jdom <- rjd3toolkit::.jdomain(0, NULL, NULL)
    }
    jnspec <- .jcall(
        obj = "jdplus/x13/base/r/X13",
        returnSig = "Ljdplus/x13/base/api/x13/X13Spec;",
        method = "refreshSpec",
        jspec, jrefspec, jdom, policy
    )
    return(.jd2r_spec_x13(jnspec))
}

#' X-13 Dictionary
#'
#' @return A vector containing the names of all the available output objects (series, diagnostics, parameters).
#' @export
x13_dictionary <- function() {
    return(.jcall("jdplus/x13/base/r/X13", "[S", "dictionary"))
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
x13_full_dictionary <- function() {
    q <- .jcall("jdplus/x13/base/r/X13", "[S", "fullDictionary")
    q <- `dim<-`(q, c(6, length(q) / 6))
    q <- t(q)
    q <- `colnames<-`(q, c("name", "description", "detail", "output", "type", "fullname"))
    return(q)
}
