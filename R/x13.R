#' @include utils.R x13_spec.R x13_rslts.R
NULL

#' @title RegARIMA model, pre-adjustment in X13
#'
#' @param ts an univariate time series.
#' @param spec the model specification. Can be either the name of a predefined
#' specification or a user-defined specification.
#' @param context list of external regressors (calendar or other) to be used for
#' estimation
#' @param userdefined a vector containing additional output variables
#' (see [x13_dictionary()]).
#'
#' @returns the `regarima()` function returns a list with the results
#' (`"JD3_REGARIMA_RSLTS"` object), the estimation specification and the result
#' specification, while `regarima_fast()` is a faster function that only returns
#' the results.
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' \donttest{
#' library("rjd3toolkit")
#'
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' sp <- regarima_spec("rg5c")
#' sp <- add_outlier(sp,
#'     type = c("AO"), c("2015-01-01", "2010-01-01")
#' )
#' regarima_fast(y, spec = sp)
#' sp <- set_transform(
#'     set_tradingdays(
#'         set_easter(sp, enabled = FALSE),
#'         option = "workingdays"
#'     ),
#'     fun = "None"
#' )
#' regarima_fast(y, spec = sp)
#' sp <- set_outlier(sp, outliers.type = c("AO"))
#' regarima_fast(y, spec = sp)
#' }
#'
#' @name regarima
#'
#' @export
#'
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
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined))
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
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}

#' @importFrom RProtoBuf read
.regarima_output <- function(jq) {
    if (is.jnull(jq)) {
        return(NULL)
    }
    q_obj <- .jcall("jdplus/x13/base/r/RegArima", "[B", "toBuffer", jq)
    p <- RProtoBuf::read(x13.RegArimaOutput, q_obj)
    return(structure(
        list(
            result = rjd3toolkit::.p2r_regarima_rslts(p$result),
            estimation_spec = .p2r_spec_regarima(p$estimation_spec),
            result_spec = .p2r_spec_regarima(p$result_spec)
        ),
        class = "JD3_REGARIMA_OUTPUT"
    ))
}

#' @title Seasonal Adjustment with  X13-ARIMA
#'
#' @inheritParams regarima
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' \donttest{
#' library("rjd3toolkit")
#'
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' x13_fast(y, "rsa3")
#' x13(y, "rsa5c")
#' regarima_fast(y, "rg0")
#' regarima(y, "rg3")
#'
#' sp <- x13_spec("rsa5c")
#' sp <- add_outlier(sp,
#'     type = c("AO"), c("2015-01-01", "2010-01-01")
#' )
#' sp <- set_transform(
#'     set_tradingdays(
#'         set_easter(sp, enabled = FALSE),
#'         option = "workingdays"
#'     ),
#'     fun = "None"
#' )
#' x13(y, spec = sp)
#' sp <- set_x11(sp,
#'     henderson.filter = 13
#' )
#' x13_fast(y, spec = sp)
#' j <- jx13(y, spec = sp)
#' class(j)
#' }
#'
#' @returns the `x13()` function returns a list with the results, the estimation
#' specification and the result specification, while `x13_fast()` is a faster
#' function that only returns the results. The `jx13()` functions only returns
#' results in a java object which will allow to customize outputs in other
#' packages (use [rjd3toolkit::dictionary()] to get the list of variables and
#' [rjd3toolkit::result()] to get a specific variable). In the estimation
#' functions `x13()` and `x13_fast()` you can directly use a specification name
#' (string). If you want to customize a specification you have to create a
#' specification object first.
#'
#' @export
#'
#' @name x13
#'
x13 <- function(ts,
                spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
                context = NULL,
                userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("g", "sa", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1], choices = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c"))
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
    }

    res <- .x13_output(jrslt)
    output <- rjd3toolkit::.add_ud_var(
        res,
        jrslt,
        userdefined = userdefined,
        out_class = "Ljdplus/x13/base/core/x13/X13Results;"
    )
    return(output)
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
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}

#' @export
#' @rdname x13
jx13 <- function(ts, spec = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"), context = NULL, userdefined = NULL) {
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

#' @importFrom RProtoBuf read
.x13_output <- function(jq) {
    if (is.jnull(jq)) {
        return(NULL)
    }
    q_obj <- .jcall("jdplus/x13/base/r/X13", "[B", "toBuffer", jq)
    p <- RProtoBuf::read(x13.X13Output, q_obj)
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
#' @returns the `x11()` function returns a list with the results (series) and final parameters
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
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
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}

#' @title Refresh a specification with constraints
#'
#' @description
#' Functions `x13_refresh` and `regarima_refresh` allow to create a new
#' specification by updating an existing one.
#' Some selected parameters will be kept fixed while others will be freed within the boundaries of a
#' reference specification. In practice each freed parameter of the specification to be updated
#' (`spec`) is replaced by the corresponding parameter of the reference specification (`refspec`).
#' See details and examples.
#'
#' @details
#' A particular selection of parameters to be kept fixed or re-estimated is called a
#' revision policy.
#'
#' Available refresh policies are:
#' \enumerate{
#' \item \strong{Current}: applying the current pre-adjustment reg-arima model
#' and handling the new raw data points, or any sub-span of the series as
#' Additive Outliers (defined as new intervention variables);
#' X11 and Benchmarking part parameters are untouched.
#' \item \strong{Fixed}: applying the current pre-adjustment reg-arima model
#' and replacing forecasts by new raw data points;
#' X11 and Benchmarking part parameters are untouched.
#' \item \strong{FixedParameters}: pre-adjustment reg-arima model is partially
#' modified: regression coefficients will be re-estimated but regression
#' variables, Arima orders and coefficients are unchanged;
#' \item \strong{FixedAutoRegressiveParameters}: same as FixedParameters but
#' Arima Moving Average coefficients (MA) are also re-estimated, Auto-regressive
#'  (AR) coefficients are kept fixed;
#' X11 and Benchmarking part parameters are untouched.
#' \item \strong{FreeParameters}: all regression and Arima model coefficients
#' are re-estimated, regression variables and Arima orders are kept fixed;
#' X11 and Benchmarking part parameters are untouched.
#' \item \strong{Outliers}: regression variables and Arima orders are kept
#' fixed, but outliers will be re-detected on the defined span, thus all
#' regression and Arima model coefficients are re-estimated;
#' X11 and Benchmarking part parameters are untouched.
#' \item \strong{Outliers_StochasticComponent}: same as "Outliers" but Arima
#' model orders (p,d,q)(P,D,Q) can also be re-identified;
#' X11 and Benchmarking part parameters are untouched.
#' \item \strong{Complete}: All the parameters are re-identified and
#' re-estimated, unless constrained in the domain spec.
#' X11 and Benchmarking part parameters are entirely reset to values in the reference spec.
#' }
#'
#' @param spec specification to be refreshed
#' Object of class "JD3_X13_SPEC" or "JD3_REGARIMA_SPEC",
#' can be obtained as an output of `x13_spec` or `regarima_spec` and customised with `set_` functions,
#' see `x13_spec` documentation

#' @param refspec reference specification
#' By default `"RG4c"` or `"rsa4"` specification.
#' Object of class "JD3_X13_SPEC" or "JD3_REGARIMA_SPEC",
#' can be obtained as an output of `x13_spec` or `regarima_spec` and customised with `set_` functions,
#' see `x13_spec` documentation
#'
#' @param policy refresh policy to apply (see details)
#'
#' @param period,start,end  additional parameters used to specify the span on
#' which additive outliers (AO) are introduced when `policy = "Current"` or to
#' specify the span on which outliers will be re-detected when
#' `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`, in this
#' last case \code{end} is unused.
#'
#' If \code{start} is not specified, outliers will be re-identified on the whole
#' series.
#' Span definition: \code{period}: numeric, number of observations in a year
#' (12, 4...).
#' \code{start} and \code{end}: defined as arrays of two elements: year and
#' first period (for example, `period = 12` and `c(1980, 1)` stands for January
#' 1980)
#' The dates corresponding to \code{start} and \code{end} are included in the span
#' definition.
#'
#' @returns a new specification, an object of class `"JD3_X13_SPEC"` or
#' `"JD3_REGARIMA_SPEC"`.
#'
#' @references
#' More information on revision policies in JDemetra+ documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-rev-policies}
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' library("rjd3toolkit")
#' \donttest{
#' # Example 1 : refresh mechanism
#' # Create reference spec, here the default "rsa3"
#' rsa3<- x13_spec("rsa3")
#' # Customize this spec
#' ## Reg-Arima part
#' ### For example, disable automatic arima modelling
#' user_spec <- set_automodel(rsa3, enabled = FALSE)
#' ### set a user-defined arima model
#' user_spec <- set_arima(
#'    user_spec,
#'    mean = 0.2,
#'    mean.type = "Fixed",
#'    p = 1,
#'    d = 2,
#'    q = 0,
#'    bp = 1,
#'    bd = 1,
#'    bq = 0,
#'    coef = c(0.6, 0.7),
#'    coef.type = c("Initial", "Fixed")
#')
#' #print(user_spec)
#'
#'## Customize the x11 part
#'user_spec<-set_x11(user_spec,
#'                   lsigma = 2,
#'                   usigma = 3,
#'                   fcasts = -2,
#'                   bcasts = -1)

#' #print(user_spec)
## Customize the benchmarking part
#' user_spec<- set_benchmarking(
#'    user_spec,
#'    enabled = TRUE,
#'    target = "Original",
#'    rho = 0.7,
#'    lambda = 0.5,
#'    forecast = TRUE,
#'    bias = "Multiplicative")

#' #print(user_spec)

#' # Use policy: "Outliers_StochasticComponent"

#' x13_spec_ref <- x13_refresh(spec= user_spec,
#'                            refspec= rsa3,
#'                            policy = "Outliers_StochasticComponent"
#')

#' # print(x13_spec_ref)
#' # user defined reg-arima model is reset and outliers will be re-identified
#' # on the whole series as no start and end specified, X11 and Benchmarking parameters
#' # are left unchanged

#' # Use policy: "Complete"

#' x13_spec_ref <- x13_refresh(spec= user_spec,
#'                            refspec= rsa3,
#'                            policy = "Complete"
#')

#' # print(x13_spec_ref)
#' # all user defined parameters are reset and replaced with "rsa3" parameters,
#' # including for X11 and Benchmarking parameters

#'
#' # Example 2 : practical re-estimation use-case

#' y <- rjd3toolkit::ABS$X0.2.08.10.M

#' # raw series for first estimation
#' y_raw <- window(y, end = c(2016, 12))

#' # raw series for second (refreshed) estimation: new data points
#' y_new <- window(y, end = c(2017, 6))

# '# first estimation
#' sa_x13 <- x13(y_raw, user_spec)

#' # refreshing the specification resulting from the first estimation
#' # to partially adapt it to new data

#' spec_to_refresh <- sa_x13$result_spec
#' reference_spec <- sa_x13$estimation_spec

#' # policy = "Fixed"
#' spec_x13_ref <- x13_refresh(spec_to_refresh,
#'                             reference_spec,
#'                             policy = "Fixed"
#' )
#' # 2nd estimation with refreshed specification
#' sa_x13_ref <- x13(y_new, spec_x13_ref)

#' # policy = "Outliers"
#' spec_x13_ref <- x13_refresh(spec_to_refresh,
#'                            reference_spec,
#'                            policy = "Outliers",
#'                            period = 12,
#'                            start = c(2017, 1)
#')
#'# outliers will be re-detected from January 2017 included
#'# 2nd estimation with refreshed specification
#'sa_x13_ref <- x13(y_new, spec_x13_ref)
#'
#'# policy = "Current"
#'
#'spec_x13_ref <- x13_refresh(spec_to_refresh,
#'                            reference_spec,
#'                            policy = "Current",
#'                            period = 12,
#'                            start = c(2017, 1),
#'                            end = end(y_new)
#')
#'}
#' # Points from January 2017 (included) until the end of the series will be
#' # treated as Additive Outliers, the previous reg-Arima model being otherwise
#' # kept fixed 2nd estimation with refreshed specification
#' sa_x13_ref <- x13(y_new, spec_x13_ref)

# Procedure is the same procedure using regarima_refresh instead of x13_refresh
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
        stop("Invalid specification type", call. = FALSE)
    }
    jspec <- .r2jd_spec_regarima(spec)
    if (is.null(refspec)) {
        jrefspec <- .jcall("jdplus/x13/base/api/regarima/RegArimaSpec",
                           "Ljdplus/x13/base/api/regarima/RegArimaSpec;", "fromString", "rg4")
    } else {
        if (!inherits(refspec, "JD3_REGARIMA_SPEC")) {
            stop("Invalid specification type", call. = FALSE)
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
        stop("Invalid specification type", call. = FALSE)
    }
    jspec <- .r2jd_spec_x13(spec)
    if (is.null(refspec)) {
        jrefspec <- .jcall(
            obj = "jdplus/x13/base/api/x13/X13Spec",
            returnSig = "Ljdplus/x13/base/api/x13/X13Spec;",
            method = "fromString",
            "rsa4"
        )
    } else {
        if (!inherits(refspec, "JD3_X13_SPEC")) {
            stop("Invalid specification type", call. = FALSE)
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

#' @title X-13 Dictionary
#'
#' @description
#' Functions to provide information for all output objects (series, diagnostics,
#' parameters) available with `x13()` function.
#'
#' @returns \code{x13_dictionary()} returns a character vector containing the
#' names of all output objects (series, diagnostics, parameters) available with
#' the `x13()` function, whereas \code{x13_full_dictionary()} returns a
#' \code{data.frame} with format and description, for all the output objects.
#'
#' @name x13_dictionary
#'
#' @details
#' These functions provide lists of output names (series, diagnostics,
#' parameters) available with the \code{x13()} function. These names can be
#' used to generate customized outputs with the userdefined option of the
#' \code{x13()} function (see examples).
#' The \code{x13_full_dictionary} function provides additional information on
#' object format and description.
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' \donttest{
#' # Visualize the dictionary
#' print(x13_dictionary())
#' summary(x13_dictionary())
#'
#' # first 10 lines
#' head(x13_full_dictionary(), n = 10)
#' # For more structured information call `View(x13_full_dictionary())`
#'
#' # Extract names of output of interest
#' user_defined_output <- x13_dictionary()[c(65, 95, 135)]
#' user_defined_output
#'
#' # Generate the corresponding output in an estimation
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' m <- x13(y,"rsa3", userdefined=user_defined_output)
#'
#' # Retrieve user defined output
#' tail(m$user_defined$ylin)
#' m$user_defined$residuals.kurtosis
#' m$user_defined$sa_f
#' }
#'
#' @export
x13_dictionary <- function() {
    output <- .jcall("jdplus/x13/base/r/X13", "[S", "dictionary")
    class(output) <- "JD3_DICTIONARY"
    return(output)
}

#' @export
#' @rdname x13_dictionary
x13_full_dictionary <- function() {
    dico <- .jcall("jdplus/x13/base/r/X13", "[S", "fullDictionary")
    dico <- `dim<-`(dico, c(6, length(dico) / 6))
    dico <- t(dico)
    dico <- `colnames<-`(dico, c("name", "description", "detail", "output", "type", "fullname"))
    dico <- as.data.frame(dico)
    class(dico) <- c("JD3_FULL_DICTIONARY", "data.frame")
    return(dico)
}
