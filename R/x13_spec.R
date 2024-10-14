#' @include utils.R
NULL


#' @title RegARIMA/X-13 Default Specifications
#'
#' @description
#' Set of functions to create default specification objects associated with the
#' X-13ARIMA seasonal adjustment method.
#'
#' Specification setting of sheer X-11 decomposition method (without reg-arima
#' pre-adjustment) is supported by the `x11_spec()` function only and doesn't
#' appear among the possible X13-Arima default specifications.
#'
#' Specification setting can be restricted to the reg-arima part with the
#' `regarima_spec()` function, without argument `regarima_spec()` yields a RG5c
#' specification.
#'
#' When setting a complete X13-Arima spec, `x13_spec()` without argument yields
#' a RSA5c specification.
#'
#'
#' @param name the name of a predefined specification.
#'
#' @examples
#' init_spec <- x11_spec()
#' init_spec <- regarima_spec("rg4")
#' init_spec <- x13_spec("rsa5c")
#'
#' @return an object of class `"JD3_X13_SPEC"` (`x13_spec()`),
#' `"JD3_REGARIMA_SPEC"` (`regarima_spec()`) or
#' `"JD3_X11_SPEC"` (`x11_spec()`).
#'
#' @details
#' The available predefined 'JDemetra+' model specifications are described in the table below:
#'
#' \tabular{rrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
#' RSA0/RG0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA1/RG1 |\tab automatic |\tab AO/LS/TC  |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA2c/RG2c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RSA3/RG3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RSA4c/RG4c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RSA5c/RG5c |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic
#' }
#' @seealso
#' - To set the pre-processing parameters:
#' [rjd3toolkit::set_arima()], [rjd3toolkit::set_automodel()],
#' [rjd3toolkit::set_basic()], [rjd3toolkit::set_easter()],
#' [rjd3toolkit::set_estimate()], [rjd3toolkit::set_outlier()],
#' [rjd3toolkit::set_tradingdays()], [rjd3toolkit::set_transform()],
#' [rjd3toolkit::add_outlier()], [rjd3toolkit::remove_outlier()],
#' [rjd3toolkit::add_ramp()], [rjd3toolkit::remove_ramp()],
#' [rjd3toolkit::add_usrdefvar()].
#' - To set the decomposition parameters: [set_x11()].
#' - To set the benchmarking parameters: [rjd3toolkit::set_benchmarking()].
#' @name x13_spec
#' @rdname x13_spec
#' @export
regarima_spec <- function(name = c("rg4", "rg0", "rg1", "rg2c", "rg3", "rg5c")) {
    name <- gsub("sa", "g", tolower(name), fixed = TRUE)
    name <- match.arg(name[1],
        choices = c("rg0", "rg1", "rg2c", "rg3", "rg4", "rg5c")
    )
    return(.jd2r_spec_regarima(.jcall("jdplus/x13/base/api/regarima/RegArimaSpec",
                                      "Ljdplus/x13/base/api/regarima/RegArimaSpec;",
                                      "fromString", name)))
}


#' @rdname x13_spec
#' @export
x13_spec <- function(name = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c")) {
    name <- gsub("g", "sa", tolower(name), fixed = TRUE)
    name <- match.arg(name[1],
        choices = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c")
    )
    return(.jd2r_spec_x13(.jcall("jdplus/x13/base/api/x13/X13Spec",
                                 "Ljdplus/x13/base/api/x13/X13Spec;",
                                 "fromString", name)))
}


#' @rdname x13_spec
#' @export
x11_spec <- function() {
    return(.jd2r_spec_x11(.jfield("jdplus/x13/base/api/x11/X11Spec", "Ljdplus/x13/base/api/x11/X11Spec;", "DEFAULT")))
}

#' @export
#' @rdname jd3_utilities
.jd2r_spec_x11 <- function(jspec) {
    b <- .jcall("jdplus/x13/base/r/X11", "[B", "toBuffer", jspec)
    p <- RProtoBuf::read(x13.X11Spec, b)
    return(.p2r_spec_x11(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_spec_x11 <- function(spec) {
    p <- .r2p_spec_x11(spec)
    b <- RProtoBuf::serialize(p, NULL)
    nspec <- .jcall("jdplus/x13/base/r/X11", "Ljdplus/x13/base/api/x11/X11Spec;", "of", b)
    return(nspec)
}

#' @export
#' @rdname jd3_utilities
.r2jd_spec_regarima <- function(spec) {
    p <- .r2p_spec_regarima(spec)
    b <- RProtoBuf::serialize(p, NULL)
    nspec <- .jcall("jdplus/x13/base/r/RegArima", "Ljdplus/x13/base/api/regarima/RegArimaSpec;", "specOf", b)
    return(nspec)
}

#' @export
#' @rdname jd3_utilities
.jd2r_spec_regarima <- function(jspec) {
    b <- .jcall("jdplus/x13/base/r/RegArima", "[B", "toBuffer", jspec)
    p <- RProtoBuf::read(x13.RegArimaSpec, b)
    return(.p2r_spec_regarima(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_spec_x13 <- function(spec) {
    p <- .r2p_spec_x13(spec)
    b <- RProtoBuf::serialize(p, NULL)
    nspec <- .jcall("jdplus/x13/base/r/X13", "Ljdplus/x13/base/api/x13/X13Spec;", "specOf", b)
    return(nspec)
}

#' @export
#' @rdname jd3_utilities
.jd2r_spec_x13 <- function(jspec) {
    b <- .jcall("jdplus/x13/base/r/X13", "[B", "toBuffer", jspec)
    p <- RProtoBuf::read(x13.Spec, b)
    return(.p2r_spec_x13(p))
}

## P <-> R

.p2r_spec_regarima <- function(pspec) {
    basic <- list(
        span = rjd3toolkit::.p2r_span(pspec$basic$span),
        preprocessing = pspec$basic$preprocessing,
        preliminaryCheck = pspec$basic$preliminary_check
    )
    transform <- list(
        fn = rjd3toolkit::.enum_extract(modelling.Transformation, pspec$transform$transformation),
        adjust = rjd3toolkit::.enum_extract(modelling.LengthOfPeriod, pspec$transform$adjust),
        aicdiff = pspec$transform$aicdiff,
        outliers = pspec$transform$outliers_correction
    )
    automodel <- list(
        enabled = pspec$automodel$enabled,
        ljungbox = pspec$automodel$ljungbox,
        tsig = pspec$automodel$tsig,
        predcv = pspec$automodel$predcv,
        ubfinal = pspec$automodel$ubfinal,
        ub1 = pspec$automodel$ub1,
        ub2 = pspec$automodel$ub2,
        cancel = pspec$automodel$cancel,
        fct = pspec$automodel$fct,
        acceptdef = pspec$automodel$acceptdef,
        mixed = pspec$automodel$mixed,
        balanced = pspec$automodel$balanced
    )

    arima <- rjd3toolkit::.p2r_spec_sarima(pspec$arima)

    outlier <- list(
        outliers = lapply(pspec$outlier$outliers, function(z) {
            list(type = z$code, va = z$va)
        }),
        span = rjd3toolkit::.p2r_span(pspec$outlier$span),
        defva = pspec$outlier$defva,
        method = rjd3toolkit::.enum_extract(x13.OutlierMethod, pspec$outlier$method),
        monthlytcrate = pspec$outlier$monthly_tc_rate,
        maxiter = pspec$outlier$maxiter,
        lsrun = pspec$outlier$lsrun
    )

    td <- list(
        td = rjd3toolkit::.enum_sextract(modelling.TradingDays, pspec$regression$td$td),
        lp = rjd3toolkit::.enum_extract(modelling.LengthOfPeriod, pspec$regression$td$lp),
        holidays = pspec$regression$td$holidays,
        users = unlist(pspec$regression$td$users),
        w = pspec$regression$td$w,
        test = rjd3toolkit::.enum_extract(x13.RegressionTest, pspec$regression$td$test),
        auto = rjd3toolkit::.enum_extract(x13.AutomaticTradingDays, pspec$regression$td$auto),
        autoadjust = pspec$regression$td$auto_adjust,
        tdcoefficients = rjd3toolkit::.p2r_parameters(pspec$regression$td$tdcoefficients),
        lpcoefficient = rjd3toolkit::.p2r_parameter(pspec$regression$td$lpcoefficient),
        ptest1 = pspec$regression$td$ptest1,
        ptest2 = pspec$regression$td$ptest2
    )

    easter <- list(
        type = rjd3toolkit::.enum_extract(x13.EasterType, pspec$regression$easter$type),
        duration = pspec$regression$easter$duration,
        test = rjd3toolkit::.enum_extract(x13.RegressionTest, pspec$regression$easter$test),
        coefficient = rjd3toolkit::.p2r_parameter(pspec$regression$easter$coefficient)
    )

    # TODO: complete regression
    regression <- list(
        mean = rjd3toolkit::.p2r_parameter(pspec$regression$mean),
        check_mean = pspec$regression$check_mean,
        td = td,
        easter = easter,
        outliers = rjd3toolkit::.p2r_outliers(pspec$regression$outliers),
        users = rjd3toolkit::.p2r_uservars(pspec$regression$users),
        interventions = rjd3toolkit::.p2r_ivs(pspec$regression$interventions),
        ramps = rjd3toolkit::.p2r_ramps(pspec$regression$ramps)
    )

    estimate <- list(
        span = rjd3toolkit::.p2r_span(pspec$estimate$span),
        tol = pspec$estimate$tol
    )
    return(structure(
        list(
            basic = basic,
            transform = transform,
            outlier = outlier,
            arima = arima,
            automodel = automodel,
            regression = regression,
            estimate = estimate
        ),
        class = "JD3_REGARIMA_SPEC"
    ))
}


.r2p_spec_regarima <- function(r) {
    p <- x13.RegArimaSpec$new()
    # BIAS
    p$basic$preliminary_check <- r$basic$preliminaryCheck
    p$basic$preprocessing <- r$basic$preprocessing
    p$basic$span <- rjd3toolkit::.r2p_span(r$basic$span)

    # TRANSFORM
    p$transform$transformation <- rjd3toolkit::.enum_of(modelling.Transformation, r$transform$fn, "FN")
    p$transform$adjust <- rjd3toolkit::.enum_of(modelling.LengthOfPeriod, r$transform$adjust, "LP")
    p$transform$aicdiff <- r$transform$aicdiff
    p$transform$outliers_correction <- r$transform$outliers

    # OUTLIER
    p$outlier$outliers <- lapply(X = r$outlier$outliers, FUN = function(z) {
        t <- x13.RegArimaSpec$OutlierSpec$Type$new()
        t$code <- z$type
        t$va <- z$va
        return(t)
    })
    p$outlier$span <- rjd3toolkit::.r2p_span(r$outlier$span)
    p$outlier$defva <- r$outlier$defva
    p$outlier$method <- rjd3toolkit::.enum_of(x13.OutlierMethod, r$outlier$method, "OUTLIER")
    p$outlier$monthly_tc_rate <- r$outlier$monthlytcrate
    p$outlier$maxiter <- r$outlier$maxiter
    p$outlier$lsrun <- r$outlier$lsrun

    # AMI

    p$automodel$enabled <- r$automodel$enabled
    p$automodel$ljungbox <- r$automodel$ljungbox
    p$automodel$tsig <- r$automodel$tsig
    p$automodel$predcv <- r$automodel$predcv
    p$automodel$ubfinal <- r$automodel$ubfinal
    p$automodel$ub1 <- r$automodel$ub1
    p$automodel$ub2 <- r$automodel$ub2
    p$automodel$cancel <- r$automodel$cancel
    p$automodel$fct <- r$automodel$fct
    p$automodel$acceptdef <- r$automodel$acceptdef
    p$automodel$mixed <- r$automodel$mixed
    p$automodel$balanced <- r$automodel$balanced

    # ARIMA
    p$arima <- rjd3toolkit::.r2p_spec_sarima(r$arima)

    # REGRESSION

    p$regression$mean <- rjd3toolkit::.r2p_parameter(r$regression$mean)
    p$regression$check_mean <- r$regression$check_mean
    p$regression$outliers <- rjd3toolkit::.r2p_outliers(r$regression$outliers)
    p$regression$users <- rjd3toolkit::.r2p_uservars(r$regression$users)
    p$regression$interventions <- rjd3toolkit::.r2p_ivs(r$regression$interventions)
    p$regression$ramps <- rjd3toolkit::.r2p_ramps(r$regression$ramps)

    # TD
    p$regression$td$td <- rjd3toolkit::.enum_sof(modelling.TradingDays, r$regression$td$td)
    p$regression$td$lp <- rjd3toolkit::.enum_of(modelling.LengthOfPeriod, r$regression$td$lp, "LP")
    p$regression$td$holidays <- r$regression$td$holidays
    p$regression$td$users <- r$regression$td$users
    p$regression$td$w <- r$regression$td$w
    p$regression$td$test <- rjd3toolkit::.enum_of(x13.RegressionTest, r$regression$td$test, "TEST")
    p$regression$td$auto <- rjd3toolkit::.enum_of(x13.AutomaticTradingDays, r$regression$td$auto, "TD")
    p$regression$td$auto_adjust <- r$regression$td$autoadjust
    p$regression$td$tdcoefficients <- rjd3toolkit::.r2p_parameters(r$regression$td$tdcoefficients)
    p$regression$td$lpcoefficient <- rjd3toolkit::.r2p_parameter(r$regression$td$lpcoefficient)
    p$regression$td$ptest1 <- r$regression$td$ptest1
    p$regression$td$ptest2 <- r$regression$td$ptest2

    # EASTER
    p$regression$easter$type <- rjd3toolkit::.enum_of(x13.EasterType, r$regression$easter$type, "EASTER")
    p$regression$easter$duration <- r$regression$easter$duration
    p$regression$easter$test <- rjd3toolkit::.enum_of(x13.RegressionTest, r$regression$easter$test, "TEST")
    p$regression$easter$coefficient <- rjd3toolkit::.r2p_parameter(r$regression$easter$coefficient)

    # ESTIMATE
    p$estimate$span <- rjd3toolkit::.r2p_span(r$estimate$span)
    p$estimate$tol <- r$estimate$tol

    return(p)
}


.p2r_spec_x11 <- function(p) {
    return(structure(list(
        mode = rjd3toolkit::.enum_extract(sa.DecompositionMode, p$mode),
        seasonal = p$seasonal,
        henderson = p$henderson,
        sfilters = sapply(p$sfilters, function(z) {
            rjd3toolkit::.enum_extract(x13.SeasonalFilter, z)
        }),
        lsig = p$lsig,
        usig = p$usig,
        nfcasts = p$nfcasts,
        nbcasts = p$nbcasts,
        sigma = rjd3toolkit::.enum_extract(x13.CalendarSigma, p$sigma),
        vsigmas = p$vsigmas,
        excludefcasts = p$exclude_fcasts,
        bias = rjd3toolkit::.enum_extract(x13.BiasCorrection, p$bias)
    ), class = "JD3_X11_SPEC"))
}


.r2p_spec_x11 <- function(r) {
    p <- x13.X11Spec$new()
    p$mode <- rjd3toolkit::.enum_of(x13.DecompositionMode, r$mode, "MODE")
    p$seasonal <- r$seasonal
    p$henderson <- r$henderson
    p$sfilters <- sapply(r$sfilters, function(z) {
        rjd3toolkit::.enum_of(x13.SeasonalFilter, z, "SEASONAL")
    })
    p$lsig <- r$lsig
    p$usig <- r$usig
    p$nfcasts <- r$nfcasts
    p$nbcasts <- r$nbcasts
    p$sigma <- rjd3toolkit::.enum_of(x13.CalendarSigma, r$sigma, "SIGMA")
    p$vsigmas <- r$vsigmas
    p$exclude_fcasts <- r$excludefcasts
    p$bias <- rjd3toolkit::.enum_of(x13.BiasCorrection, r$bias, "BIAS")
    return(p)
}

.p2r_spec_x13 <- function(pspec) {
    return(structure(list(
        regarima = .p2r_spec_regarima(pspec$regarima),
        x11 = .p2r_spec_x11(pspec$x11),
        benchmarking = rjd3toolkit::.p2r_spec_benchmarking(pspec$benchmarking)
    ), class = "JD3_X13_SPEC"))
}

.r2p_spec_x13 <- function(r) {
    p <- x13.Spec$new()
    p$regarima <- .r2p_spec_regarima(r$regarima)
    p$x11 <- .r2p_spec_x11(r$x11)
    p$benchmarking <- rjd3toolkit::.r2p_spec_benchmarking(r$benchmarking)
    return(p)
}
