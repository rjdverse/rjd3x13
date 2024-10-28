#' @importFrom stats printCoefmat
#' @importFrom utils capture.output
print_x11_decomp <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    mstats <- matrix(unlist(x$mstats),
        ncol = 1,
        dimnames = list(names(x$mstats), "M stats")
    )
    cat(
        "Monitoring and Quality Assessment Statistics:",
        "\n"
    )
    printCoefmat(mstats, digits = digits, P.values = FALSE, na.print = "NA", ...)
    cat("\n")
    cat("Final filters:", "\n")
    cat(sprintf("Seasonal filter: S3X%s", x$decomposition$final_seasonal))
    cat("\n")
    cat(sprintf("Trend filter: %s terms Henderson moving average", x$decomposition$final_henderson))
    cat("\n")
    return(invisible(x))
}
print_diagnostics <- function(x, digits = max(3L, getOption("digits") - 3L),
                              ...) {
    variance_decomposition <- x$variance_decomposition
    residual_tests <- x$residual_tests

    cat("Relative contribution of the components to the stationary",
        "portion of the variance in the original series,",
        "after the removal of the long term trend (in %)",
        sep = "\n"
    )
    cat("\n")
    cat(
        paste0(
            " ",
            capture.output(
                printCoefmat(variance_decomposition * 100, digits = digits, ...)
            )
        ),
        sep = "\n"
    )
    cat("\n")

    cat("Residual seasonality tests")
    cat("\n")
    cat(
        paste0(
            " ",
            capture.output(
                printCoefmat(residual_tests[, "P.value", drop = FALSE],
                    digits = digits,
                    na.print = "NA", ...
                )
            )
        ),
        sep = "\n"
    )
    cat("\n")

    return(invisible(x))
}

#' @export
print.JD3_X13_RSLTS <- function(x, digits = max(3L, getOption("digits") - 3L), summary_info = getOption("summary_info"),
                                thresholds_pval = getOption("thresholds_pval"),
                                ...) {
    cat("Model: X-13\n")
    print(x$preprocessing, digits = digits, summary_info = FALSE, ...)
    cat("\n")
    cat(sprintf("Seasonal filter: S3X%s; ", x$decomposition$final_seasonal))
    cat(sprintf("Trend filter: H-%s terms\n", x$decomposition$final_henderson))
    cat(
        sprintf("M-Statistics: q %s (%.3f); q-m2 %s (%.3f)\n",
                ifelse(x$mstats$q <= 1, "Good", "Bad"),
                x$mstats$q,
                ifelse(x$mstats$qm2 <= 1, "Good", "Bad"),
                x$mstats$qm2
        )
    )
    cat(
        sprintf("QS test on SA: %s (%.3f); ",
                base::cut(x$diagnostics$seas.qstest.sa$pvalue, breaks = c(0, thresholds_pval),
                          labels = names(thresholds_pval)),
                x$diagnostics$seas.qstest.sa$pvalue
                )
    )
    cat(
        sprintf("F-test on SA: %s (%.3f)\n",
                base::cut(x$diagnostics$seas.ftest.sa$pvalue, breaks = c(0, thresholds_pval),
                          labels = names(thresholds_pval)),
                x$diagnostics$seas.ftest.sa$pvalue
        )
    )
    if (summary_info) {
        cat("\nFor a more detailed output, use the 'summary()' function.\n")
    }
    return(invisible(x))
}

#' @export
summary.JD3_X13_RSLTS <- function(object, ...) {
    x <- list(
        preprocessing = summary(object$preprocessing),
        decomposition = object[c("mstats", "decomposition")],
        diagnostics = rjd3toolkit::diagnostics(object),
        final = rjd3toolkit::sa_decomposition(object)
    )
    class(x) <- "summary.JD3_X13_RSLTS"
    return(x)
}

#' @export
summary.JD3_X13_OUTPUT <- function(object, ...) {
    summary(object$result, ...)
}

#' @export
print.summary.JD3_X13_RSLTS <- function(x,
                                        digits = max(3L, getOption("digits") - 3L),
                                        signif.stars = getOption("show.signif.stars"),
                                        ...) {
    cat("Model: X-13\n")
    print(x$preprocessing, digits = digits, signif.stars = signif.stars, ...)
    cat("\n", "Decomposition", "\n", sep = "")
    print_x11_decomp(x$decomposition, digits = digits, ...)
    cat("\n", "Diagnostics", "\n", sep = "")
    print_diagnostics(x$diagnostics, digits = digits, ...)
    cat("\n", "Final", "\n", sep = "")
    print(x$final, digits = digits, ...)
    return(invisible(x))
}

#' @export
print.JD3_X13_OUTPUT <- function(x,
                                 digits = max(3L, getOption("digits") - 3L),
                                 summary_info = getOption("summary_info"),
                                 ...) {
    print(x$result, digits = digits, summary_info = summary_info, ...)
    return(invisible(x))
}

#' @export
print.JD3X11 <- function(x, ...) {
    table <- do.call(cbind, x[grepl(pattern = "^d(\\d+)$", x = names(x))])

    cat("Last values\n")
    print(utils::tail(stats::.preformat.ts(table)))

    return(invisible(x))
}


#' @export
plot.JD3_X13_RSLTS <- function(x, first_date = NULL, last_date = NULL,
                               type_chart = c("sa-trend", "seas-irr"),
                               caption = c(
                                   "sa-trend" = "Y, Sa, trend",
                                   "seas-irr" = "Sea., irr."
                               )[type_chart],
                               colors = c(
                                   y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                   s = "#1E6C0B", i = "#155692"
                               ),
                               ...) {
    plot(rjd3toolkit::sa_decomposition(x),
        first_date = first_date, last_date = last_date,
        type_chart = type_chart,
        caption = caption,
        colors = colors,
        ...
    )
}
#' @export
plot.JD3_X13_OUTPUT <- function(x, first_date = NULL, last_date = NULL,
                                type_chart = c("sa-trend", "seas-irr"),
                                caption = c(
                                    "sa-trend" = "Y, Sa, trend",
                                    "seas-irr" = "Sea., irr."
                                )[type_chart],
                                colors = c(
                                    y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                    s = "#1E6C0B", i = "#155692"
                                ),
                                ...) {
    plot(x$result,
        first_date = first_date, last_date = last_date,
        type_chart = type_chart,
        caption = caption,
        colors = colors,
        ...
    )
}

#' @importFrom rjd3toolkit diagnostics
#' @export
diagnostics.JD3_X13_RSLTS <- function(x, ...) {
    if (is.null(x)) {
        return(NULL)
    }
    variance_decomposition <- x$diagnostics$vardecomposition
    variance_decomposition <- matrix(unlist(variance_decomposition),
        ncol = 1,
        dimnames = list(names(variance_decomposition), "Component")
    )
    residual_tests <- x$diagnostics[grep("test", names(x$diagnostics))]
    residual_tests <- data.frame(
        Statistic = sapply(residual_tests, function(test) test[["value"]]),
        P.value = sapply(residual_tests, function(test) test[["pvalue"]]),
        Description = sapply(residual_tests, function(test) attr(test, "distribution"))
    )
    list(
        preprocessing = rjd3toolkit::diagnostics(x$preprocessing),
        variance_decomposition = variance_decomposition,
        residual_tests = residual_tests
    )
}

#' @export
diagnostics.JD3_X13_OUTPUT <- function(x, ...) {
    return(rjd3toolkit::diagnostics(x$result, ...))
}


#' @export
print.JD3_REGARIMA_SPEC <- function(x, ...) {
    cat("Specification", "\n", sep = "")


    cat("\n", "Series", "\n", sep = "")

    cat("Serie span: ")
    print(x$basic$span)

    cat("Preliminary Check: ", ifelse(x$basic$preliminaryCheck, "Yes", "No"), "\n", sep = "")


    cat("\n", "Estimate", "\n", sep = "")

    cat("Model span: ")
    print(x$estimate$span)
    cat("\n")
    cat("Tolerance: ", x$estimate$tol, "\n", sep = "")


    cat("\n", "Transformation", "\n", sep = "")

    cat("Function: ", x$transform$fn, "\n", sep = "")
    cat("AIC difference: ", x$transform$aicdiff, "\n", sep = "")
    cat("Adjust: ", x$transform$adjust, "\n", sep = "")


    cat("\n", "Regression", "\n", sep = "")

    if (!is.null(x$regression$td$users) && length(x$regression$td$users) > 0) {
        cat("Calendar regressor: user-defined calendar", "\n", sep = "")
        cat("Test: ", x$regression$td$test, "\n", sep = "")
    } else if (x$regression$td$w > 0) {
        cat("No calendar regressor", "\n", sep = "")
    } else if (x$regression$td$td == "TD_NONE") {
        cat("No calendar regressor", "\n", sep = "")
    } else {
        if (x$regression$td$td == "TD7") {
            cat("Calendar regressor: TradingDays\n", sep = "")
        } else if (x$regression$td$td == "TD2") {
            cat("Calendar regressor: WorkingDays\n", sep = "")
        } else if (x$regression$td$td %in% c("TD3", "TD3C", "TD4")) {
            cat("Calendar regressor: ", x$regression$td$td, "\n", sep = "")
        } else {
            message("Trading days regressor unknown.")
        }
        cat("with Leap Year: ",
            ifelse(x$regression$td$lp == "LEAPYEAR", "Yes", "No"), "\n",
            sep = ""
        )
        cat("AutoAdjust: ", x$regression$td$autoadjust, "\n", sep = "")
        cat("Test: ", x$regression$td$test, "\n", sep = "")
    }

    cat("\n")

    cat("Easter: ")
    if (x$regression$easter$type == "UNUSED") {
        cat("No\n")
    } else {
        cat(x$regression$easter$type, "\n")
        cat("Duration:", x$regression$easter$duration, ifelse(x$regression$easter$duration == 8, "(Auto)", ""), "\n")
        cat("Test:", x$regression$easter$test, ifelse(x$regression$easter$test == "ADD", "(Auto)", ""), "\n")

        if (!is.null(x$regression$easter$coef)) {
            cat("Coef:\n")
            cat(
                "\t- Type:", x$regression$easter$coefficient$type,
                ifelse(x$regression$easter$coefficient$type == "FIXED", "(Auto)", ""), "\n"
            )
            cat("\t- Value:", x$regression$easter$coefficient$value, "\n")
        }
    }

    cat("\n")

    cat("Pre-specified outliers: ", length(x$regression$outliers), "\n", sep = "")
    if (!is.null(x$regression$outliers) && length(x$regression$outliers) > 0) {
        for (out in x$regression$outliers) {
            cat("\t- ", out$name,
                ifelse(is.null(out$coef), "", paste0(", coefficient: ", out$coef$value, " (", out$coef$type, ")")),
                "\n",
                sep = ""
            )
        }
    }
    cat("Ramps: ")
    if (!is.null(x$regression$ramps) && length(x$regression$ramps) > 0) {
        cat("\n")
        for (ramp in x$regression$ramps) {
            cat("\t- start: ", ramp$start, ", end : ", ramp$end,
                ifelse(is.null(ramp$coef), "", paste0(", coefficient: ", ramp$coef, " (", ramp$coef$type, ")")),
                sep = ""
            )
            cat("\n")
        }
    } else {
        cat("No\n")
    }

    if (!is.null(x$regression$users) && length(x$regression$users) > 0) {
        cat("User-defined variables:\n")
        for (uv in x$regression$users) {
            cat("\t-", uv$name,
                ifelse(is.null(uv$coef), "", paste0(", coefficient: ", uv$coef)),
                ", component: ", uv$regeffect, "\n",
                sep = ""
            )
        }
    }

    cat("\n", "Outliers", "\n", sep = "")

    if (is.null(x$outlier$outliers) || length(x$outlier$outliers) == 0) {
        cat("Is enabled: No\n")
    } else {
        cat("Detection span: ")
        print(x$outlier$span)

        cat("Outliers type: \n")
        for (out in x$outlier$outliers) {
            cat("\t- ", out$type, ", critical value : ", out$va, ifelse(out$va == 0, " (Auto)", ""), "\n", sep = "")
        }

        cat("TC rate: ", x$outlier$monthlytcrate, ifelse(x$outlier$monthlytcrate == 0.7, " (Auto)", ""), "\n", sep = "")
        cat("Method: ", x$outlier$method, ifelse(x$outlier$method == "ADDONE", " (Auto)", ""), "\n", sep = "")
    }


    cat("\n", "ARIMA", "\n", sep = "")

    print(x$arima)

    return(invisible(x))
}

#' @export
print.JD3_X11_SPEC <- function(x, ...) {
    cat("Specification X11", "\n", sep = "")


    cat("Seasonal component: ", ifelse(x$seasonal, "Yes", "No"), "\n", sep = "")
    cat("Length of the Henderson filter: ", x$henderson, "\n", sep = "")
    cat("Seasonal filter: ", x$sfilters, "\n", sep = "")
    cat(
        "Boundaries used for extreme values correction :",
        "\n\t lower_sigma: ", x$lsig,
        "\n\t upper_sigma: ", x$usig
    )
    cat("\n")
    cat("Nb of forecasts: ", x$nfcasts, "\n", sep = "")
    cat("Nb of backcasts: ", x$nbcasts, "\n", sep = "")
    cat("Calendar sigma: ", x$sigma, "\n", sep = "")

    return(invisible(x))
}

#' @export
print.JD3_X13_SPEC <- function(x, ...) {
    print(x$regarima)

    cat("\n")

    print(x$x11)

    cat("\n", "Benchmarking", "\n", sep = "")

    if (x$benchmarking$enabled) {
        cat("Enabled: Yes\n", sep = "")
        cat("Target: ", x$benchmarking$target,
            ifelse(
                test = x$benchmarking$target == "TARGET_CALENDARADJUSTED",
                yes = " (Auto)",
                no = ""
            ),
            "\n", sep = "")
        cat("Lambda: ", x$benchmarking$lambda,
            ifelse(test = x$benchmarking$lambda == 1, yes = " (Auto)", no = ""),
            "\n", sep = "")
        cat("Rho: ", x$benchmarking$rho,
            ifelse(test = x$benchmarking$rho == 1, yes = " (Auto)", no = ""),
            "\n", sep = "")
        cat("Bias: ", x$benchmarking$bias,
            ifelse(test = x$benchmarking$bias == "BIAS_NONE", yes = " (Auto)", no = ""),
            "\n", sep = "")
        cat("Use forecast: ",
            ifelse(test = x$benchmarking$forecast, yes = "Yes", no = "No (Auto)"),
            "\n", sep = "")
    } else {
        cat("Is enabled: No\n")
    }

    return(invisible(x))
}
