#' @include utils.R
NULL

#' Outlier Detection with a RegARIMA Model
#'
#' @param y the dependent variable (a `ts` object).
#' @param order,seasonal the orders of the ARIMA model.
#' @param mean Boolean to include or not the mean.
#' @param X user defined regressors (other than calendar).
#' @param X.td calendar regressors.
#' @param ao,ls,so,tc Boolean to indicate which type of outliers should be
#' detected.
#' @param cv  `numeric`. The entered critical value for the outlier detection
#' procedure. If equal to 0 the critical value for the outlier detection
#' procedure is automatically determined by the number of observations.
#' @param clean Clean missing values at the beginning/end of the series.
#' Regression variables are automatically resized, if need be.
#'
#' @return a `"JD3_REGARIMA_OUTLIERS"` object, containing input variables and results
#'
#' @examples
#' regarima_outliers(rjd3toolkit::ABS$X0.2.09.10.M)
#'
#' @export
regarima_outliers <- function(y,
                              order = c(0L, 1L, 1L),
                              seasonal = c(0L, 1L, 1L),
                              mean = FALSE,
                              X = NULL,
                              X.td = NULL,
                              ao = TRUE,
                              ls = TRUE,
                              tc = FALSE,
                              so = FALSE,
                              cv = 0,
                              clean = FALSE) {
    if (!is.ts(y)) {
        stop("y must be a time series")
    }
    if (!is.null(X.td)) {
        td <- rjd3toolkit::td(s = y, groups = X.td)
        X <- cbind(X, td)
    }


    jregarima <- .jcall(
        "jdplus/x13/base/r/RegArimaOutliersDetection",
        "Ljdplus/x13/base/r/RegArimaOutliersDetection$Results;", "process",
        rjd3toolkit::.r2jd_tsdata(y), as.integer(order), as.integer(seasonal),
        mean, rjd3toolkit::.r2jd_matrix(X),
        ao, ls, tc, so, cv, clean
    )
    model <- list(
        y = rjd3toolkit::.proc_ts(jregarima, "y"),
        variables = rjd3toolkit::.proc_vector(jregarima, "variables"),
        X = rjd3toolkit::.proc_matrix(jregarima, "regressors"),
        b = rjd3toolkit::.proc_vector(jregarima, "b"),
        bcov = rjd3toolkit::.proc_matrix(jregarima, "bvar"),
        linearized = rjd3toolkit::.proc_vector(jregarima, "linearized")
    )

    ll0 <- rjd3toolkit::.proc_likelihood(jregarima, "initiallikelihood.")
    ll1 <- rjd3toolkit::.proc_likelihood(jregarima, "finallikelihood.")

    return(structure(
        list(
            model = model,
            likelihood = list(initial = ll0, final = ll1)
        ),
        class = "JD3_REGARIMA_OUTLIERS"
    ))
}
