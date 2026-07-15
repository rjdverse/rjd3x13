#' @importFrom rjd3jars check_java_version
.onAttach <- function(libname, pkgname) {
    # Check java version
    rjd3jars::check_java_version(silent = FALSE, startup = TRUE)
}

#' @importFrom stats is.ts start coef df.residual logLik residuals vcov nobs
#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass .jfield
#' @importFrom rjd3jars check_java_version reload_dictionaries
.onLoad <- function(libname, pkgname) {
    # Loading dependencies
    if (!requireNamespace("rjd3jars", quietly = TRUE)) {
        stop("Loading {rjd3jars} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) {
        stop("Loading {rjd3toolkit} failed", call. = FALSE)
    }

    # Loading Java class
    jar_dir <- file.path(libname, pkgname, "inst", "java")
    jars_inst <- list.files(
        jar_dir,
        pattern = "\\.jar$",
        full.names = TRUE,
        all.files = TRUE
    )
    result <- rJava::.jpackage(
        pkgname,
        lib.loc = libname,
        morePaths = jars_inst
    )
    if (!result) {
        stop("Loading java packages failed")
    }

    # If java >= 21, then reload dictionnaries
    has_java <- rjd3jars::check_java_version(silent = TRUE)
    if (has_java) {
        rjd3jars::reload_dictionaries()
    }

    # Loading Proto class
    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    # Set options
    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
    if (is.null(getOption("thresholds_pval"))) {
        options(
            thresholds_pval = c(
                Severe = 0.001,
                Bad = 0.01,
                Uncertain = 0.05,
                Good = Inf
            )
        )
    }
}

#' @title Set an option for x13
#'
#' @param name Name of the option
#' @param obj Option
#'
#' @export
#'
#' @examples
#' x13_option("test", "DUMMY")
x13_option <- function(name, obj) {
    options <- rjd3toolkit::.jd3_env$x13
    options[[name]] <- obj
    assign("x13", options, rjd3toolkit::.jd3_env)
    return(invisible(NULL))
}

#' @title Set an option for x13
#'
#' @param name Name of the option
#'
#' @returns The requested option or NULL if it doesn't exist
#' @export
#'
#' @examples
#' x13_option("test", "DUMMY")
#' get_x13_option("test")
get_x13_option <- function(name) {
    options <- rjd3toolkit::.jd3_env$x13
    return(options[[name]])
}
