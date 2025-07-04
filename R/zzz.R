#' @importFrom RProtoBuf readProtoFiles2
#' @importFrom stats is.ts start
#' @include utils.R

#' @rdname jd3_utilities
#' @export
jversion <- NULL

.onAttach <- function(libname, pkgname) {
    # what's your java  version?  Need >= 17
    if (jversion < 17) {
        packageStartupMessage(sprintf("Your java version is %s. 17 or higher is needed.", jversion))
    }
}

.onLoad <- function(libname, pkgname) {
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) stop("Loading rjd3 libraries failed", call. = FALSE)

    result <- .jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)

    proto.dir <- system.file("proto", package = pkgname)
    readProtoFiles2(protoPath = proto.dir)

    jversion <<- .jcall("java.lang.System", "S", "getProperty", "java.version")
    jversion <<- as.integer(regmatches(jversion, regexpr(pattern = "^(\\d+)", text = jversion)))

    # reload extractors
    rjd3toolkit::reload_dictionaries()
    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
    if (is.null(getOption("thresholds_pval"))) {
        options(thresholds_pval = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf))
    }
}
