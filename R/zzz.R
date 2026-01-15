#' @importFrom RProtoBuf readProtoFiles2
#' @importFrom stats is.ts start
#' @include utils.R

#' @title Java version.
#'
#' @returns \code{current_java_version} is the current Java version and \code{minimal_java_version} is the minimum accepted Java version.
#'
#' @importFrom rjd3jars get_java_version minimal_java_version
#' @export
#' @name java_version
#'
#' @examples
#' print(minimal_java_version)
#' print(current_java_version)
#' @export
current_java_version <- rjd3jars::get_java_version()

#' @rdname java_version
#' @export
minimal_java_version <- rjd3jars::minimal_java_version

.onAttach <- function(libname, pkgname) {
    if (current_java_version < minimal_java_version) {
        packageStartupMessage(sprintf("Your java version is %s. %s or higher is needed.",
                                      current_java_version, minimal_java_version))
    }

    # reload extractors
    rjd3toolkit::reload_dictionaries()
}

#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull is.jnull .jfield
.onLoad <- function(libname, pkgname) {
    if (!requireNamespace("rjd3jars", quietly = TRUE)) stop("Loading rjd3 libraries failed", call. = FALSE)
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) stop("Loading rjd3 libraries failed", call. = FALSE)

    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
    if (is.null(getOption("thresholds_pval"))) {
        options(thresholds_pval = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf))
    }
}
