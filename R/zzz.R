#' @importFrom RProtoBuf readProtoFiles2
#' @importFrom stats is.ts start
#' @include utils.R

.onAttach <- function(libname, pkgname) {
    current_java_version <- rjd3toolkit::get_java_version()
    if (current_java_version < rjd3toolkit::minimal_java_version) {
        packageStartupMessage(sprintf("Your java version is %s. %s or higher is needed.",
                                      current_java_version, rjd3toolkit::minimal_java_version))
    }
}

#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull is.jnull .jfield
.onLoad <- function(libname, pkgname) {
    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)

    current_java_version <- rjd3toolkit::get_java_version()
    if (current_java_version >= rjd3toolkit::minimal_java_version) {
        rjd3toolkit::reload_dictionaries()
    }

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
    if (is.null(getOption("thresholds_pval"))) {
        options(thresholds_pval = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf))
    }
}
