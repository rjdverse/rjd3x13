#' @include utils.R
NULL

#' @importFrom rjd3toolkit get_java_version minimal_java_version
.onAttach <- function(libname, pkgname) {
    current_java_version <- rjd3toolkit::get_java_version()
    if (current_java_version < rjd3toolkit::minimal_java_version) {
        packageStartupMessage(sprintf("Your java version is %s. %s or higher is needed.",
                                      current_java_version, rjd3toolkit::minimal_java_version))
    }
}

#' @importFrom stats is.ts start
#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull is.jnull .jfield .jaddClassPath
#' @importFrom rjd3toolkit get_java_version minimal_java_version
.onLoad <- function(libname, pkgname) {
    jar_dir <- file.path(libname, pkgname, "inst", "java")
    jars <- list.files(jar_dir, pattern = "\\.jar$", full.names = TRUE, all.files = TRUE)
    rJava::.jaddClassPath(jars)
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
