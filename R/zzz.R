#' @include utils.R
#' @importFrom rjd3jars check_java_version reload_dictionaries
#' @importFrom stats is.ts start
#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage
NULL



.onLoad <- function(libname, pkgname) {
    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)

    if (rjd3jars::check_java_version(FALSE)) {
        rjd3jars::reload_dictionaries()
    }

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    assign("x13", list(), rjd3toolkit::.jd3_env)

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
    if (is.null(getOption("thresholds_pval"))) {
        options(thresholds_pval = c(Severe = 0.001, Bad = 0.01, Uncertain = 0.05, Good = Inf))
    }
}

#' Set an option for x13
#'
#' @param name Name of the option
#' @param obj Option
#'
#' @export
#'
#' @examples
#' x13_option("test", "DUMMY")
x13_option<-function(name, obj){
    options<-rjd3toolkit::.jd3_env$x13
    options[[name]]<-obj
    assign("x13", options, rjd3toolkit::.jd3_env)
    invisible()
}

#' Set an option for x13
#'
#' @param name Name of the option
#'
#' @returns The requested option or NULL if it doesn't exist
#' @export
#'
#' @examples
#' x13_option("test", "DUMMY")
#' get_x13_option("test")
get_x13_option<-function(name){
    options<-rjd3toolkit::.jd3_env$x13
    return (options[[name]])
}
