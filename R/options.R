#' @title Set an option for x13
#'
#' @param name Name of the option
#' @param obj Option
#'
#' @returns Invisibly `NULL`
#'
#' @export
#'
#' @examples
#' x13_option("test", "DUMMY")
x13_option <- function(name, obj) {
    options_x13 <- rjd3toolkit::.jd3_env$x13
    options_x13[[name]] <- obj
    assign("x13", options_x13, rjd3toolkit::.jd3_env)
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
    options_x13 <- rjd3toolkit::.jd3_env$x13
    return(options_x13[[name]])
}
