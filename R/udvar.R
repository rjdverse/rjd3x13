.add_ud_var <- function(x, jx, userdefined = NULL, out_class = NULL, result = FALSE){
  if (is.null(userdefined)) {
    x$user_defined = rjd3toolkit::user_defined(x, NULL)
  } else {
    if (result) {
      res = jx
    } else {
      if (is.null(out_class)) {
        res = jx$getResult()
      } else {
        res = .jcall(jx, out_class, "getResult")
      }
    }
    res = rjd3toolkit::.jd3_object(res, result = TRUE)
    x$user_defined = rjd3toolkit::user_defined(res, userdefined = userdefined)
  }
  x
}

