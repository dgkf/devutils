`::` <- function(pkg, name) {
  env <- parent.frame()
  pkgsym <- substitute(pkg)
  namesym <- substitute(name)
  if (exists(pkgsym, envir = env)) {
    double_colon(pkg, as.character(namesym))
  } else {
    do.call(getExportedValue("base", "::"), list(pkgsym, namesym))
  }
}

#' @export
double_colon <- function(pkg, name) UseMethod("double_colon")

#' @export
double_colon.default <- getExportedValue("base", "::")


`:::` <- function(pkg, name) {
  env <- parent.frame()
  pkgsym <- substitute(pkg)
  namesym <- substitute(name)
  if (exists(pkgsym, envir = env)) {
    triple_colon(pkg, as.character(namesym))
  } else {
    do.call(getExportedValue("base", ":::"), list(pkgsym, namesym))
  }
}

#' @export
triple_colon <- function(pkg, name) UseMethod("triple_colon")

#' @export
triple_colon.default <- getExportedValue("base", ":::")
