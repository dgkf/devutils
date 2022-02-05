#' Import a package as a Suggests dependency
#'
#' Creates an object in the current environment with the same name as the
#' package, which can be used just like a package to access its namespace,
#' throwing an error if the package is not installed.
#'
#' @param pkgname A \code{character} package name to import as a "Suggests"
#'   dependency
#' @param pkgversion A \code{character} vector of package version
#'   specifications, as they would be specified in \code{DESCRIPTION} (for
#'   example, \code{">= 1.2.3"}).
#' @param unavailable_callback A \code{function} to be called when the suggested
#'   package is meant to be used, but unavailable.
#' @param env A \code{environment} in which the suggested package object should
#'   be assigned. By default, an object of the same name as the package is
#'   created in the parent environment.
#'
#' @export
suggested <- function(pkgname, pkgversion = "",
  unavailable_callback = suggested_callback_error,
  env = parent.frame()) {

  suggested_pkg_obj <- structure(
    new.env(parent = emptyenv()),
    pkg = pkgname,
    ver = pkgversion,
    callback = unavailable_callback,
    class = "devutils_suggested_package"
  )

  if (permits_mutation(env)) {
    assign(pkgname, suggested_pkg_obj, env)
  }

  invisible(suggested_pkg_obj)
}

#' @export
available <- function(pkg, env = parent.frame()) {
  pkgname <- as.character(substitute(pkg))
  if (exists(pkgname, env) && inherits(pkg, "devutils_suggested_package")) {
    return(suggested_loaded(pkg))
  } else {
    return(!inherits(try(find.package(pkgname), silent = TRUE), "try-error"))
  }
}

suggested_loaded <- function(pkg) {
  ns <- get0(".__NAMESPACE__.", pkg, inherits = FALSE, ifnotfound = list())
  !is.null(ns$path)
}

suggested_load_attempted <- function(pkg) {
  exists(".__NAMESPACE__.", pkg, inherits = FALSE)
}


#' @export
`$.devutils_suggested_package` <- function(x, name) {
  `[[`(x, as.character(name))
}

#' @export
`[[.devutils_suggested_package` <- function(x, name, ...) {
  # if namespace is loaded, return value
  if (name %in% names(x) && suggested_load_attempted(x))
    return(get(name, envir = x, inherits = FALSE))

  pkg <- attr(x, "pkg")
  callback <- attr(x, "callback")

  # terminate if namespace is loaded, but the function does not exist
  if (length(x) && suggested_loaded(x))
    stop(sprintf("Suggested package '%s' does not export '%s'", pkg, name))

  # if not loaded, check if required package is available
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!suggested_load_attempted(x)) {
      assign(".__NAMESPACE__.", NULL, x)
      return(x[[name]])
    }

    callback(pkg)
  }

  ns <- loadNamespace(pkg)
  for (n in names(ns)) x[[n]] <- ns[[n]]
  x[[name]]
}

#' @export
double_colon.devutils_suggested_package <- `[[.devutils_suggested_package`

#' @export
triple_colon.devutils_suggested_package <- `[[.devutils_suggested_package`



#' @export
suggested_fallback <- function(x, ...) {
  xcall <- substitute(x)
  if (inherits(xcall, "call") &&
      (xcall[[1L]] == "::" || xcall[[1L]] == "$" || xcall[[1L]] == "[[")) {
    pkgname <- as.character(xcall[[2L]])
    pkgobj  <- as.character(xcall[[3L]])
    val <- ..1
  } else {
    pkgname <- x
    pkgobj <- ..1
    val <- ..2
  }

  env <- get0(pkgname, parent.frame())
  assign(pkgobj, val, env)
}
