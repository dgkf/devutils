#' Callbacks To Handle Missing Suggests Packages
#'
#' These callbacks can be provided to \code{suggested} when declaring a
#' suggested dependency. If a Suggested package namespace is accessed, the
#' callback is used when handling the missing dependency. Generally, these are
#' mitigating or communicative steps to handle the missing package in its
#' entirety. By contrast, a package author may also define fallback behaviors
#' for specific namespace objects when the package is otherwise missing.
#'
#' @name suggested_callbacks
#' @rdname suggested_callbacks
NULL


#'
#' @section Callbacks:
#' \describe{
#'   \item{\code{suggested_callback_error}}{
#'     Throw an error if a suggested package is not installed.
#'   }
#' }
#'
#' @export
#' @rdname suggested_callbacks
suggested_callback_error <- function(pkgname, ...) {
  stop(sprintf(
    "This feature is unavailable because package '%s' is not installed.",
    pkgname
  ))
}



#'
#' @section Callbacks:
#' \describe{
#'   \item{\code{suggested_callback_prompt}}{
#'     Prompt to install a suggested package and resume if installed
#'   }
#' }
#'
#' @export
#' @rdname suggested_callbacks
suggested_callback_prompt <- function(pkgname, ...) {
  is_available <- pkgname %in% available.packages()[, "Package"]
  if (interactive() && is_available) {
    msg <- sprintf(paste0(
      "Suggested package '%s' is required for this functionality. ",
      "Would you like to install it now? (Y/n)"), pkgname)
    message(paste0(collapse = "\n", strwrap(indent = 2, msg)))
    res <- toupper(readLines(n = 1L)[[1L]])
    if (nchar(res) == 0L || startsWith(res, "Y"))
      install.packages(pkgname)
  } else {
    stop(sprintf(paste0(
      "Suggested package '%s' is required for this functionality, ",
      "but is unavailable."),
      pkgname
    ))
  }
}
