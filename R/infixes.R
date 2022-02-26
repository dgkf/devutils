#' If not null else
#'
#' Return the left-hand-side if it is not `NULL`, otherwise return the result of
#' the right-hand-side.
#'
#' @family infix
#' @export
#'
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}



#' If not error else
#'
#' Return the result of the left-hand-side if it does not produce an error,
#' otherwise return the result of the right-hand-side.
#'
#' @family infix
#' @export
#'
`%?%` <- function(lhs, rhs) {
  tryCatch(lhs, error = function(e) rhs)
}



#' Decorator
#'
#' Decorates a function with a wrapping "decorator" function
#'
#' @family infix
#' @family decorators
#' @export
#'
`%@%<-` <- function(dec, name, value) {
  name <- substitute(name)
  assign(as.character(name), dec(value), envir = parent.frame())
  dec
}



#' Decorator
#'
`%@%` <- function(dec, fn) {
  decexpr <- substitute(dec)
  if (is.name(decexpr)) return(dec(fn))

  decexpr <- as.call(append(
    as.list(decexpr),
    substitute(fn),
    after = 1L
  ))

  eval(decexpr, envir = parent.frame())
}
