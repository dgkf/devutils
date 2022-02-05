#' If not null else
#'
#' Return the left-hand-side if it is not `NULL`, otherwise return the result of
#' the right-hand-side.
#'
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
#' @export
#'
`%?%` <- function(lhs, rhs) {
  tryCatch(lhs, error = function(e) rhs)
}
