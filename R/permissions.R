#' Choose how extensively devutils affects a package namespace
#'
#' `devutils` will invariably inject some objects into the package namespace.
#' However, the extent to which this happens may be undesirable. Unless new
#' privileges are granted, `devutils` will aim to minimally affect your
#' namespace.
#'
#' @section Permission Tiers:
#' Each level of permissioning grants additional permissions to modify your
#' package namespace. From least to most extensive, permissions are granted as
#' one of
#'
#' \describe{
#'   \item{"none"}{
#'     Minimalist permissions, and the default behavior.
#'     \itemize{
#'       \item{Creates a \code{.devutils} environment in your package namespace
#'             wherein settings and data are stored}
#'     }
#'   }
#'   \item{"some"}{
#'     Moderate permissions, modifying your namespace, but only as a side effect
#'     of direct use of `devutils` functions.
#'     \itemize{
#'       \item{using `suggested` creates an object of the package's name in the
#'             package environment, which can be used with `$` or `[[` indexing}
#'     }
#'   }
#'   \item{"all"}{
#'     Full permissions, additionally masking some base functions in your
#'     package namespace.
#'     \itemize{
#'       \item{Masks base `::` and `:::` to automatically handle suggested
#'       packages, throwing errors when unavailable suggested package functions
#'       are called}
#'     }
#'   }
#' }
#'
#' @examples
#' \dontrun{permit_mutation("some")
#' utils <- suggested("utils")
#' utils$head(1:10)
#'
#' permit_mutation("all")
#' suggested("utils")
#' utils::head(1:10)
#'
#' permit_mutation("all")
#' suggested("not.a.real.package")
#' not.a.real.package::head(1:10)
#' # Error: This feature is unavailable because package 'not.a.real.package'
#' #   is not installed.
#' }
#'
#' @export
permit_mutation <- function(which = c("all", "some", "none"),
  env = parent.frame()) {

  which <- match.arg(which, c("all", "some", "none"), several.ok = FALSE)
  switch(which,
    all = permit_mutation_all(env),
    some = permit_mutation_some(env),
    none = permit_mutation_none(env)
  )
}

get_devutils <- function(env = parent.frame(2L)) {
  if (!exists(".devutils", env)) {
    devutilsenv <- new.env(parent = emptyenv())
    assign(".devutils", devutilsenv, env)
    permit_mutation_none(env)
  }

  get(".devutils", env, inherit = FALSE)
}


permit_mutation_all <- function(env = parent.frame()) {
  devutilsenv <- get_devutils(env)
  devutilsenv$permits <- "all"

  # inject masked `::` and `:::` operators
  assign("::", `::`, env)
  assign(":::", `::`, env)
  invisible(devutilsenv)
}

permit_mutation_some <- function(env = parent.frame()) {
  devutilsenv <- get_devutils(env)
  devutilsenv$permits <- "some"
  invisible(devutilsenv)
}

permit_mutation_none <- function(env = parent.frame()) {
  devutilsenv <- get_devutils(env)
  devutilsenv$permits <- "none"
  invisible(devutilsenv)
}

permits_mutation <- function(env = parent.frame()) {
  devutilsenv <- get_devutils(env)
  match(devutilsenv$permits, c("none", "some", "all")) >= 2L
}
