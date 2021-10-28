pkgname <- function(env = parent.frame(2L)) {
  eval(quote(packageName()), env)
}

get_pkgoptions <- function(env = parent.frame(2L)) {
  pkgutilsenv <- get_devutils(env)
  if (!exists("options", pkgutilsenv)) {
    optenv <- structure(
      new.env(parent = emptyenv()),
      class = "pkgutils_options_env"
    )

    assign("options", optenv, pkgutilsenv)
  }

  get("options", pkgutilsenv, inherit = FALSE)
}

new_pkgoption_details <- function(
  name,
  desc = NULL,
  option_name = paste(
    c(pkgname(), name),
    collapse = "."
  ),
  envvar_name = paste(
    gsub("[^A-Z0-9]", "_", toupper(c("R", pkgname(), name))),
    collapse = "_"
  ),
  envvar_fn = new_pkgoption_fn(identity, "raw")) {

  list(
    name = name,
    desc = desc,
    option_name = option_name,
    envvar_name = envvar_name,
    envvar_fn = envvar_fn
  )
}

#' @export
define_pkgoption <- function(name, default, ..., quoted = TRUE,
  eval.env = parent.frame()) {

  if (!quoted)
    default <- substitute(default)

  # add value in option environment
  optenv <- get_pkgoptions(env = eval.env)
  do.call(
    delayedAssign,
    list(name, default, eval.env, optenv)
  )

  # add option details to global ..pkgoption_details..
  pkgoptdeets <- "..pkgoption_details.."
  deets <- get0(pkgoptdeets, optenv, inherits = FALSE, ifnotfound = list())
  deets[[name]] <- new_pkgoption_details(name, ...)
  assign(pkgoptdeets, deets, optenv)

  invisible(optenv)
}

#' @export
define_pkgoptions <- function(...) {
  dots <- substitute(...())

  is_default <- names(dots) != ""
  if (length(dots) > 0L && !is_default[[length(dots)]])
    stop("Unnamed option descriptions should not follow the last option.")

  dots[!is_default] <- lapply(dots[!is_default], eval, envir = parent.frame())
  descs <- unname(lapply(
    split(dots[!is_default], cumsum(is_default)[!is_default]),
    paste,
    collapse = " "
  ))
  names(descs) <- names(dots[is_default])
  args <- dots[is_default]

  for (n in names(args)) {
    define_pkgoption(n, args[[n]], desc = descs[[n]], eval.env = parent.frame())
  }

  invisible(get_pkgoptions())
}

#' @export
pkgoption <- function(x, default = NULL, env = parent.frame()) {
  optenv <- get_pkgoptions(env)
  pkgoptdeets <- "..pkgoption_details.."
  deets <- get0(pkgoptdeets, optenv, inherits = FALSE, ifnotfound = list())

  unset <- "__DEVUTILS_OPT_UNSET__"
  env_val <- Sys.getenv(deets$envvar_name, unset = unset)
  opt_val <- getOption(deets$option_name, default = unset)

  if (env_val != unset) return((deets$envvar_fn %||% identity)(env_val))
  if (opt_val != unset) return(opt_val)

  opt_default_unset <- !(x %in% names(optenv))
  if (opt_default_unset) return(default)

  substitute_expr <- bquote(substitute(.(as.symbol(x)), .pkgutils_options))
  opt_default_unset <- nchar(eval(substitute_expr, parent.frame())) == 0L
  if (opt_default_unset) return(default)

  optenv[[x]]
}

#' @export
format.devutils_options_env <- function(x, ...) {
  pkgoptdeets <- "..pkgoption_details.."
  details <- x[[pkgoptdeets]]
  paste(collapse = "", lapply(setdiff(names(x), pkgoptdeets), function(n) {
    optdetails <- details[[n]]
    sprintf("%s:\n  description:\n%s\n  default:\n%s\n  option: '%s'\n  envvar: '%s' %s\n\n",
      n,
      paste0(collapse = "\n", "  | ",
        strwrap(optdetails$desc, width = getOption("width", 80L) - 4L)),
      paste0("  | ", deparse(eval(bquote(substitute(.(as.symbol(n)), x)))), collapse = "\n"),
      optdetails$option_name,
      optdetails$envvar_name,
      sprintf("(%s)", attr(optdetails$envvar_fn, "desc") %||% "preprocessed")
    )
  }))
}

#' @export
print.devutils_options_env <- function(x, ...) {
  cat(format(x, ...))
}

#' Utility functions for processing option values from strings
#'
#' @name pkgoption_fns
#' @rdname pkgoption_fns
NULL

new_pkgoption_fn <- function(f, desc) {
  attr(f, "desc") <- desc
  f
}

#' @export
#' @rdname pkgoption_fns
pkgoption_fn_parse <- function(...) {
  new_pkgoption_fn(
    function(raw) eval(parse(text = raw)),
    "as parsed and evaluated expression"
  )
}

#' @export
#' @rdname pkgoption_fns
pkgoption_fn_is_true <- function(...) {
  new_pkgoption_fn(
    function(raw) isTRUE(toupper(trimws(raw)) == "true"),
    "parsed as TRUE if 'true'"
  )
}

#' @export
#' @rdname pkgoption_fns
pkgoption_fn_is_false <- function(...) {
  new_pkgoption_fn(
    function(raw) isTRUE(toupper(trimws(raw)) == "false"),
    "parsed as FALSE if 'false'"
  )
}

#' @export
#' @rdname pkgoption_fns
pkgoption_fn_str_split <- function(delim = ";", ...) {
  new_pkgoption_fn(
    function(raw) trimws(strsplit(raw, ";")[[1L]]),
    sprintf("as character vector, split on '%s' delimiter", delim)
  )
}
