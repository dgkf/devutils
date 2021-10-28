#' @export
roxygenize_pkgoptions <- function(
  title = sprintf("%s Options", pkgname(env)),
  desc, env = parent.frame()) {

  if (missing(desc)) {
    desc <- paste0(
      "Internally used, package-specific options. All options will prioritize ",
      "R options() values, and fall back to environment variables if ",
      "undefined. If neither the option nor the environment variable is set, ",
      "a default value is used."
    )
  }

  pkgoptdeets <- "..pkgoption_details.."
  optenv <- get_pkgoptions(env)
  details <- optenv[[pkgoptdeets]]

  c(
    sprintf("@title %s", title),
    sprintf("@description %s", desc),
    sprintf("@name %s_options", pkgname(env)),
    sprintf("@usage options(\"%1$s.<option>\")", pkgname(env)),
    sprintf("@usage options(\"%1$s.<option>\" = <value>)", pkgname(env)),
    "@section Options:",
    "\\describe{",
    vapply(setdiff(names(optenv), pkgoptdeets), function(n) {
      sprintf("\\item{%s}{\\describe{%s}}", n,
        paste0(sep = "\n",
          details[[n]]$desc,
          sprintf("\\item{default: }{\\preformatted{%s}}",
            paste0(collapse = "\n",
              deparse(eval(bquote(substitute(.(as.symbol(n)), optenv)))))),
          sprintf("\\item{option: }{%s}", details[[n]]$option_name),
          sprintf("\\item{envvar: }{%s (%s)}",
            details[[n]]$envvar_name,
            attr(details[[n]]$envvar_fn, "desc") %||% "preprocessed"
          )
        )
      )
    }, character(1L)),
    "}",
    "@docType pkgoptions"
  )
}

#' @export
roxygenize_pkgoptions_params <- function(which = names(env),
  env = parent.frame()) {

  pkgoptdeets <- "..pkgoption_details.."
  optenv <- get_pkgoptions(env)
  details <- optenv[[pkgoptdeets]]

  format_param <- function(n) {
    default <- paste0(
      deparse(eval(bquote(substitute(.(as.symbol(n)), optenv)))),
      collapse = "; "
    )

    sprintf(
      paste0(
        "@param %s %s (Defaults to `%s`, overwritable using option '%s' or ",
        "environment variable '%s')"),
      n,
      details[[n]]$desc,
      default,
      details[[n]]$option_name,
      details[[n]]$envvar_name
    )
  }

  c(
    "@title Generated Package Option Parameters",
    vapply(setdiff(which, pkgoptdeets), format_param, character(1L))
  )
}
