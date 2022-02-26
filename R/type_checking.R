missing_value <- function(x) {
  formals()[[1L]]
}


is_not_missing_or <- function(name, or, envir = parent.frame()) {
  val <- mget(as.character(name), envir)[[1L]]
  if (missing(val)) force(or)
  val
}

split_arg_type_checking <- function(expr, depth = 0L) {
  arg <- list(value = list(), type = list())

  if (length(expr) == 1L && as.character(expr) == "") return(arg)

  if (length(expr) == 1L) {
    arg$value <- expr
  } else if (length(expr) > 1L && expr[[1L]] == quote(`:`) &&
    !(is.atomic(expr[[3L]]) && class(expr[[3L]]) == "numeric")) {
    arg$value <- expr[[2L]]
    arg$type <- expr[[3L]]
  } else {
    expr <- as.list(expr)
    for (i in rev(seq_along(expr))) {
      s <- split_arg_type_checking(expr[[i]], depth + 1L)
      if (length(s$value) > 1L) s$value <- as.call(s$value)
      if (length(s$type) > 0L) {
        if (i == length(expr)) {
          arg$value <- append(head(expr, i - 1L), s$value)
          arg$type  <- s$type
        } else {
          arg$value <- append(head(expr, i - 1L)[-1L], s$value)
          arg$type  <- c(expr[1L], s$type, tail(expr, -i))
        }
        break
      } else if (i == length(expr)) {
        arg$value <- expr
      }
    }
  }

  if (depth == 0L && length(arg$value) > 1L) {
    arg$value <- as.call(arg$value)
  } else if (length(arg$value) == 1L && arg$value == quote(`.`)) {
    arg$value <- list()
  }

  if (depth == 0L && length(arg$type) > 1L) {
    arg$type <- as.call(arg$type)
  }

  arg
}

split_fn_type_checking <- function(fn) {
  fn_formals <- formals(fn)
  lapply(fn_formals, split_arg_type_checking)
}

constraint_type_defs <- function(exprs) {
  which_def <- vapply(exprs, is.name, logical(1L))
  as.character(exprs[which_def])
}

build_constraint_fns <- function(exprs) {
  which_def <- vapply(exprs, is.name, logical(1L))
  type_defs <- as.character(exprs[which_def])

  fns_all <- exprs[which_def]
  names(fns_all) <- type_defs
  fns_all[] <- list(function(vals) {
    all(vapply(vals[-1], identical, logical(1L), vals[[1L]]))
  })

  fns_each <- list()
  for (i in seq_along(exprs)[!which_def]) {
    expr <- exprs[[i]]

    expr_fn <- function() { }
    expr_fn_formals <- intersect(all.names(expr), type_defs)
    names(expr_fn_formals) <- expr_fn_formals
    expr_fn_formals[] <- list(NULL)

    formals(expr_fn) <- expr_fn_formals
    body(expr_fn) <- expr

    fns_each[[length(fns_each)+1L]] <- expr_fn
  }

  list(constrain_all = fns_all, constrain_each = fns_each)
}

typed <- function(fn, ...) {
  constraints <- substitute(...())
  arg_data <- split_fn_type_checking(fn)

  type_check_args <- Filter(length, lapply(arg_data, `[[`, "type"))

  type_check_call <- do.call("call", quote = TRUE, c(
    "type_check",
    list(constraint_type_defs(constraints)),
    type_check_args
  ))

  type_constrain_call <- as.call(append(
    as.list(call("type_constrain", type_check_call)),
    constraints
  ))

  # strip out typing information from header
  formals(fn) <- lapply(arg_data, function(i) {
    if (length(i$value)) i$value else missing_value()
  })

  # insert type checking statements
  if (body(fn)[[1L]] == quote(`{`)) {
    body(fn) <- as.call(append(
      as.list(body(fn)),
      after = 1L,
      list(type_constrain_call)
    ))
  } else {
    body(fn) <- as.call(list(quote(`{`), type_check_call, body(fn)))
  }

  fn
}

type_check <- function(type_defs, ...) {
  args <- substitute(...())
  envir <- parent.frame()

  # initialize type constraints accumulator
  const <- new.env(parent = emptyenv())
  for (t in type_defs) const[[t]] <- list()

  for (i in seq_along(args)) {
    signature <- args[[i]]
    name <- names(args)[[i]]
    val <- is_not_missing_or(name, next, envir)
    if (!type_check_against(signature, name, val, envir, const)) {
      throw_type_check_error(signature, name)
    }
  }

  as.list(const)
}

type_constrain <- function(const, ...) {
  constraint_fns <- build_constraint_fns(substitute(...()))

  # apply identity type constraints
  for (type_var in names(constraint_fns$constrain_all)) {
    constraint_fn <- constraint_fns$constrain_all[[type_var]]
    if (!constraint_fn(const[[type_var]])) {
      constraint <- as.name(type_var)
      throw_type_bounds_error(constraint)
    }
  }

  # apply functional type constraints
  for (constraint_fn in constraint_fns$constrain_each) {
    type_vars <- names(formals(constraint_fn))
    names(type_vars) <- type_vars
    type_vars[] <- lapply(const[names(type_vars)], unlist)
    type_val_grid <- do.call(expand.grid, type_vars)

    # apply type constraint to all applicable combinations of trait values
    type_val_grid_satisfied <- apply(type_val_grid, 1L, function(vals) {
      do.call(constraint_fn, as.list(vals))
    })

    if (!all(type_val_grid_satisfied)) {
      constraint <- body(constraint_fn)
      throw_type_bounds_error(constraint)
    }
  }
}

type_check_against <- function(pattern, name, val, envir, const) {
  UseMethod("type_check_against")
}

throw_type_check_error <- function(signature, name) {
  stop(sprintf(
    "Type of parameter '%s' does not match signature `%s`",
    name,
    deparse(signature)
  ))
}

throw_type_bounds_error <- function(bounds) {
  stop(sprintf("Type constraint '%s' not satisfied", deparse(bounds)))
}

type_check_against.default <- function(pattern, name, val, envir, const) {
  throw_type_check_error(pattern, name)
}

type_check_against.character <- function(pattern, name, val, envir, const) {
  if (pattern %in% names(const)) {
    const[[pattern]][[name]] = list(class = class(val), mode = mode(val))
    return(TRUE)
  }

  mode(val) == pattern || inherits(val, pattern)
}

type_check_against.name <- function(pattern, name, val, envir, const) {
  pattern_chr <- as.character(pattern)
  pattern_val <- mget(pattern_chr, envir, inherits = TRUE)[[1L]]
  if (missing(pattern_val)) {
    return(type_check_against(pattern_chr, name, val, envir, const))
  }

  res <- tryCatch(
    do.call(pattern, list(val), envir = envir),
    error = function(e) NULL
  )

  if (is.logical(res)) {
    return(res)
  }

  type_check_against(pattern_chr, name, val, envir, const)
}

type_check_against.function <- function(pattern, name, val, envir, const) {
  pattern(val)
}

type_check_against.call <- function(pattern, name, val, envir, const) {
  if (pattern[[1L]] == "function") {
    fn <- eval(pattern, envir = envir)
    return(type_check_against(fn, name, val, envir, const))
  } else if (pattern[[1L]] == quote(`|`)) {
    union_checks <- vapply(pattern[-1L], type_check_against, logical(1L),
      name, val, envir, const)
    return(any(union_checks))
  }

  if (!type_check_against(pattern[[1L]], name, val, envir, const))
    return(FALSE)

  for (trait_i in seq_along(pattern)[-1L]) {
    trait_fn <- names(pattern)[[trait_i]]
    trait_bound <- pattern[[trait_i]]
    trait_actual <- do.call(trait_fn, list(val), envir = envir)
    if (is.atomic(trait_bound)) {
      if (trait_actual != trait_bound) return(FALSE)
    } else {
      const[[as.character(trait_bound)]][[name]] <- trait_actual
    }
  }

  TRUE
}


x <-
typed(T, N, N == 3) %@%
function(
  a,
  b = . :character|numeric,
  c = 10L,
  d = 10L * 3,
  e = paste0("a", "b") :character,
  f = 1:3 :T(length=N),
  g = 1:3 :T(length=N),
  h = list(1, 2, 3) :list
) {
  print(c)
}
