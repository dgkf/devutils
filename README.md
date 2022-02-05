# `devutils`

Utilities for common package development patterns

##### options

- Managing internal package options, defaulting to global options or environment
  variables
- Provides helpers for parsing environment variable strings into more convenient
  R objects.
- Automated package option documentation
- Provides helpers for creating a stub function with parameters corresponding to
  each option, such that parameter definitions can be easily inherited. Helpful
  for globally configurable behaviors that also are used to set frequent
  parameter defaults, such as `quiet` or `verbose`. 

##### suggests

- Transparent class for suggested packages
- Provide fallback functions for when a suggested package isn't available

##### common infix operators

- `%||%` "if-not-null-else" and `%?%` "if-not-error-else" infix operators

## Examples

### Defining options

Options are maintained in a hidden namespace variable called `.devutils`. By
calling the `define_pkgoption(s)` functions, you are updating that environment
to include definitions for your options, storing their defaults, descriptions,
and related option and environment variable names.

`define_pkgoptions` provides a convenient shorthand for defining many options
quickly, whereas `define_pkgoption` is a bit more rigid about its inputs, but
gives you more flexibility to customize the option behavior.

```r
define_pkgoptions(
  "This is an example of how a package author would document their internally",
  "used options. This option could make the package default to executing",
  "quietly."
  quiet = TRUE

  "Multiple options can be defined, providing default values if a global",
  "option or environment variable isn't set."
  second_example = FALSE

  "Default values are lazily evaluated, so you are free to use package",
  "functions without worrying about build-time evaluation order"
  lazy_example = fn_not_defined_until_later()
)

define_pkgoption(
  "concrete_example",
  desc = paste0(
    "Or, if you prefer a more concrete constructor you can define each option ",
    "explicitly."),
  option_name = "mypackage_concrete", # define custom option names
  envvar_name = "MYPACKAGE_CONCRETE", # and custom environment variable names
  envvar_fn = pkgoption_fn_is_true()  # and use helpers to handle envvar parsing
)
```

Once these are defined, you can use `pkgoption()` within your package to
retrieve values, implicitly sourcing values from global options, environment
variables or the provided default value.

### Creating option Rd documentation

As long as the options have been created as shown above, documenting your
options is as easy as adding this small roxygen stub within your package.

```r
#' @eval devutils::roxygenize_pkgoptions()
NULL
```

Produces `?mypackage_options`

```
mypackage Options

Description:

     Internally used, package-specific options. All options will
     prioritize R options() values, and fall back to environment
     variables if undefined. If neither the option nor the environment
     variable is set, a default value is used.

Options:

     quiet
          This is an example of how a package author would document their
          internally used options. This option could make the package default to
          executing quietly.

          default:

              TRUE

          option: mypackage.quiet

          envvar: R_MYPACKAGE_QUIET (raw)
...
```

### Reusing option documentation for parameters

In some cases, options are frequently used as default values for function
parameters. In these cases, you might find that you're repeating yourself trying
to document all the environment settings that might affect both the parameter
and option. Instead, you can create a function stub from which you can inherit
parameters in order to reuse the same option definitions as parameter
definitions.

```r
#' @eval devutils::roxygenize_pkgoption_params()
mypackage_option_params <- NULL

#' Example Function
#'
#' @inheritParams mypackage_option_params
#'
count_to_three <- function(quiet = pkgoption("quiet")) {
  for (i in 1:3) if (!quiet) cat(i, "\n")
}
```

### Throw error when suggested package is unavailable

By default, suggested packages are loaded as a new object. Functions from the
suggested package are accessed by `$` or `[[`.

```r
#' @import devutils
missingpackage <- suggested("missingpackage")

#' @export
count_to_three <- function() {
  missingpackage$do_thing()
}
```

```sh
R> library(mypackage)
R> count_to_three()
Error: This feature is unavailable because package 'missingpackage' is not installed.
```

> It can be inconvenient to manage the overhead of remembering which packages
> are suggested (using `$` and imported `::`). You can call
> `permit_mutation("all")` to give `devutils` permission to modify your package
> namespace, masking `::` and `:::` with versions that allow for suggested
> packages to be automatically handled.  
>
> Masking base operators, especially for something as foundational as package
> namespace interaction could be error-prone. For now this behavior is only for
> testing purposes.

### Define a fallback behavior when a suggested package is unavailable

Sometimes, a suggested package is convenient because it produces nicer output
than something you care to write yourself, but you can still hack something
sufficient together when the suggested package isn't available. In this
scenario, you can provide a fallback to that function. Errors will still be
raised if other functions are used, but the fallback will be used when only that
function is needed.

```r
#' @import devutils
missingpackage <- suggested("missingpackage")

suggested_fallback(
  missingpackage$do_thing,
  function() for(i in 1:3) cat(i, "\n")
)

#' @export
count_to_three <- function() {
  missingpackage$do_thing()
  missingpackage$do_other_thing()
}
```

```sh
R> library(mypackage)
R> count_to_three()
1
2
3
Error: This feature is unavailable because package 'missingpackage' is not installed.
```

### A drop-in replacement for dependencies (experimental)

If you're feeling adventurous, you can give `devutils` added privileges to mask
some base functions in your package namespace, allowing you to flag a package as
suggested while directly reuseing existing namespaced package objects (accessed via
`::` or `:::`).

This features is very experimental, and mucking with the base namespace is not
something that should be done carelessly. For now, this feature is only
recommended for testing.

```r
#' @import devutils
devutils::permit_mutation("all")

suggested("utils")  # modified in-place; now `utils` is a suggested package object

#' @export
count_to_three <- function() {
  for (i in utils::head(1:5, 3L)) cat(i, "\n")
}
```


