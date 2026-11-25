#' @export
Math.precision <- function(x, ...) {
  vec_math(.Generic, x, ...)
}

#' @export
Summary.precision <- function(..., na.rm = FALSE) {
  dots <- list(...)
  x <- if (length(dots) == 1L) dots[[1L]] else vec_c(...)
  vec_math(.Generic, x, na.rm = na.rm)
}

#' @export
vec_math.precision <- function(.fn, .x, ...) {
  switch(
    .fn,
    sum = , prod = , mean = , min = , max = , range = {
      math_summary(.fn, .x, ...)
    },
    is.nan = , is.finite = , is.infinite = {
      getExportedValue("base", .fn)(field_x(.x))
    },
    round = math_round(.x, ...),
    signif = math_signif(.x, ...),
    any = , all = {
      cli_abort("{.fn {(.fn)}} is not supported for {.cls precision} vectors.", call = NULL)
    },
    {
      fn <- getExportedValue("base", .fn)
      new_precision(fn(field_x(.x), ...), op_precision(.fn, .x))
    }
  )
}

drop_missing <- function(x, na.rm) {
  if (na.rm && anyNA(x)) x[!is.na(x)] else x
}

math_summary <- function(.fn, .x, ..., na.rm = FALSE) {
  fn <- getExportedValue("base", .fn)
  .x <- drop_missing(.x, na.rm)
  new_precision(fn(field_x(.x), ...), op_precision(.fn, .x))
}

# Digits must be the size of x or 1, like arithmetic.
check_rounding <- function(x, digits, call = caller_env()) {
  digits <- as_bare_double(digits, "digits", call = call)
  if (anyNA(digits)) cli_abort("{.arg digits} can't be missing.", call = call)
  vec_recycle(digits, length(x), x_arg = "digits", call = call)
  digits
}

math_round <- function(x, digits = 0, ...) {
  digits <- check_rounding(x, digits)
  new_precision(round(field_x(x), digits), op_precision("round", x, digits))
}

math_signif <- function(x, digits = 6, ...) {
  digits <- check_rounding(x, digits)
  new_precision(signif(field_x(x), digits), op_precision("signif", x, digits))
}

#' @export
mean.precision <- function(x, ..., na.rm = FALSE) {
  vec_math("mean", x, ..., na.rm = na.rm)
}

#' @export
median.precision <- function(x, na.rm = FALSE, ...) {
  x <- drop_missing(x, na.rm)
  new_precision(as.double(median(field_x(x))), op_precision("median", x))
}

#' @export
quantile.precision <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...) {
  x <- drop_missing(x, na.rm)
  args <- list(...)
  # precision vectors can't have names
  args$names <- NULL
  out <- do.call(quantile, c(list(field_x(x), probs, names = FALSE), args))
  new_precision(as.double(out), op_precision("quantile", x, probs))
}
