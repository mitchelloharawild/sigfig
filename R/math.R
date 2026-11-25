#' @export
Math.sigfig <- function(x, ...) {
  vec_math(.Generic, x, ...)
}

#' @export
Summary.sigfig <- function(..., na.rm = FALSE) {
  dots <- list(...)
  x <- if (length(dots) == 1L) dots[[1L]] else vec_c(...)
  vec_math(.Generic, x, na.rm = na.rm)
}

#' @export
vec_math.sigfig <- function(.fn, .x, ...) {
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
      cli_abort("{.fn {(.fn)}} is not supported for {.cls sigfig} vectors.", call = NULL)
    },
    {
      fn <- getExportedValue("base", .fn)
      new_sigfig(fn(field_x(.x), ...), op_sigfig(.fn, .x))
    }
  )
}

drop_missing <- function(x, na.rm) {
  if (na.rm && anyNA(x)) x[!is.na(x)] else x
}

math_summary <- function(.fn, .x, ..., na.rm = FALSE) {
  fn <- getExportedValue("base", .fn)
  .x <- drop_missing(.x, na.rm)
  new_sigfig(fn(field_x(.x), ...), op_sigfig(.fn, .x))
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
  new_sigfig(round(field_x(x), digits), op_sigfig("round", x, digits))
}

math_signif <- function(x, digits = 6, ...) {
  digits <- check_rounding(x, digits)
  new_sigfig(signif(field_x(x), digits), op_sigfig("signif", x, digits))
}

#' @export
mean.sigfig <- function(x, ..., na.rm = FALSE) {
  vec_math("mean", x, ..., na.rm = na.rm)
}

#' @export
median.sigfig <- function(x, na.rm = FALSE, ...) {
  x <- drop_missing(x, na.rm)
  new_sigfig(as.double(median(field_x(x))), op_sigfig("median", x))
}

#' @export
quantile.sigfig <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...) {
  x <- drop_missing(x, na.rm)
  args <- list(...)
  # precision vectors can't have names
  args$names <- NULL
  out <- do.call(quantile, c(list(field_x(x), probs, names = FALSE), args))
  new_sigfig(as.double(out), op_sigfig("quantile", x, probs))
}
