#' Arithmetic with known precision
#'
#' Results are calculated using the full values. The precision of the result is
#' the coarsest known precision of the inputs, so `3.2 + 4.12` is `7.3`. Ordinary
#' numbers are exact, and don't affect the precision of the result.
#'
#' @details
#' \describe{
#'   \item{Arithmetic}{`+`, `-`, `*`, `/`, `^`, `%%` and `%/%` use the coarsest
#'     known precision of each pair of elements. Inputs must be the same size, or
#'     size 1.}
#'   \item{Summaries}{`sum()`, `prod()`, `mean()`, `min()`, `max()`, `range()`,
#'     `median()` and `quantile()` use the coarsest known precision of all
#'     elements (excluding missing values if `na.rm = TRUE`).}
#'   \item{Cumulative}{`cumsum()`, `cumprod()`, `cummin()` and `cummax()` use the
#'     coarsest known precision of the elements so far.}
#'   \item{Rounding}{`round()` and `signif()` round the value, and the precision
#'     becomes no finer than the digits rounded to. `floor()`, `ceiling()` and
#'     `trunc()` similarly reduce known precision to no finer than 0, while 
#'     exact numbers remain exact. `sign()` is exact.}
#'   \item{Other functions}{`exp()`, `log()`, `sqrt()`, `abs()` and so on keep the
#'     precision of their input.}
#' }
#'
#' The result's precision is missing only if it is missing for all inputs.
#' Logical operators (`&`, `|`, `!`, `any()`, `all()`) are not supported, and
#' `is.na()`, `is.nan()`, `is.finite()` and `is.infinite()` give logical vectors
#' based only on the value.
#'
#' Base R finds methods for functions like `sum()` with their first argument, so
#' use `sum(x, 1)` instead of `sum(1, x)`.
#'
#' @name precision-arithmetic
#' @examples
#' precision(3.2, 1) + precision(4.12, 2)
#' precision(3.2, 1) * 2
#' sum(precision(c(1.234, 5.6), c(3, 1)))
#' cumsum(precision(c(1.234, 5.6, 7.89), c(3, 1, 2)))
#' round(precision(3.14159, 5), 2)
NULL

#' The precision of a result
#'
#' All operations obtain the precision of their result from here, so that the
#' rule can be changed. See _dev/arithmetic-precision.md for alternatives.
#'
#' @param op The name of the operation.
#' @param e1,e2 Operands: precision vectors, or plain numbers (which are exact).
#'   For `round()` and `signif()`, `e2` is the digits.
#' @param n The size of elementwise results.
#' @return An integer vector of precision, the size of the result.
#' @noRd
op_precision <- function(op, e1, e2 = NULL, n = length(e1)) {
  p1 <- if (is_precision(e1)) field_p(e1) else NA_integer_
  switch(
    op,
    # Reductions
    sum = , prod = , mean = , min = , max = , median = rle_coarsest(p1),
    range = rle_rep(rle_coarsest(p1), 2L),
    # `e2` is the probabilities
    quantile = rle_rep(rle_coarsest(p1), length(e2)),
    # Cumulative
    cumsum = , cumprod = , cummin = , cummax = rle_cumcoarsest(p1),
    # Rounding
    floor = , ceiling = , trunc = rle_cap(p1, 0L),
    round = rle_coarsest2(p1, as_rounding(e2), n),
    signif = rle_coarsest2(p1, signif_digits(field_x(e1), e2), n),
    sign = rle_rep(NA_integer_, n),
    # Elementwise
    {
      p2 <- if (is_precision(e2)) field_p(e2) else NA_integer_
      rle_coarsest2(p1, p2, n)
    }
  )
}

# Digits for round(), which need not be whole numbers
as_rounding <- function(digits) {
  digits <- floor(digits + 0.5)
  as.integer(pmax(pmin(digits, max_digits), -max_digits))
}

# Decimal places retained by signif(), from the rounded value as 9.96 becomes 10
signif_digits <- function(x, digits) {
  digits <- pmax(1, floor(digits + 0.5))
  mag <- floor(log10(abs(signif(x, digits))))
  mag[!is.finite(mag)] <- NA
  as_rounding(digits - 1 - mag)
}

# vec_arith() ------------------------------------------------------------------

#' @export
#' @method vec_arith precision
vec_arith.precision <- function(op, x, y, ...) {
  UseMethod("vec_arith.precision", y)
}

#' @export
#' @method vec_arith.precision default
vec_arith.precision.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.precision precision
vec_arith.precision.precision <- function(op, x, y, ...) {
  arith_precision(op, x, y)
}

#' @export
#' @method vec_arith.precision numeric
vec_arith.precision.numeric <- function(op, x, y, ...) {
  arith_precision(op, x, y)
}

#' @export
#' @method vec_arith.precision logical
vec_arith.precision.logical <- function(op, x, y, ...) {
  check_all_na(op, x, y, y)
  arith_precision(op, x, y)
}

#' @export
#' @method vec_arith.precision MISSING
vec_arith.precision.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "+" = x,
    "-" = new_precision(-field_x(x), op_precision(op, x)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric precision
vec_arith.numeric.precision <- function(op, x, y, ...) {
  arith_precision(op, x, y)
}

#' @export
#' @method vec_arith.logical precision
vec_arith.logical.precision <- function(op, x, y, ...) {
  check_all_na(op, x, y, x)
  arith_precision(op, x, y)
}

check_all_na <- function(op, x, y, lgl) {
  if (!all(is.na(lgl))) stop_incompatible_op(op, x, y)
}

operand_value <- function(x, arg) {
  if (is_precision(x)) field_x(x) else as_bare_double(x, arg, call = NULL)
}

arith_precision <- function(op, x, y) {
  # Logical operators have no meaningful precision
  fn <- switch(
    op,
    "+" = `+`, "-" = `-`, "*" = `*`, "/" = `/`, "^" = `^`,
    "%%" = `%%`, "%/%" = `%/%`,
    stop_incompatible_op(op, x, y)
  )
  vx <- operand_value(x, "x")
  vy <- operand_value(y, "y")
  n <- vec_size_common(x = vx, y = vy, .call = NULL)
  new_precision(fn(vx, vy), op_precision(op, x, y, n))
}
