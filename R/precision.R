# Limits of p, beyond which all doubles are unchanged or rounded to 0
max_digits <- 1100L

#' Numbers with known precision
#'
#' A `precision` vector pairs each number with the decimal places to which it is
#' known. Numbers are displayed and compared as described by their precision,
#' while calculations use the full value so that rounding error does not
#' accumulate.
#'
#' The precision is stored compactly using run length encoding, so repeating the
#' same precision throughout a vector uses very little memory. Names and
#' dimensions are not supported; they are dropped and an error respectively.
#'
#' @param x For `precision()` and `new_precision()`, a vector of numbers.
#'   Otherwise, an object to test.
#' @param p The decimal places known, as whole numbers between -1100 and 1100.
#'   Negative values describe powers of ten: `-2` is known to the nearest
#'   hundred. Missing values (the default) are used if the precision is not
#'   known, or if the number is exact.
#'
#' @return `precision()` and `new_precision()` return a `precision` vector.
#'   `is_precision()` returns `TRUE` or `FALSE`.
#'
#' @seealso [precision-arithmetic], [precision-comparison], [as_precision()],
#'   [prec_value()]
#' @examples
#' x <- precision(3.2, 1)
#' y <- precision(4.12, 2)
#' x + y
#' prec_value(x + y)
#'
#' # Precision can differ throughout the vector
#' precision(1234.567, c(2, 0, -2, NA))
#' @export
precision <- function(x = double(), p = NA_integer_) {
  x <- as_bare_double(x)
  p <- as_digits(p)
  n <- vec_size_common(x, p)
  if (length(x) != n) x <- rep_len(x, n)
  if (length(p) != n) p <- rle_rep(p, n)
  new_precision(x, rle_compress(p))
}

#' @description `new_precision()` is a low-level constructor for developers. It
#'   requires a double `x` and integer `p` of the same size.
#' @rdname precision
#' @export
new_precision <- function(x = double(), p = integer()) {
  if (!is.double(x) || !is.null(attributes(x))) {
    cli_abort("{.arg x} must be a double vector without attributes.")
  }
  if (!is.integer(p) || is.object(p)) {
    cli_abort("{.arg p} must be an integer vector.")
  }
  if (length(x) != length(p)) {
    cli_abort("{.arg x} and {.arg p} must be the same size.")
  }
  new_rcrd(list(x = x, p = p), class = "precision")
}

#' @rdname precision
#' @export
is_precision <- function(x) {
  inherits(x, "precision")
}

# Remove all attributes, rejecting dimensions (since size would be ambiguous)
as_bare_double <- function(x, arg = "x", call = caller_env()) {
  if (!is.null(dim(x))) {
    cli_abort("{.arg {arg}} must not have dimensions.", call = call)
  }
  x <- vec_cast(x, double(), x_arg = arg, call = call)
  if (!is.null(attributes(x))) attributes(x) <- NULL
  x
}

as_digits <- function(p, arg = "p", call = caller_env()) {
  p <- vec_cast(p, integer(), x_arg = arg, call = call)
  if (!is.null(attributes(p))) attributes(p) <- NULL
  # Base functions would materialise compact p
  values <- if (rle_state(p) == "compact") rle_runs(p)$values else p
  if (max(values, 0L, na.rm = TRUE) > max_digits ||
      min(values, 0L, na.rm = TRUE) < -max_digits) {
    cli_abort(
      "{.arg {arg}} must be between {-max_digits} and {max_digits}.",
      call = call
    )
  }
  p
}

check_precision <- function(x, arg = "x", call = caller_env()) {
  if (!is_precision(x)) {
    cli_abort(
      "{.arg {arg}} must be a {.cls precision} vector, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}

field_x <- function(x) .subset2(x, "x")
field_p <- function(x) .subset2(x, "p")

#' Components of precision vectors
#'
#' @param x A [precision()] vector.
#' @param p The decimal places known, see [precision()]. Size 1 or the size of
#'   `x`.
#'
#' @return
#' * `prec_value()`: the full (unrounded) values.
#' * `prec_digits()`: an integer vector of the decimal places known.
#' * `prec_runs()`: a data frame with a row for each run of equal `prec_digits()`,
#'   with columns `value` and `length`.
#' * `set_digits()`: `x` with the precision replaced, leaving values unchanged.
#'
#' @examples
#' x <- precision(c(3.14159, 2.71828, 1200), c(2, 2, -2))
#' x
#' prec_value(x)
#' prec_digits(x)
#' prec_runs(x)
#' set_digits(x, 3)
#' @export
prec_value <- function(x) {
  check_precision(x)
  field_x(x)
}

#' @rdname prec_value
#' @export
prec_digits <- function(x) {
  check_precision(x)
  # Base operators can make run tables stale, this spares x's
  rle_fresh(field_p(x))
}

#' @rdname prec_value
#' @export
prec_runs <- function(x) {
  check_precision(x)
  runs <- rle_runs(field_p(x))
  new_data_frame(list(value = runs$values, length = runs$lengths))
}

#' @rdname prec_value
#' @export
set_digits <- function(x, p) {
  check_precision(x)
  p <- as_digits(p)
  n <- length(field_x(x))
  if (length(p) != n) {
    if (length(p) != 1L) {
      cli_abort("{.arg p} must be size 1 or {n}, not {length(p)}.")
    }
    p <- rle_rep(p, n)
  }
  new_precision(field_x(x), rle_compress(p))
}

# Round to displayed values, for any x and p of size 1 or n
prec_round <- function(x, p, n = length(x)) {
  .Call(C_prec_round, x, p, n)
}

# vctrs proxies ----------------------------------------------------------------

# vctrs accesses fields with INTEGER(), which would materialise p and make its
# run table stale. Proxies are given their own ALTREP object to spare x's.
#' @export
vec_proxy.precision <- function(x, ...) {
  new_data_frame(
    list(x = field_x(x), p = rle_fresh(field_p(x))),
    n = length(field_x(x))
  )
}

#' @export
vec_restore.precision <- function(x, to, ...) {
  new_precision(.subset2(x, "x"), rle_compress(.subset2(x, "p")))
}

# Precise values are distinct from those displaying equally with other p, and
# finest (last) in order. All missing values are the same regardless of p.
#' @export
vec_proxy_equal.precision <- function(x, ...) {
  value <- field_x(x)
  p <- field_p(x)
  # vctrs considers rows to be incomplete if any column is missing, so missing p
  # needs to be a value. Expanding first avoids materialising x's precision.
  digits <- rle_expand(p)
  digits[is.na(digits)] <- .Machine$integer.max
  digits[is.na(value)] <- NA_integer_
  new_data_frame(
    list(x = prec_round(value, p), p = digits),
    n = length(value)
  )
}

#' @export
vec_proxy_compare.precision <- function(x, ...) {
  vec_proxy_equal.precision(x)
}

#' @export
vec_proxy_order.precision <- function(x, ...) {
  vec_proxy_equal.precision(x)
}

# Consistent with vec_proxy_equal(), but matching plain numbers when p is NA.
#' @exportS3Method NULL
mtfrm.precision <- function(x) {
  x_round <- prec_round(field_x(x), field_p(x))
  p <- rle_expand(field_p(x))
  out <- as.character(x_round)
  known <- !is.na(p) & !is.na(x_round)
  out[known] <- sprintf("%.17g@%d", x_round[known], p[known])
  out
}

# Missing values ---------------------------------------------------------------

#' @export
is.na.precision <- function(x) {
  is.na(field_x(x))
}

#' @export
anyNA.precision <- function(x, recursive = FALSE) {
  anyNA(field_x(x))
}

# Like base vectors, assigning past the end extends with missing values. Needed
# by rbind() for data frames.
#' @export
`[<-.precision` <- function(x, i, value) {
  if (missing(i)) i <- TRUE
  if (is.numeric(i) && !is.object(i) && length(i)) {
    size <- max(i, 0, na.rm = TRUE)
    if (size > length(x)) length(x) <- size
  }
  value <- vec_cast(value, x)
  vec_assign(x, i, value)
}
