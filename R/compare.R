#' Comparing numbers with known precision
#'
#' Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`) round both sides to their
#' coarsest known precision, and give a logical vector. Values which display
#' equally at the same precision are therefore always equal. Ordinary numbers
#' are exact, so use the precision of the number they are compared with. Numbers
#' are compared exactly when neither has known precision.
#'
#' @details
#' Equality is not transitive when precision differs: `3.12` and `3.14` both equal
#' `3.1`, but not each other. So [unique()], [duplicated()], [match()], [sort()],
#' [order()] and grouping instead use each element's own rounded value *and* its
#' precision: `3.1` and `3.10` are distinct, with the coarser sorted first. All
#' missing values are the same, regardless of their precision.
#'
#' `min()`, `max()` and `range()` use the full value.
#'
#' @name sigfig-comparison
#' @examples
#' sigfig(3.14159, 2) == 3.14
#' sigfig(3.12, 2) == sigfig(3.1, 1)
#' unique(sigfig(c(3.1, 3.1, 3.12), c(1, 2, 1)))
NULL

compare_sigfig <- function(op, e1, e2) {
  for (e in list(e1, e2)) {
    ok <- is_sigfig(e) ||
      (is.numeric(e) && !is.object(e)) ||
      (is.logical(e) && !is.object(e) && all(is.na(e)))
    if (!ok) stop_incompatible_op(op, e1, e2)
  }
  x <- operand_value(e1, "e1")
  y <- operand_value(e2, "e2")
  n <- vec_size_common(x = x, y = y, .call = NULL)
  p <- op_sigfig(op, e1, e2, n)
  fn <- switch(
    op, "==" = `==`, "!=" = `!=`, "<" = `<`, "<=" = `<=`, ">" = `>`, ">=" = `>=`
  )
  fn(prec_round(x, p, n), prec_round(y, p, n))
}

#' @export
`==.sigfig` <- function(e1, e2) compare_sigfig("==", e1, e2)

#' @export
`!=.sigfig` <- function(e1, e2) compare_sigfig("!=", e1, e2)

#' @export
`<.sigfig` <- function(e1, e2) compare_sigfig("<", e1, e2)

#' @export
`<=.sigfig` <- function(e1, e2) compare_sigfig("<=", e1, e2)

#' @export
`>.sigfig` <- function(e1, e2) compare_sigfig(">", e1, e2)

#' @export
`>=.sigfig` <- function(e1, e2) compare_sigfig(">=", e1, e2)
