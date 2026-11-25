#' Format numbers with known precision
#'
#' Values are rounded to their precision, and displayed in fixed notation with
#' exactly that many decimal places. With negative precision, the unknown digits
#' are zeros. Numbers without known precision are displayed as ordinary numbers,
#' using up to 15 significant digits.
#'
#' @param x A [sigfig()] vector.
#' @param ... Not used.
#'
#' @return An unnamed character vector the size of `x`. Missing values are
#'   `"NA"`, or `NA` with `as.character()`.
#' @examples
#' format(sigfig(1234.567, c(4, 1, 0, -2, NA)))
#' @export
format.sigfig <- function(x, ...) {
  out <- prec_format(x)
  out[is.na(out)] <- "NA"
  out
}

# Missing values are NA
prec_format <- function(x) {
  .Call(C_prec_format, field_x(x), field_p(x))
}

#' @export
vec_ptype_abbr.sigfig <- function(x, ...) {
  "prec"
}

#' @export
vec_ptype_full.sigfig <- function(x, ...) {
  "sigfig"
}

#' @export
obj_print_data.sigfig <- function(x, ..., max = getOption("max.print")) {
  n <- length(x)
  if (n == 0L) return(invisible(x))
  shown <- if (n > max) x[seq_len(max)] else x
  print(format(shown), quote = FALSE, right = TRUE, max = max)
  if (n > max) {
    cat(" [ omitted ", n - max, " entries ]\n", sep = "")
  }
  invisible(x)
}
