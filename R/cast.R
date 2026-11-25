#' Convert to numbers with known precision
#'
#' Plain numbers are exact, so have missing precision. The precision of text is
#' the number of decimal places written, less any exponent: `"3.20"` is known to
#' 2 decimal places, and `"1.20e3"` to the nearest ten (`-1`).
#'
#' Text of numbers with non-negative precision is restored by `as_sigfig()`.
#' The digits of `"1200"` are assumed to be known (`p = 0`), as text doesn't
#' describe if trailing zeros of whole numbers are known.
#'
#' @param x A numeric, character or [sigfig()] vector.
#' @param ... Not used.
#'
#' @return A [sigfig()] vector. Text which isn't a number is an error, except
#'   `NA` and empty strings which are missing.
#' @examples
#' as_sigfig(3.2)
#' as_sigfig(c("3.20", "4.1", "1.20e3"))
#'
#' # The full value and displayed text
#' x <- sigfig(3.14159, 2)
#' as.double(x)
#' as.character(x)
#' @export
as_sigfig <- function(x, ...) {
  UseMethod("as_sigfig")
}

#' @export
as_sigfig.default <- function(x, ...) {
  vec_cast(x, new_sigfig())
}

#' @export
as_sigfig.sigfig <- function(x, ...) {
  x
}

#' @export
as_sigfig.numeric <- function(x, ...) {
  sigfig(x)
}

#' @export
as_sigfig.character <- function(x, ...) {
  parse_sigfig(as.vector(x))
}

number_pattern <-
  "^[+-]?([0-9]+[.]?([0-9]*)|[.]([0-9]+))([eE]([+-]?[0-9]+))?$"
special_pattern <- "^([+-]?(inf|infinity)|nan)$"

parse_sigfig <- function(x, to = new_sigfig(), ..., x_arg = "x",
                            to_arg = "", call = caller_env()) {
  text <- trimws(x)
  text[!is.na(text) & text %in% c("", "NA")] <- NA_character_
  number <- grepl(number_pattern, text)
  special <- grepl(special_pattern, text, ignore.case = TRUE)

  bad <- !is.na(text) & !number & !special
  if (any(bad)) {
    stop_incompatible_cast(
      x, to, x_arg = x_arg, to_arg = to_arg, call = call,
      details = paste0(
        "Not a number: ", paste0('"', utils::head(x[bad], 3L), '"', collapse = ", "),
        if (sum(bad) > 3L) ", ..."
      )
    )
  }

  value <- rep_len(NA_real_, length(x))
  ok <- number | special
  value[ok] <- as.double(text[ok])

  p <- rep_len(NA_integer_, length(x))
  if (any(number)) {
    num <- text[number]
    decimals <- nchar(gsub("[eE].*$|^[^.]*[.]?", "", num))
    exponent <- rep_len(0, length(num))
    has_exp <- grepl("[eE]", num)
    exponent[has_exp] <- as.double(sub("^.*[eE]", "", num[has_exp]))
    # as_digits() checks the range of text like "1e-9999999999"
    p[number] <- as_digits(
      pmax(pmin(decimals - exponent, 1e9), -1e9), x_arg, call = call
    )
  }
  sigfig(value, p)
}

# Prototypes -------------------------------------------------------------------

#' @export
vec_ptype2.sigfig.sigfig <- function(x, y, ...) new_sigfig()
#' @export
vec_ptype2.sigfig.double <- function(x, y, ...) new_sigfig()
#' @export
vec_ptype2.double.sigfig <- function(x, y, ...) new_sigfig()
#' @export
vec_ptype2.sigfig.integer <- function(x, y, ...) new_sigfig()
#' @export
vec_ptype2.integer.sigfig <- function(x, y, ...) new_sigfig()

# Casts ------------------------------------------------------------------------
# vctrs handles logical vectors of only missing values

#' @export
vec_cast.sigfig.sigfig <- function(x, to, ...) x
#' @export
vec_cast.sigfig.double <- function(x, to, ...) sigfig(x)
#' @export
vec_cast.sigfig.integer <- function(x, to, ...) sigfig(x)
#' @export
vec_cast.sigfig.character <- function(x, to, ..., x_arg = "x", to_arg = "",
                                         call = caller_env()) {
  parse_sigfig(x, to, x_arg = x_arg, to_arg = to_arg, call = call)
}
#' @export
vec_cast.double.sigfig <- function(x, to, ...) field_x(x)
#' @export
vec_cast.integer.sigfig <- function(x, to, ..., x_arg = "", to_arg = "",
                                       call = caller_env()) {
  vec_cast(field_x(x), integer(), x_arg = x_arg, to_arg = to_arg, call = call)
}
#' @export
vec_cast.character.sigfig <- function(x, to, ...) prec_format(x)

#' @export
as.double.sigfig <- function(x, ...) field_x(x)

# Truncates, like base
#' @export
as.integer.sigfig <- function(x, ...) as.integer(field_x(x))

#' @export
as.character.sigfig <- function(x, ...) prec_format(x)
