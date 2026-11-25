# Write in place via a writable pointer, or obtain a read-only pointer
rle_touch <- function(x, i, value) .Call(C_rle_touch, x, i, value)
rle_peek <- function(x) .Call(C_rle_peek, x)

state <- function(x) rle_state(if (is_sigfig(x)) field_p(x) else x)

# An RLE even if it is small
force_rle <- function(x) {
  r <- rle(ifelse(is.na(x), -99999L, as.integer(x)))
  v <- as.integer(r$values)
  v[v == -99999L] <- NA
  rle_make(v, r$lengths, force = TRUE)
}

ref_coarsest2 <- function(x, y) {
  out <- suppressWarnings(pmin(x, y, na.rm = TRUE))
  as.integer(out)
}
ref_coarsest <- function(x) if (all(is.na(x))) NA_integer_ else min(x, na.rm = TRUE)
ref_cum <- function(x) {
  x[is.na(x)] <- .Machine$integer.max
  x <- cummin(x)
  x[x == .Machine$integer.max] <- NA
  x
}
ref_runs <- function(x) {
  r <- rle(ifelse(is.na(x), -99999L, x))
  r$values[r$values == -99999L] <- NA
  list(values = as.integer(r$values), lengths = r$lengths)
}

# Independent of materialisation
expect_canonical <- function(x) {
  r <- rle_runs(x)
  testthat::expect_true(all(r$lengths > 0))
  v <- ifelse(is.na(r$values), -99999L, r$values)
  testthat::expect_false(any(diff(v) == 0))
  testthat::expect_equal(sum(r$lengths), length(x))
}
