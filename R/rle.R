# Run length encoded integers -------------------------------------------------
#
# Thin wrappers for the C kernels. All accept any integer vector, and return
# either an ALTREP run length encoded integer or (if it wouldn't be smaller) an
# ordinary integer. None materialise compact inputs. See src/rle.c.

rle_compress <- function(x) .Call(C_rle_compress, x)

# `force = TRUE` always uses the ALTREP representation
rle_make <- function(values, lengths, force = FALSE) {
  .Call(C_rle_make, values, lengths, force)
}

rle_rep <- function(value, n) rle_make(value, n)

rle_runs <- function(x) {
  runs <- .Call(C_rle_runs, x)
  list(values = runs[[1L]], lengths = runs[[2L]])
}

# O(1), shares the run table but not materialised data
rle_fresh <- function(x) .Call(C_rle_fresh, x)

rle_expand <- function(x) .Call(C_rle_expand, x)

# Elementwise, recycling size 1 inputs to size n
rle_coarsest2 <- function(x, y, n = vec_size_common(x, y)) {
  .Call(C_rle_coarsest2, x, y, n)
}

rle_coarsest <- function(x) .Call(C_rle_coarsest, x)

rle_cumcoarsest <- function(x) .Call(C_rle_cumcoarsest, x)

# Limit to at most `cap`, with NA unchanged
rle_cap <- function(x, cap) .Call(C_rle_cap, x, cap)

# "ordinary", "compact", "materialised" (but unmodified), or "stale"
rle_state <- function(x) .Call(C_rle_state, x)
