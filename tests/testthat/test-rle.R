test_that("compresses only when worthwhile", {
  expect_equal(state(rle_compress(rep(1L, 63))), "ordinary")
  expect_equal(state(rle_compress(rep(1L, 64))), "compact")
  expect_equal(state(rle_compress(rep(1:25, each = 4))), "compact")
  expect_equal(state(rle_compress(1:100)), "ordinary")
  expect_equal(state(rle_compress(rep(1:26, length.out = 100))), "ordinary")
  expect_equal(state(rle_compress(integer())), "ordinary")
  x <- rep(c(1L, NA, -3L), c(100, 200, 300))
  expect_identical(rle_expand(rle_compress(x)), x)
  expect_identical(rle_expand(rle_compress(1:100)), 1:100)
  expect_error(rle_compress(1), "integer")
})

test_that("make and runs are canonical", {
  x <- rle_make(c(1L, 1L, 2L, 3L, 2L), c(2, 3, 0, 0, 100))
  expect_equal(rle_runs(x), list(values = 1:2, lengths = c(5L, 100L)))
  expect_canonical(x)
  expect_length(rle_make(integer(), integer()), 0)
  e <- rle_make(integer(), integer(), force = TRUE)
  expect_equal(state(e), "compact")
  expect_length(e, 0)
  expect_identical(e[1], NA_integer_)
  expect_identical(rle_expand(e), integer())
  expect_error(rle_make(1L, -1), "non-negative")
  expect_error(rle_make(1L, NA_integer_), "non-negative")
  expect_error(rle_make(1L, 1.5), "whole")
  expect_error(rle_make(1L, 1:2), "same size")
  expect_error(rle_make(1L, Inf), "whole")
  expect_error(rle_make(1:2, c(4e15, 4e15)), "too long")
})

test_that("long vectors are supported without expansion", {
  skip_on_cran()
  x <- rle_make(c(2L, NA, 1L), c(3e9, 1, 3e9))
  expect_equal(length(x), 6e9 + 1)
  expect_equal(rle_runs(x)$lengths, c(3e9, 1, 3e9))
  expect_identical(x[c(1, 3e9, 3e9 + 1, 3e9 + 2, 6e9 + 1, 6e9 + 2)],
                   c(2L, 2L, NA, 1L, 1L, NA))
  expect_equal(rle_coarsest(x), 1L)
  expect_equal(rle_runs(rle_cumcoarsest(x))$values, c(2L, 1L))
  y <- unserialize(serialize(x, NULL))
  expect_equal(rle_runs(y), rle_runs(x))
  expect_error(x[[6e9 + 2]], "subscript")
})

test_that("kernels match ordinary versions", {
  n_fuzz <- as.integer(Sys.getenv("PRECISION_FUZZ", "200"))
  set.seed(1)
  # Reporting an expectation for each comparison would be slow
  failures <- character()
  check <- function(x, y) {
    if (!identical(x, y)) failures <<- c(failures, deparse(sys.call())[1])
  }
  check_canonical <- function(x) {
    v <- rle_runs(x)$values
    v[is.na(v)] <- -99999L
    if (any(diff(v) == 0)) failures <<- c(failures, deparse(sys.call())[1])
  }
  for (i in 1:n_fuzz) {
    n <- sample(c(0, 1, 2, 10, 63, 64, 200, 1000), 1)
    gen <- function(n) {
      out <- integer()
      while (length(out) < n) {
        out <- c(out, rep(sample(c(NA, -3:3), 1), sample(c(1, 2, 20, 100), 1)))
      }
      as.integer(out[seq_len(n)])
    }
    a <- gen(n)
    b <- gen(n)
    forms <- function(x) {
      stale <- force_rle(x + 0L)
      if (length(x)) rle_touch(stale, 1L, x[1])
      list(x, rle_compress(x), force_rle(x), rle_peek(force_rle(x)), stale)
    }
    for (x in forms(a)) {
      check(rle_expand(x), a)
      check(rle_runs(x), ref_runs(a))
      check(rle_coarsest(x), ref_coarsest(a))
      check(rle_expand(rle_cumcoarsest(x)), ref_cum(a))
      check(rle_expand(rle_cap(x, 0L)), pmin(a, 0L))
      check_canonical(rle_cumcoarsest(x))
      for (y in forms(b)) {
        z <- rle_coarsest2(x, y, n)
        check(rle_expand(z), ref_coarsest2(a, b))
        check_canonical(z)
      }
      s <- sample(c(NA, -3:3), 1)
      check(rle_expand(rle_coarsest2(x, s, n)), ref_coarsest2(a, rep(s, n)))
      check(rle_expand(rle_coarsest2(s, x, n)), ref_coarsest2(rep(s, n), a))

      # Subsetting
      for (idx in list(
        sample(c(NA, seq_len(n + 2)), 50, TRUE), seq_len(n), rev(seq_len(n)),
        as.double(seq_len(n + 70)), c(NA_real_, 1.9, n, n + 0.5, 1e10)
      )) {
        check(rle_expand(x[idx]), a[idx])
        check_canonical(x[idx])
      }
      if (n) {
        check(rle_expand(x[-1]), a[-1])
        i <- sample(n, 1)
        check(x[[i]], a[[i]])
      }
      check(anyNA(x), anyNA(a))
      check(is.unsorted(x), is.unsorted(a))
      check(sum(x), sum(a))
      check(sum(x, na.rm = TRUE), sum(a, na.rm = TRUE))
      check(suppressWarnings(min(x)), suppressWarnings(min(a)))
      check(suppressWarnings(max(x, na.rm = TRUE)),
                       suppressWarnings(max(a, na.rm = TRUE)))
      check(sort(x), sort(a))
      check(order(x), order(a))
      check(order(x, decreasing = TRUE), order(a, decreasing = TRUE))
      check(rle_expand(unserialize(serialize(x, NULL))), a)
      # Base operators obtain a writable pointer
      check(x == 1L, a == 1L)
      check(rle_runs(x), ref_runs(a))
    }
  }
  expect_identical(unique(failures), character())
})

test_that("extreme integers", {
  m <- .Machine$integer.max
  a <- rep(c(-m, NA, m), each = 100)
  x <- rle_compress(a)
  expect_identical(rle_coarsest(x), -m)
  expect_identical(rle_expand(rle_coarsest2(x, rev(x))), ref_coarsest2(a, rev(a)))
  # Overflow gives a double or NA with a warning, depending on the R version
  expect_identical(suppressWarnings(sum(x[201:300])), suppressWarnings(sum(a[201:300])))
  expect_equal(sum(x, na.rm = TRUE), 0L)
})

test_that("recycling errors", {
  expect_error(rle_coarsest2(1:2, 1:3, 3), "recycle")
  expect_length(rle_coarsest2(1L, integer(), 0), 0)
})

test_that("reading keeps runs, writing makes them stale", {
  x <- rle_make(1:2, c(100, 100))
  expect_equal(state(x), "compact")
  # Methods don't materialise
  sum(x); min(x); x[50]; x[[150]]; anyNA(x); length(x); rle_runs(x); rle_expand(x)
  expect_equal(state(x), "compact")
  rle_peek(x)
  expect_equal(state(x), "materialised")
  expect_equal(rle_runs(x)$lengths, c(100L, 100L))

  rle_touch(x, 1L, 9L)
  expect_equal(state(x), "stale")
  expect_identical(rle_runs(x), list(values = c(9L, 1L, 2L), lengths = c(1L, 99L, 100L)))
  expect_identical(rle_coarsest(x), 1L)
  rle_touch(x, 200L, -5L)
  expect_identical(rle_coarsest(x), -5L)
  expect_identical(min(x), -5L)
  expect_identical(sum(x), sum(c(9L, rep(1L, 99), rep(2L, 99), -5L)))
  expect_identical(x[c(1, 200, 201)], c(9L, -5L, NA))
  expect_true(is.unsorted(x))
  expect_equal(state(rle_compress(x)), "compact")
  expect_identical(rle_expand(unserialize(serialize(x, NULL))), rle_expand(x))
})

test_that("copies are independent", {
  x <- rle_make(1:2, c(100, 100))
  y <- x
  y[1] <- 7L
  expect_identical(x[1], 1L)
  expect_identical(y[1], 7L)
  expect_identical(rle_runs(x)$values, 1:2)
  z <- rle_fresh(x)
  rle_touch(z, 1L, 5L)
  expect_equal(state(x), "compact")
  expect_identical(x[1], 1L)
  # Copies of stale vectors
  w <- z
  w[2] <- 8L
  expect_identical(z[1:2], c(5L, 1L))
  expect_identical(w[1:2], c(5L, 8L))
  # Run tables can't be modified by results
  r <- rle_runs(x)
  r$values[1] <- 100L
  expect_identical(x[1], 1L)
})

test_that("inspects", {
  insp <- function(x) paste(capture.output(.Internal(inspect(x))), collapse = "")
  x <- rle_make(1:2, c(100, 100))
  expect_match(insp(x), "len: 200, runs: 2, compact")
  rle_peek(x)
  expect_match(insp(x), "materialised")
  rle_touch(x, 1L, 1L)
  expect_match(insp(x), "stale")
})

test_that("serialises runs", {
  x <- rle_make(1:2, c(1e6, 1e6))
  s <- serialize(x, NULL)
  expect_lt(length(s), 1000)
  expect_equal(state(unserialize(s)), "compact")
  rle_peek(x)
  expect_lt(length(serialize(x, NULL)), 1000)
})

test_that("serialises to a new process", {
  skip_on_cran()
  # load_all() doesn't provide an installed package, so needs a library with one
  path <- getNamespaceInfo("precision", "path")
  lib <- Sys.getenv("PRECISION_TEST_LIB", dirname(path))
  skip_if_not(file.exists(file.path(lib, "precision", "Meta", "package.rds")))
  rds <- tempfile(fileext = ".rds")
  script <- tempfile(fileext = ".R")
  on.exit(unlink(c(rds, script)))
  saveRDS(precision(1:1e5 / 8, rep(c(1L, -1L), each = 5e4)), rds)
  expect_lt(length(serialize(field_p(readRDS(rds)), NULL)), 1000)
  # The package isn't attached or loaded beforehand
  writeLines(c(
    sprintf(".libPaths(c(%s, .libPaths()))", deparse(lib)),
    sprintf("x <- readRDS(%s)", deparse(rds)),
    "stopifnot(format(x[c(1, 100000)]) == c('0.1', '12500'))",
    "stopifnot(precision::prec_runs(x)$length == 5e4)",
    "cat(precision:::rle_state(precision::prec_digits(x)))"
  ), script)
  out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", script), stdout = TRUE)
  expect_equal(out, "compact")
})
