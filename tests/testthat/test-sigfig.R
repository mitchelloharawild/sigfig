test_that("constructs", {
  x <- sigfig(c(1.25, 2), c(2, 0))
  expect_s3_class(x, c("sigfig", "vctrs_rcrd", "vctrs_vctr"), exact = TRUE)
  expect_true(is_sigfig(x))
  expect_false(is_sigfig(1))
  expect_identical(prec_value(x), c(1.25, 2))
  expect_identical(prec_digits(x), c(2L, 0L))
  expect_length(sigfig(), 0)
  expect_identical(prec_digits(sigfig(1:3)), rep(NA_integer_, 3))
  expect_identical(prec_value(sigfig(1, 1:2)), c(1, 1))
  expect_identical(sigfig(c(a = 1), c(b = 1)), sigfig(1, 1))
  expect_null(names(sigfig(c(a = 1))))
  expect_error(sigfig(1:2, 1:3), class = "vctrs_error_incompatible_size")
  expect_error(sigfig("a"), class = "vctrs_error_incompatible_type")
  expect_error(sigfig(1, 1.5), class = "vctrs_error_cast_lossy")
  expect_error(sigfig(1, 1101), "between")
  expect_error(sigfig(matrix(1:4, 2)), "dimensions")
  expect_error(new_sigfig(1L, 1L), "double")
  expect_error(new_sigfig(1, 1), "integer")
  expect_error(new_sigfig(1, 1:2), "same size")
  expect_error(prec_value(1), "sigfig")
})

test_that("accessors", {
  x <- sigfig(1:200, rep(1:2, each = 100))
  expect_equal(prec_runs(x), data.frame(value = 1:2, length = c(100L, 100L)))
  expect_equal(prec_runs(sigfig(1:3, c(1, 2, 1)))$value, c(1L, 2L, 1L))
  expect_identical(nrow(prec_runs(sigfig())), 0L)
  expect_identical(prec_digits(set_digits(x, 5))[200], 5L)
  expect_identical(format(set_digits(sigfig(1.256, 1), 2)), "1.26")
  expect_error(set_digits(x, 1:3), "size 1 or 200")
  p <- prec_digits(x)
  p[1] <- 9L
  expect_identical(prec_digits(x)[1], 1L)
})

test_that("1e6 elements stay compact", {
  n <- 1e6
  x <- sigfig(as.double(seq_len(n)), rep(1:2, each = n / 2))
  used <- function(x) length(serialize(field_p(x), NULL))
  for (y in list(
    x, x + 1, 1 - x, x * x, x / sigfig(3, 1), -x, exp(x / n), round(x, 1),
    floor(x), cumsum(x), x[-1], x[seq(1, n, 2)], rev(x), rep(x, 2), c(x, x),
    x[x > 10], set_digits(x, 3), vctrs::vec_assign(x, 5, sigfig(1, 9)),
    vctrs::vec_rbind(data.frame(x = x), data.frame(x = x))$x,
    unserialize(serialize(x, NULL))
  )) {
    expect_equal(state(y), "compact")
    expect_lt(used(y), 1000)
  }
  # Without materialising the source
  sum(x); range(x); x == 1; invisible(format(x[1:10])); vctrs::vec_unique(x[1:1000])
  invisible(vctrs::vec_chop(x, indices = list(1:10, 11:20)))
  invisible(sort(x)); is.na(x); head(x); vctrs::vec_group_id(x)
  expect_equal(state(x), "compact")

  # Actual memory, as object.size() assumes that p is expanded
  gc()
  before <- gc()[2, 2]
  y <- x + x
  z <- y[seq_len(n - 1)]
  extra <- gc()[2, 2] - before
  expect_lt(extra, 17) # 8 MB each for x, with no p (4 MB each)
})

test_that("p which doesn't compress", {
  x <- sigfig(runif(1000), rep_len(1:3, 1000))
  expect_equal(state(x), "ordinary")
  expect_equal(prec_digits(x + x), rep_len(1:3, 1000))
  expect_equal(state(x + sigfig(1, 0)), "compact")
  expect_equal(state(x[seq(1, 1000, 3)]), "compact")
  expect_equal(state(x[1:3]), "ordinary")
})

test_that("modification doesn't affect copies", {
  x <- sigfig(1:100, 1)
  y <- x + 1
  z <- x
  z[2] <- sigfig(5.123, 3)
  z[[3]] <- 1
  expect_identical(format(z[1:4]), c("1.0", "5.123", "1", "4.0"))
  expect_identical(format(x[1:4]), c("1.0", "2.0", "3.0", "4.0"))
  expect_identical(format(y[2]), "3.0")
  expect_equal(nrow(prec_runs(z)), 4)
  vctrs::field(z, "p") <- rep(0L, 100)
  expect_identical(format(z[1:2]), c("1", "5"))
})

test_that("vctrs tools", {
  x <- sigfig(c(1.5, NA, 2.25, NA), c(1, 2, 2, NA))
  expect_identical(vctrs::vec_slice(x, 3), sigfig(2.25, 2))
  expect_identical(vctrs::vec_c(x[1], 2, 3L, NA), sigfig(c(1.5, 2, 3, NA), c(1, NA, NA, NA)))
  expect_identical(vctrs::vec_c(1, x[1]), sigfig(c(1, 1.5), c(NA, 1)))
  expect_identical(c(x[1], x[3]), x[c(1, 3)])
  expect_identical(vctrs::vec_init(x, 2), sigfig(c(NA, NA)))
  expect_identical(is.na(x), c(FALSE, TRUE, FALSE, TRUE))
  expect_true(anyNA(x))
  expect_identical(vctrs::vec_detect_missing(x), is.na(x))
  # Unknown precision isn't missing
  expect_true(vctrs::vec_detect_complete(sigfig(1)))
  expect_identical(vctrs::vec_unique_count(x), 3L)
  expect_identical(x[c(1, NA)], sigfig(c(1.5, NA), c(1, NA)))
  expect_identical(x[-(1:4)], sigfig())
  expect_identical(rep(x[1], 3), sigfig(rep(1.5, 3), 1))
  expect_length(x, 4)
  expect_length(as.list(x), 4)
  expect_identical(x[[3]], sigfig(2.25, 2))
  expect_identical(unclass(vctrs::vec_ptype_abbr(x)), "prec")
  expect_identical(vctrs::vec_ptype_full(x), "sigfig")
  is.na(x) <- 1
  expect_true(is.na(x[1]))
})

test_that("unique, sort and match use rounded value and p", {
  x <- sigfig(c(3.1, 3.1, 3.12, NA, NA, -0.04, 0.04), c(1, 2, 1, 1, 2, 1, 1))
  expect_identical(format(unique(x)), c("3.1", "3.10", "NA", "0.0"))
  expect_identical(duplicated(x), c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  y <- sigfig(c(2, 3.14, 3.1, 1), c(1, 2, 1, NA))
  expect_identical(format(sort(y)), c("1", "2.0", "3.1", "3.14"))
  expect_identical(order(sigfig(c(1, 1, 1, 1), c(NA, 2, 0, 1))), c(3L, 4L, 2L, 1L))
  # 3.14 and 3.06 both show as 3.1 at p = 1
  expect_identical(order(sigfig(c(3.14, 3.06), 1)), 1:2)
  expect_identical(sort(sigfig(c(2, 1, 3), 0), decreasing = TRUE), sigfig(3:1, 0))
  expect_identical(vctrs::vec_match(sigfig(3.14, 1), x), 1L)
  skip_if(getRversion() < "4.2.0")
  expect_identical(match(sigfig(c(3.14, 3.12), c(1, 2)), x), c(1L, NA))
  expect_true(2 %in% sigfig(2))
  expect_false(sigfig(1200, -2) %in% sigfig(1200, 0))
})

test_that("data frames", {
  d <- data.frame(x = sigfig(c(1.5, 2.25, 1.5), c(1, 2, 1)))
  expect_identical(d[2, "x"], sigfig(2.25, 2))
  expect_identical(as.character(format(d)$x), c("1.5", "2.25", "1.5"))
  expect_output(print(d), "2.25")
  expect_identical(rbind(d, d)$x, c(d$x, d$x))
  expect_identical(nrow(unique(d)), 2L)
  expect_identical(merge(d, d[2, , drop = FALSE])$x, sigfig(2.25, 2))
  expect_output(str(d), "prec")
})

test_that("tibbles", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("pillar")
  tb <- tibble::tibble(x = sigfig(c(1.5, 22.25, NA), c(1, 2, 1)))
  out <- cli::ansi_strip(format(tb))
  expect_match(out, "<prec>", all = FALSE)
  expect_match(out, " 1.5$", all = FALSE)
  expect_match(out, "22.25$", all = FALSE)
  shaft <- pillar::pillar_shaft(tb$x)
  expect_identical(attr(shaft, "align"), "right")
  expect_match(cli::ansi_strip(paste(format(shaft, 20), collapse = "")), "NA")
})

test_that("grouping", {
  skip_if_not_installed("dplyr")
  d <- dplyr::tibble(k = sigfig(c(1.04, 1, 2), 1), v = sigfig(c(1, 2, 3.25), c(1, 2, 2)))
  out <- dplyr::summarise(dplyr::group_by(d, k), v = sum(v))
  expect_identical(format(out$k), c("1.0", "2.0"))
  expect_identical(format(out$v), c("3.0", "3.25"))
  expect_identical(format(dplyr::arrange(d, dplyr::desc(v))$v), c("3.25", "2.00", "1.0"))
  expect_identical(dplyr::if_else(c(TRUE, FALSE), d$v[1:2], 0), sigfig(c(1, 0), c(1, NA)))
})
