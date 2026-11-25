test_that("constructs", {
  x <- precision(c(1.25, 2), c(2, 0))
  expect_s3_class(x, c("precision", "vctrs_rcrd", "vctrs_vctr"), exact = TRUE)
  expect_true(is_precision(x))
  expect_false(is_precision(1))
  expect_identical(prec_value(x), c(1.25, 2))
  expect_identical(prec_digits(x), c(2L, 0L))
  expect_length(precision(), 0)
  expect_identical(prec_digits(precision(1:3)), rep(NA_integer_, 3))
  expect_identical(prec_value(precision(1, 1:2)), c(1, 1))
  expect_identical(precision(c(a = 1), c(b = 1)), precision(1, 1))
  expect_null(names(precision(c(a = 1))))
  expect_error(precision(1:2, 1:3), class = "vctrs_error_incompatible_size")
  expect_error(precision("a"), class = "vctrs_error_incompatible_type")
  expect_error(precision(1, 1.5), class = "vctrs_error_cast_lossy")
  expect_error(precision(1, 1101), "between")
  expect_error(precision(matrix(1:4, 2)), "dimensions")
  expect_error(new_precision(1L, 1L), "double")
  expect_error(new_precision(1, 1), "integer")
  expect_error(new_precision(1, 1:2), "same size")
  expect_error(prec_value(1), "precision")
})

test_that("accessors", {
  x <- precision(1:200, rep(1:2, each = 100))
  expect_equal(prec_runs(x), data.frame(value = 1:2, length = c(100L, 100L)))
  expect_equal(prec_runs(precision(1:3, c(1, 2, 1)))$value, c(1L, 2L, 1L))
  expect_identical(nrow(prec_runs(precision())), 0L)
  expect_identical(prec_digits(set_digits(x, 5))[200], 5L)
  expect_identical(format(set_digits(precision(1.256, 1), 2)), "1.26")
  expect_error(set_digits(x, 1:3), "size 1 or 200")
  p <- prec_digits(x)
  p[1] <- 9L
  expect_identical(prec_digits(x)[1], 1L)
})

test_that("1e6 elements stay compact", {
  n <- 1e6
  x <- precision(as.double(seq_len(n)), rep(1:2, each = n / 2))
  used <- function(x) length(serialize(field_p(x), NULL))
  for (y in list(
    x, x + 1, 1 - x, x * x, x / precision(3, 1), -x, exp(x / n), round(x, 1),
    floor(x), cumsum(x), x[-1], x[seq(1, n, 2)], rev(x), rep(x, 2), c(x, x),
    x[x > 10], set_digits(x, 3), vctrs::vec_assign(x, 5, precision(1, 9)),
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
  x <- precision(runif(1000), rep_len(1:3, 1000))
  expect_equal(state(x), "ordinary")
  expect_equal(prec_digits(x + x), rep_len(1:3, 1000))
  expect_equal(state(x + precision(1, 0)), "compact")
  expect_equal(state(x[seq(1, 1000, 3)]), "compact")
  expect_equal(state(x[1:3]), "ordinary")
})

test_that("modification doesn't affect copies", {
  x <- precision(1:100, 1)
  y <- x + 1
  z <- x
  z[2] <- precision(5.123, 3)
  z[[3]] <- 1
  expect_identical(format(z[1:4]), c("1.0", "5.123", "1", "4.0"))
  expect_identical(format(x[1:4]), c("1.0", "2.0", "3.0", "4.0"))
  expect_identical(format(y[2]), "3.0")
  expect_equal(nrow(prec_runs(z)), 4)
  vctrs::field(z, "p") <- rep(0L, 100)
  expect_identical(format(z[1:2]), c("1", "5"))
})

test_that("vctrs tools", {
  x <- precision(c(1.5, NA, 2.25, NA), c(1, 2, 2, NA))
  expect_identical(vctrs::vec_slice(x, 3), precision(2.25, 2))
  expect_identical(vctrs::vec_c(x[1], 2, 3L, NA), precision(c(1.5, 2, 3, NA), c(1, NA, NA, NA)))
  expect_identical(vctrs::vec_c(1, x[1]), precision(c(1, 1.5), c(NA, 1)))
  expect_identical(c(x[1], x[3]), x[c(1, 3)])
  expect_identical(vctrs::vec_init(x, 2), precision(c(NA, NA)))
  expect_identical(is.na(x), c(FALSE, TRUE, FALSE, TRUE))
  expect_true(anyNA(x))
  expect_identical(vctrs::vec_detect_missing(x), is.na(x))
  # Unknown precision isn't missing
  expect_true(vctrs::vec_detect_complete(precision(1)))
  expect_identical(vctrs::vec_unique_count(x), 3L)
  expect_identical(x[c(1, NA)], precision(c(1.5, NA), c(1, NA)))
  expect_identical(x[-(1:4)], precision())
  expect_identical(rep(x[1], 3), precision(rep(1.5, 3), 1))
  expect_length(x, 4)
  expect_length(as.list(x), 4)
  expect_identical(x[[3]], precision(2.25, 2))
  expect_identical(unclass(vctrs::vec_ptype_abbr(x)), "prec")
  expect_identical(vctrs::vec_ptype_full(x), "precision")
  is.na(x) <- 1
  expect_true(is.na(x[1]))
})

test_that("unique, sort and match use rounded value and p", {
  x <- precision(c(3.1, 3.1, 3.12, NA, NA, -0.04, 0.04), c(1, 2, 1, 1, 2, 1, 1))
  expect_identical(format(unique(x)), c("3.1", "3.10", "NA", "0.0"))
  expect_identical(duplicated(x), c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  y <- precision(c(2, 3.14, 3.1, 1), c(1, 2, 1, NA))
  expect_identical(format(sort(y)), c("1", "2.0", "3.1", "3.14"))
  expect_identical(order(precision(c(1, 1, 1, 1), c(NA, 2, 0, 1))), c(3L, 4L, 2L, 1L))
  # 3.14 and 3.06 both show as 3.1 at p = 1
  expect_identical(order(precision(c(3.14, 3.06), 1)), 1:2)
  expect_identical(sort(precision(c(2, 1, 3), 0), decreasing = TRUE), precision(3:1, 0))
  expect_identical(vctrs::vec_match(precision(3.14, 1), x), 1L)
  skip_if(getRversion() < "4.2.0")
  expect_identical(match(precision(c(3.14, 3.12), c(1, 2)), x), c(1L, NA))
  expect_true(2 %in% precision(2))
  expect_false(precision(1200, -2) %in% precision(1200, 0))
})

test_that("data frames", {
  d <- data.frame(x = precision(c(1.5, 2.25, 1.5), c(1, 2, 1)))
  expect_identical(d[2, "x"], precision(2.25, 2))
  expect_identical(as.character(format(d)$x), c("1.5", "2.25", "1.5"))
  expect_output(print(d), "2.25")
  expect_identical(rbind(d, d)$x, c(d$x, d$x))
  expect_identical(nrow(unique(d)), 2L)
  expect_identical(merge(d, d[2, , drop = FALSE])$x, precision(2.25, 2))
  expect_output(str(d), "prec")
})

test_that("tibbles", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("pillar")
  tb <- tibble::tibble(x = precision(c(1.5, 22.25, NA), c(1, 2, 1)))
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
  d <- dplyr::tibble(k = precision(c(1.04, 1, 2), 1), v = precision(c(1, 2, 3.25), c(1, 2, 2)))
  out <- dplyr::summarise(dplyr::group_by(d, k), v = sum(v))
  expect_identical(format(out$k), c("1.0", "2.0"))
  expect_identical(format(out$v), c("3.0", "3.25"))
  expect_identical(format(dplyr::arrange(d, dplyr::desc(v))$v), c("3.25", "2.00", "1.0"))
  expect_identical(dplyr::if_else(c(TRUE, FALSE), d$v[1:2], 0), precision(c(1, 0), c(1, NA)))
})
