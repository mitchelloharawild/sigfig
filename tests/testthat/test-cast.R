test_that("from numbers", {
  expect_identical(as_precision(c(1.5, 2)), precision(c(1.5, 2)))
  expect_identical(as_precision(1:2), precision(1:2))
  x <- precision(1, 2)
  expect_identical(as_precision(x), x)
  expect_identical(as_precision(NA), precision(NA))
  expect_error(as_precision(TRUE), class = "vctrs_error_cast")
  expect_error(as_precision(list(1)), class = "vctrs_error_cast")
})

test_that("from text", {
  text <- c("3.20", "4", " -.5 ", "+1.20e3", "1e-3", "1.", NA, "", "Inf", "-inf", "NaN")
  x <- as_precision(text)
  expect_identical(prec_value(x), c(3.2, 4, -0.5, 1200, 0.001, 1, NA, NA, Inf, -Inf, NaN))
  expect_identical(prec_digits(x), c(2L, 0L, 1L, -1L, 3L, 0L, NA, NA, NA, NA, NA))
  expect_identical(vctrs::vec_cast("3.20", precision()), precision(3.2, 2))
  for (bad in c("abc", "1,000", ".", "1e", "0x10", "1.2.3", "e5", "--1")) {
    expect_error(as_precision(bad), class = "vctrs_error_cast")
  }
  expect_error(as_precision("1e-99999999999999999999"), "between")
  expect_length(as_precision(character()), 0)
})

test_that("round trips", {
  f <- c("3.20", "0.001", "-12.5", "7", "0.00", "123456789.12")
  expect_identical(format(as_precision(f)), f)
})

test_that("to other types", {
  x <- precision(c(3.75, NA), 1)
  expect_identical(as.double(x), c(3.75, NA))
  expect_identical(as.numeric(x), c(3.75, NA))
  expect_identical(as.integer(x), c(3L, NA))
  expect_identical(as.character(x), c("3.8", NA))
  expect_identical(vctrs::vec_cast(x, double()), c(3.75, NA))
  expect_identical(vctrs::vec_cast(x, character()), c("3.8", NA))
  expect_error(vctrs::vec_cast(x, integer()), class = "vctrs_error_cast_lossy")
  expect_identical(vctrs::vec_cast(precision(3, 1), integer()), 3L)
})

test_that("common types", {
  x <- precision()
  for (y in list(double(), integer(), NA, precision(1, 1))) {
    expect_identical(vctrs::vec_ptype2(x, y), x)
    expect_identical(vctrs::vec_ptype2(y, x), x)
  }
  expect_error(vctrs::vec_ptype2(x, ""), class = "vctrs_error_incompatible_type")
  expect_error(vctrs::vec_ptype2(x, TRUE), class = "vctrs_error_incompatible_type")
})
