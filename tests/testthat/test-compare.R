test_that("rounds to common coarsest p", {
  x <- precision(3.14159, 2)
  expect_true(x == 3.14)
  expect_true(3.14 == x)
  expect_true(x == 3.1416)
  expect_false(x != 3.14)
  expect_false(x > 3.14)
  expect_true(x >= 3.14)
  expect_true(x <= 3.14)
  expect_false(3 > x)
  expect_true(3 < x)
  expect_true(3L < x)
  expect_true(x == precision(3.1, 1))
  expect_false(x == precision(3.1, 2))
  # Exact
  expect_false(precision(0.1 + 0.2) == 0.3)
})

test_that("recycles", {
  x <- precision(c(1.04, 2, 3), c(1, NA, 0))
  expect_identical(x == 1, c(TRUE, FALSE, FALSE))
  expect_identical(1 == x, c(TRUE, FALSE, FALSE))
  expect_identical(precision(2.96, 2) == x, c(FALSE, FALSE, TRUE))
  expect_identical(x > precision(2, 1), c(FALSE, FALSE, TRUE))
  expect_identical(precision(1, 1) == c(1, 1.04, 2), c(TRUE, TRUE, FALSE))
  expect_identical(precision() == 1, logical())
  expect_error(x == 1:2, class = "vctrs_error_incompatible_size")
})

test_that("missing and special", {
  x <- precision(c(NA, NaN, Inf), 1)
  expect_identical(x == Inf, c(NA, NA, TRUE))
  expect_identical(x == NA, rep(NA, 3))
  expect_identical(x < NA_integer_, rep(NA, 3))
  expect_true(precision(-0.04, 1) == 0)
})

test_that("incompatible", {
  x <- precision(1, 1)
  expect_error(x == "1", class = "vctrs_error_incompatible_op")
  expect_error("1" == x, class = "vctrs_error_incompatible_op")
  expect_error(x == TRUE, class = "vctrs_error_incompatible_op")
})
