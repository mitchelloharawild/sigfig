test_that("fixed notation to p", {
  expect_identical(format(precision(3.2, 1) + precision(4.12, 2)), "7.3")
  expect_identical(format(precision(1234.567, c(4, 1, 0, -2, -4))),
                   c("1234.5670", "1234.6", "1235", "1200", "0"))
  expect_identical(format(precision(c(1e20, 1e-20, 1e5, 123456789), c(0, 2, 1, 0))),
                   c("100000000000000000000", "0.00", "100000.0", "123456789"))
  expect_identical(format(precision(-5555, -3:-4)), c("-6000", "-10000"))
})

test_that("special values", {
  expect_identical(format(precision(c(NA, NaN, Inf, -Inf), 2)), c("NA", "NaN", "Inf", "-Inf"))
  expect_identical(as.character(precision(c(NA, NaN), 1)), c(NA, "NaN"))
  expect_identical(format(precision(c(-0.04, -0, 0, -40, -0.4), c(1, 1, 3, -2, 0))),
                   c("0.0", "0.0", "0.000", "0", "0"))
  expect_identical(format(precision()), character())
  expect_null(names(format(precision(c(a = 1)))))
})

test_that("unknown p", {
  expect_identical(format(precision(c(0.1 + 0.2, 1 / 3, 1e5, 2, -0, NA))),
                   c("0.3", "0.333333333333333", "100000", "2", "0", "NA"))
})

test_that("extreme p", {
  expect_identical(format(precision(1e300, -299)), paste0("1", strrep("0", 300)))
  expect_identical(format(precision(c(1, 1e308), -400)), c("0", "0"))
  expect_identical(format(precision(1.5, 400)), paste0("1.5", strrep("0", 399)))
  expect_identical(nchar(format(precision(1e300, 1100))), 301L + 1L + 1100L)
  expect_identical(format(precision(1e-320, 320)), paste0("0.", strrep("0", 319), "1"))
  expect_identical(format(precision(.Machine$double.xmax, -308)), paste0("1", strrep("0", 308)))
  expect_identical(prec_round(5e-324, 1100L), 5e-324)
})

test_that("rounding matches base at ordinary digits", {
  set.seed(2)
  x <- c(0.15, 0.25, 0.35, 2.5, 3.5, 1.005, 2.675, 5.015, 1250, 1350, runif(5000, -1e4, 1e4),
         round(runif(5000, -100, 100), 3))
  for (p in -3:9) {
    expect_identical(prec_round(x, p), round(x, p) + 0)
  }
})

test_that("equal display means equal", {
  set.seed(3)
  for (p in c(-300:-295, -25:-20, -3:3, 10:30, 305:325, 1074)) {
    mag <- 10^(-p + c(-1, 0, 1, 5, 13, 14, 15, 16, 17))
    mag <- mag[mag > 0 & mag < 1e307]
    base <- rep(mag, each = 8) * runif(8 * length(mag), 0.9, 10)
    x <- c(base, base * (1 + 2^-52), base * (1 - 2^-52), base * (1 + 2^-50), mag, mag * 5)
    r <- prec_round(x, as.integer(p))
    f <- format(new_precision(x, rep(as.integer(p), length(x))))
    expect_identical(match(f, f), match(r, r))
    # Rounding is idempotent
    expect_identical(prec_round(r, as.integer(p)), r)
    expect_identical(format(new_precision(r, rep(as.integer(p), length(r)))), f)
    expect_false(any(grepl("e", f)))
  }
})

test_that("prints", {
  x <- precision(c(1, 22.25, NA), c(0, 2, 1))
  expect_output(print(x), "<precision[3]>", fixed = TRUE)
  expect_output(print(x), "    1 22.25    NA", fixed = TRUE)
  expect_output(print(precision()), "precision[0]", fixed = TRUE)
  withr_max <- options(max.print = 5)
  on.exit(options(withr_max))
  expect_output(print(precision(1:10, 1)), "omitted 5 entries")
})
