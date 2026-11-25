# precision

precision provides numbers that know how precisely they are known. Each
element of a `precision` vector pairs a value with the decimal places to
which it is known, which is used in three ways:

- Values are displayed exactly as their precision describes.
- Arithmetic gives a result with the coarsest known precision of its
  inputs, so `3.2 + 4.12` is `7.3`.
- The full value is kept in calculations, so rounding error does not
  accumulate.

The precision is stored with run length encoding using ALTREP, so
repeating the same precision across millions of numbers uses almost no
extra memory, and calculations work on the runs rather than on every
element.

## Installation

You can install the development version of precision from GitHub:

``` r

# install.packages("pak")
pak::pak("mitchelloharawild/precision")
```

## Usage

``` r

library(sigfig)

x <- precision(3.2, p = 1)
y <- precision(4.12, p = 2)

# The coarsest precision is used, but the full value is kept
x + y
#> <precision[1]>
#> [1] 7.3
prec_value(x + y)
#> [1] 7.32
```

Precision can differ throughout a vector, and negative values count
powers of ten.

``` r

precision(1234.567, p = c(2, 0, -2, NA))
#> <precision[4]>
#> [1]  1234.57     1235     1200 1234.567
```

Plain numbers are exact, so don’t affect the precision of a result, and
reductions use the coarsest precision of all of their elements.

``` r

x * 2
#> <precision[1]>
#> [1] 6.4

sum(precision(c(1.234, 5.6), p = c(3, 1)))
#> <precision[1]>
#> [1] 6.8
```

The precision is stored in runs, which are shown by
[`prec_runs()`](https://pkg.mitchelloharawild.com/sigfig/reference/prec_value.md).

``` r

prec_runs(precision(rep(1.5, 1e6), p = 1))
#>   value  length
#> 1     1 1000000
```

See
[`vignette("sigfig")`](https://pkg.mitchelloharawild.com/sigfig/articles/sigfig.md)
for the details of display, comparison, and how the precision is stored.

## Caveats

- Multiplication and division also use the coarsest precision, which is
  a placeholder for a rule that accounts for relative error.
- `precision` vectors are records, so don’t have names or dimensions,
  and some base functions such as
  [`table()`](https://rdrr.io/r/base/table.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) don’t treat them as
  numbers.
