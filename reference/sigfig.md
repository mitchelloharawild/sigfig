# Numbers with known precision

A `sigfig` vector pairs each number with the decimal places to which it
is known. Numbers are displayed and compared as described by their
precision, while calculations use the full value so that rounding error
does not accumulate.

`new_sigfig()` is a low-level constructor for developers. It requires a
double `x` and integer `p` of the same size.

## Usage

``` r
sigfig(x = double(), p = NA_integer_)

new_sigfig(x = double(), p = integer())

is_sigfig(x)
```

## Arguments

- x:

  For `sigfig()` and `new_sigfig()`, a vector of numbers. Otherwise, an
  object to test.

- p:

  The decimal places known, as whole numbers between -1100 and 1100.
  Negative values describe powers of ten: `-2` is known to the nearest
  hundred. Missing values (the default) are used if the precision is not
  known, or if the number is exact.

## Value

`sigfig()` and `new_sigfig()` return a `sigfig` vector. `is_sigfig()`
returns `TRUE` or `FALSE`.

## Details

The precision is stored compactly using run length encoding, so
repeating the same precision throughout a vector uses very little
memory. Names and dimensions are not supported; they are dropped and an
error respectively.

## See also

[sigfig-arithmetic](https://pkg.mitchelloharawild.com/sigfig/reference/sigfig-arithmetic.md),
[sigfig-comparison](https://pkg.mitchelloharawild.com/sigfig/reference/sigfig-comparison.md),
[`as_sigfig()`](https://pkg.mitchelloharawild.com/sigfig/reference/as_sigfig.md),
[`prec_value()`](https://pkg.mitchelloharawild.com/sigfig/reference/prec_value.md)

## Examples

``` r
x <- sigfig(3.2, 1)
y <- sigfig(4.12, 2)
x + y
#> <sigfig[1]>
#> [1] 7.3
prec_value(x + y)
#> [1] 7.32

# Precision can differ throughout the vector
sigfig(1234.567, c(2, 0, -2, NA))
#> <sigfig[4]>
#> [1]  1234.57     1235     1200 1234.567
```
