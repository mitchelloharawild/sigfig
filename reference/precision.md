# Numbers with known precision

A `precision` vector pairs each number with the decimal places to which
it is known. Numbers are displayed and compared as described by their
precision, while calculations use the full value so that rounding error
does not accumulate.

`new_precision()` is a low-level constructor for developers. It requires
a double `x` and integer `p` of the same size.

## Usage

``` r
precision(x = double(), p = NA_integer_)

new_precision(x = double(), p = integer())

is_precision(x)
```

## Arguments

- x:

  For `precision()` and `new_precision()`, a vector of numbers.
  Otherwise, an object to test.

- p:

  The decimal places known, as whole numbers between -1100 and 1100.
  Negative values describe powers of ten: `-2` is known to the nearest
  hundred. Missing values (the default) are used if the precision is not
  known, or if the number is exact.

## Value

`precision()` and `new_precision()` return a `precision` vector.
`is_precision()` returns `TRUE` or `FALSE`.

## Details

The precision is stored compactly using run length encoding, so
repeating the same precision throughout a vector uses very little
memory. Names and dimensions are not supported; they are dropped and an
error respectively.

## See also

[precision-arithmetic](https://pkg.mitchelloharawild.com/sigfig/reference/precision-arithmetic.md),
[precision-comparison](https://pkg.mitchelloharawild.com/sigfig/reference/precision-comparison.md),
[`as_precision()`](https://pkg.mitchelloharawild.com/sigfig/reference/as_precision.md),
[`prec_value()`](https://pkg.mitchelloharawild.com/sigfig/reference/prec_value.md)

## Examples

``` r
x <- precision(3.2, 1)
y <- precision(4.12, 2)
x + y
#> <precision[1]>
#> [1] 7.3
prec_value(x + y)
#> [1] 7.32

# Precision can differ throughout the vector
precision(1234.567, c(2, 0, -2, NA))
#> <precision[4]>
#> [1]  1234.57     1235     1200 1234.567
```
