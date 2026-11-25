# Components of precision vectors

Components of precision vectors

## Usage

``` r
prec_value(x)

prec_digits(x)

prec_runs(x)

set_digits(x, p)
```

## Arguments

- x:

  A
  [`precision()`](https://pkg.mitchelloharawild.com/sigfig/reference/precision.md)
  vector.

- p:

  The decimal places known, see
  [`precision()`](https://pkg.mitchelloharawild.com/sigfig/reference/precision.md).
  Size 1 or the size of `x`.

## Value

- `prec_value()`: the full (unrounded) values.

- `prec_digits()`: an integer vector of the decimal places known.

- `prec_runs()`: a data frame with a row for each run of equal
  `prec_digits()`, with columns `value` and `length`.

- `set_digits()`: `x` with the precision replaced, leaving values
  unchanged.

## Examples

``` r
x <- precision(c(3.14159, 2.71828, 1200), c(2, 2, -2))
x
#> <precision[3]>
#> [1] 3.14 2.72 1200
prec_value(x)
#> [1]    3.14159    2.71828 1200.00000
prec_digits(x)
#> [1]  2  2 -2
prec_runs(x)
#>   value length
#> 1     2      2
#> 2    -2      1
set_digits(x, 3)
#> <precision[3]>
#> [1]    3.142    2.718 1200.000
```
