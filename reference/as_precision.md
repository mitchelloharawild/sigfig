# Convert to numbers with known precision

Plain numbers are exact, so have missing precision. The precision of
text is the number of decimal places written, less any exponent:
`"3.20"` is known to 2 decimal places, and `"1.20e3"` to the nearest ten
(`-1`).

## Usage

``` r
as_precision(x, ...)
```

## Arguments

- x:

  A numeric, character or
  [`precision()`](https://pkg.mitchelloharawild.com/sigfig/reference/precision.md)
  vector.

- ...:

  Not used.

## Value

A
[`precision()`](https://pkg.mitchelloharawild.com/sigfig/reference/precision.md)
vector. Text which isn't a number is an error, except `NA` and empty
strings which are missing.

## Details

Text of numbers with non-negative precision is restored by
`as_precision()`. The digits of `"1200"` are assumed to be known
(`p = 0`), as text doesn't describe if trailing zeros of whole numbers
are known.

## Examples

``` r
as_precision(3.2)
#> <precision[1]>
#> [1] 3.2
as_precision(c("3.20", "4.1", "1.20e3"))
#> <precision[3]>
#> [1] 3.20  4.1 1200

# The full value and displayed text
x <- precision(3.14159, 2)
as.double(x)
#> [1] 3.14159
as.character(x)
#> [1] "3.14"
```
