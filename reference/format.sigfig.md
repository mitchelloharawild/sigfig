# Format numbers with known precision

Values are rounded to their precision, and displayed in fixed notation
with exactly that many decimal places. With negative precision, the
unknown digits are zeros. Numbers without known precision are displayed
as ordinary numbers, using up to 15 significant digits.

## Usage

``` r
# S3 method for class 'sigfig'
format(x, ...)
```

## Arguments

- x:

  A
  [`sigfig()`](https://pkg.mitchelloharawild.com/sigfig/reference/sigfig.md)
  vector.

- ...:

  Not used.

## Value

An unnamed character vector the size of `x`. Missing values are `"NA"`,
or `NA` with [`as.character()`](https://rdrr.io/r/base/character.html).

## Examples

``` r
format(sigfig(1234.567, c(4, 1, 0, -2, NA)))
#> [1] "1234.5670" "1234.6"    "1235"      "1200"      "1234.567" 
```
