# Arithmetic with known precision

Results are calculated using the full values. The precision of the
result is the coarsest known precision of the inputs, so `3.2 + 4.12` is
`7.3`. Ordinary numbers are exact, and don't affect the precision of the
result.

## Details

- Arithmetic:

  `+`, `-`, `*`, `/`, `^`, `%%` and `%/%` use the coarsest known
  precision of each pair of elements. Inputs must be the same size, or
  size 1.

- Summaries:

  [`sum()`](https://rdrr.io/r/base/sum.html),
  [`prod()`](https://rdrr.io/r/base/prod.html),
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html),
  [`range()`](https://rdrr.io/r/base/range.html),
  [`median()`](https://rdrr.io/r/stats/median.html) and
  [`quantile()`](https://rdrr.io/r/stats/quantile.html) use the coarsest
  known precision of all elements (excluding missing values if
  `na.rm = TRUE`).

- Cumulative:

  [`cumsum()`](https://rdrr.io/r/base/cumsum.html),
  [`cumprod()`](https://rdrr.io/r/base/cumsum.html),
  [`cummin()`](https://rdrr.io/r/base/cumsum.html) and
  [`cummax()`](https://rdrr.io/r/base/cumsum.html) use the coarsest
  known precision of the elements so far.

- Rounding:

  [`round()`](https://rdrr.io/r/base/Round.html) and
  [`signif()`](https://rdrr.io/r/base/Round.html) round the value, and
  the precision becomes no finer than the digits rounded to.
  [`floor()`](https://rdrr.io/r/base/Round.html),
  [`ceiling()`](https://rdrr.io/r/base/Round.html) and
  [`trunc()`](https://rdrr.io/r/base/Round.html) similarly reduce known
  precision to no finer than 0, while exact numbers remain exact.
  [`sign()`](https://rdrr.io/r/base/sign.html) is exact.

- Other functions:

  [`exp()`](https://rdrr.io/r/base/Log.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
  [`abs()`](https://rdrr.io/r/base/MathFun.html) and so on keep the
  precision of their input.

The result's precision is missing only if it is missing for all inputs.
Logical operators (`&`, `|`, `!`,
[`any()`](https://rdrr.io/r/base/any.html),
[`all()`](https://rdrr.io/r/base/all.html)) are not supported, and
[`is.na()`](https://rdrr.io/r/base/NA.html),
[`is.nan()`](https://rdrr.io/r/base/is.finite.html),
[`is.finite()`](https://rdrr.io/r/base/is.finite.html) and
[`is.infinite()`](https://rdrr.io/r/base/is.finite.html) give logical
vectors based only on the value.

Base R finds methods for functions like
[`sum()`](https://rdrr.io/r/base/sum.html) with their first argument, so
use `sum(x, 1)` instead of `sum(1, x)`.

## Examples

``` r
sigfig(3.2, 1) + sigfig(4.12, 2)
#> <sigfig[1]>
#> [1] 7.3
sigfig(3.2, 1) * 2
#> <sigfig[1]>
#> [1] 6.4
sum(sigfig(c(1.234, 5.6), c(3, 1)))
#> <sigfig[1]>
#> [1] 6.8
cumsum(sigfig(c(1.234, 5.6, 7.89), c(3, 1, 2)))
#> <sigfig[3]>
#> [1] 1.234   6.8  14.7
round(sigfig(3.14159, 5), 2)
#> <sigfig[1]>
#> [1] 3.14
```
