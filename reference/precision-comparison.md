# Comparing numbers with known precision

Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`) round both sides to their
coarsest known precision, and give a logical vector. Values which
display equally at the same precision are therefore always equal.
Ordinary numbers are exact, so use the precision of the number they are
compared with. Numbers are compared exactly when neither has known
precision.

## Details

Equality is not transitive when precision differs: `3.12` and `3.14`
both equal `3.1`, but not each other. So
[`unique()`](https://rdrr.io/r/base/unique.html),
[`duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`match()`](https://rdrr.io/r/base/match.html),
[`sort()`](https://rdrr.io/r/base/sort.html),
[`order()`](https://rdrr.io/r/base/order.html) and grouping instead use
each element's own rounded value *and* its precision: `3.1` and `3.10`
are distinct, with the coarser sorted first. All missing values are the
same, regardless of their precision.

[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html) and
[`range()`](https://rdrr.io/r/base/range.html) use the full value.

## Examples

``` r
precision(3.14159, 2) == 3.14
#> [1] TRUE
precision(3.12, 2) == precision(3.1, 1)
#> [1] TRUE
unique(precision(c(3.1, 3.1, 3.12), c(1, 2, 1)))
#> <precision[2]>
#> [1]  3.1 3.10
```
