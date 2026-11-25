# sigfig: Numeric Vectors with Known Precision

A semantic data type for numbers with known precision. Each element
pairs a double precision value with a signed integer precision,
describing the number of decimal places (or with negative precision, the
powers of ten) to which the value is known. Values are displayed exactly
as described by their precision, and operations on vectors of differing
precision give results with the coarsest known precision (e.g. 3.2 +
4.12 is 7.3). Full values are retained in computation, so rounding error
does not accumulate. The precision is stored with run length encoding
using ALTREP (Alternative Representations for R Objects), allowing the
same precision to be repeated many times throughout a vector with
minimal memory use.

## See also

Useful links:

- <https://pkg.mitchelloharawild.com/sigfig/>

- <https://github.com/mitchelloharawild/sigfig>

- Report bugs at <https://github.com/mitchelloharawild/sigfig/issues>

## Author

**Maintainer**: Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
([ORCID](https://orcid.org/0000-0001-6729-7695))

Authors:

- Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
  ([ORCID](https://orcid.org/0000-0001-6729-7695))
