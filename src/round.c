#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rle.h"

/*
 * Rounding and formatting share round_dp(), so that values displaying equally
 * at the same (known) precision always compare equal.
 */

#define MAX_POW 308
/* Doubles have no more than 1074 decimal places */
#define MAX_DP 1100
#define BUF_SIZE (MAX_DP + 2 * MAX_POW + 32)

static double pow10_tab[MAX_POW + 1];

void round_init(void) {
  char buf[16];
  for (int i = 0; i <= MAX_POW; i++) {
    snprintf(buf, sizeof(buf), "1e%d", i);
    pow10_tab[i] = strtod(buf, NULL);
  }
}

/* The nearer candidate to x (non-negative), or the even one when equidistant */
static R_INLINE double nearest(double x, double lo, double hi, double lo_int) {
  if (!R_FINITE(hi)) return lo;
  double down = x - lo, up = hi - x;
  if (down < up || (down == up && fmod(lo_int, 2.0) == 0)) return lo;
  return hi;
}

static double round_dp(double x, int p) {
  /* + 0.0 removes negative zeros */
  if (p == NA_INTEGER || !R_FINITE(x) || x == 0) return ISNAN(x) ? x : x + 0.0;
  if (p == 0) return nearbyint(x) + 0.0;

  double ax = fabs(x), r;
  if (p < 0) {
    if (p < -MAX_POW) return 0.0;
    double unit = pow10_tab[-p], q = ax / unit;
    /* As many digits as a double can hold */
    if (q >= 1e15) return x;
    double lo = floor(q);
    r = nearest(ax, lo * unit, ceil(q) * unit, lo);
  } else {
    double q = p <= 22 ? ax * pow10_tab[p] : 0;
    if (p <= 22 && q < 1e14) {
      /* Powers of ten up to 1e22 are exact, so the candidates are the closest
       * doubles to their decimal representations. Same as round() in R >= 4. */
      double lo = floor(q);
      r = nearest(ax, lo / pow10_tab[p], ceil(q) / pow10_tab[p], lo);
    } else if (p <= 22 && q >= 1e16) {
      return x;
    } else {
      /* At the limits of double precision use the displayed value itself. */
      char buf[BUF_SIZE];
      snprintf(buf, sizeof(buf), "%.*f", p > MAX_DP ? MAX_DP : p, ax);
      r = strtod(buf, NULL);
    }
  }
  return x < 0 ? 0.0 - r : r;
}

/* Round x to the known precision p, each being of size 1 or `size` */
SEXP prec_round(SEXP x, SEXP p, SEXP size) {
  if (TYPEOF(x) != REALSXP) error("`x` must be a double vector");
  R_xlen_t n = (R_xlen_t) asReal(size), nx = XLENGTH(x), np = XLENGTH(p);
  if ((nx != n && nx != 1) || (np != n && np != 1)) error("can't recycle");

  SEXP out = PROTECT(allocVector(REALSXP, n));
  double *po = REAL(out);
  const double *px = REAL_RO(x);
  rle_iter it;
  rle_iter_init(&it, p);
  R_xlen_t i = 0;
  while (i < n) {
    int v = NA_INTEGER;
    rle_iter_next(&it, &v);
    R_xlen_t end = np == 1 ? n : it.pos;
    if (nx == 1) {
      double r = round_dp(px[0], v);
      for (; i < end; i++) po[i] = r;
    } else {
      for (; i < end; i++) {
        if ((i & 0xFFFF) == 0) R_CheckUserInterrupt();
        po[i] = round_dp(px[i], v);
      }
    }
  }
  UNPROTECT(1);
  return out;
}

static void format_one(char *buf, double x, int p) {
  if (p == NA_INTEGER) {
    snprintf(buf, BUF_SIZE, "%.15g", x + 0.0);
    return;
  }
  double r = round_dp(x, p);
  if (p >= 0) {
    snprintf(buf, BUF_SIZE, "%.*f", p > MAX_DP ? MAX_DP : p, r);
  } else if (r == 0) {
    strcpy(buf, "0");
  } else {
    /* r != 0 implies that p >= -MAX_POW */
    double q = nearbyint(r / pow10_tab[-p]);
    if (fabs(q) < 1e15) {
      int len = snprintf(buf, BUF_SIZE, "%.0f", q);
      memset(buf + len, '0', -p);
      buf[len - p] = '\0';
    } else {
      snprintf(buf, BUF_SIZE, "%.0f", r);
    }
  }
}

/* Missing values are NA_STRING */
SEXP prec_format(SEXP x, SEXP p) {
  if (TYPEOF(x) != REALSXP) error("`x` must be a double vector");
  R_xlen_t n = XLENGTH(x), i = 0;
  if (XLENGTH(p) != n) error("`x` and `p` must be the same size");
  SEXP out = PROTECT(allocVector(STRSXP, n));
  const double *px = REAL_RO(x);
  char buf[BUF_SIZE];
  rle_iter it;
  rle_iter_init(&it, p);
  int v;
  while (rle_iter_next(&it, &v)) {
    for (; i < it.pos; i++) {
      if ((i & 0xFFF) == 0) R_CheckUserInterrupt();
      double d = px[i];
      if (R_IsNA(d)) {
        SET_STRING_ELT(out, i, NA_STRING);
      } else if (ISNAN(d)) {
        SET_STRING_ELT(out, i, mkChar("NaN"));
      } else if (!R_FINITE(d)) {
        SET_STRING_ELT(out, i, mkChar(d > 0 ? "Inf" : "-Inf"));
      } else {
        format_one(buf, d, v);
        SET_STRING_ELT(out, i, mkChar(buf));
      }
    }
  }
  UNPROTECT(1);
  return out;
}
