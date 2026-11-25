#include <math.h>
#include <limits.h>
#include <string.h>
#include "rle.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>

/*
 * An ALTREP integer vector stored as a run table.
 *
 *  data1: VECSXP (list) length 2, immutable and possibly shared
 *      [0]: INTSXP, the value of each run (adjacent runs always differ)
 *      [1]: INTSXP (or REALSXP for long vectors), cumulative run ends
 *  data2: expanded INTSXP, or R_NilValue until materialised
 *
 * A writable Dataptr may be mutated at any later time, so vending one drops
 * data1 (set to R_NilValue) and makes data2 authoritative: the vector is then
 * 'stale'. A read-only Dataptr caches data2 but retains the run table.
 *
 * ALTREP methods must not check for user interrupts, since R disables the
 * garbage collector around some of them and a longjmp would leave it disabled.
 */

static R_altrep_class_t rle_class;

/* Run tables are only used if they need less than about half of the memory. */
#define RLE_MIN_SIZE 64
#define RLE_MIN_RATIO 4

#define IS_RLE(x)      (ALTREP(x) && R_altrep_inherits(x, rle_class))
#define RLE_TABLE(x)   R_altrep_data1(x)
#define RLE_DENSE(x)   R_altrep_data2(x)
#define IS_STALE(x)    (RLE_TABLE(x) == R_NilValue)

Rboolean rle_is_compact(SEXP x) {
  return (Rboolean) (IS_RLE(x) && !IS_STALE(x));
}

static rle_table rle_get_table(SEXP x) {
  SEXP d = RLE_TABLE(x);
  SEXP values = VECTOR_ELT(d, 0), ends = VECTOR_ELT(d, 1);
  rle_table t;
  t.k = XLENGTH(values);
  t.values = INTEGER_RO(values);
  t.iends = TYPEOF(ends) == INTSXP ? INTEGER_RO(ends) : NULL;
  t.dends = t.iends ? NULL : REAL_RO(ends);
  return t;
}

/* The run containing (0-based) element i */
static R_xlen_t rle_find(const rle_table *t, R_xlen_t i) {
  R_xlen_t lo = 0, hi = t->k - 1;
  while (lo < hi) {
    R_xlen_t mid = lo + (hi - lo) / 2;
    if (rle_end(t, mid) > i) hi = mid; else lo = mid + 1;
  }
  return lo;
}

/* Write elements [start, start + n) of the table to buf */
static void rle_fill(const rle_table *t, R_xlen_t start, R_xlen_t n, int *buf) {
  if (n <= 0) return;
  R_xlen_t i = start, stop = start + n;
  for (R_xlen_t j = rle_find(t, start); i < stop; j++) {
    R_xlen_t end = rle_end(t, j);
    if (end > stop) end = stop;
    int v = t->values[j];
    for (; i < end; i++) *buf++ = v;
  }
}

void rle_iter_init(rle_iter *it, SEXP x) {
  if (TYPEOF(x) != INTSXP) error("expected an integer vector");
  it->n = XLENGTH(x);
  it->pos = 0;
  it->j = 0;
  if (rle_is_compact(x)) {
    it->tab = rle_get_table(x);
    it->dense = NULL;
  } else {
    memset(&it->tab, 0, sizeof(rle_table));
    it->dense = INTEGER_RO(x);
  }
}

/* ── run builder ────────────────────────────────────────────────────────── */

/*
 * Accumulates runs of a vector of known size. Switches to an ordinary integer
 * vector as soon as there are too many runs to be worth compressing.
 */
typedef struct {
  R_xlen_t n, pos, k, cap, max_k;
  int *val;
  R_xlen_t *end;
  int *dense;
  SEXP out;
  PROTECT_INDEX ipx;
  const void *vmax;
} rle_builder;

/* Leaves one object on the protect stack. */
static void builder_init(rle_builder *b, R_xlen_t n, int force) {
  b->n = n;
  b->pos = b->k = b->cap = 0;
  b->max_k = force ? n : (n >= RLE_MIN_SIZE ? n / RLE_MIN_RATIO : 0);
  b->val = NULL;
  b->end = NULL;
  b->dense = NULL;
  b->out = R_NilValue;
  b->vmax = vmaxget();
  PROTECT_WITH_INDEX(b->out, &b->ipx);
}

static void builder_to_dense(rle_builder *b) {
  REPROTECT(b->out = allocVector(INTSXP, b->n), b->ipx);
  b->dense = INTEGER(b->out);
  R_xlen_t i = 0;
  for (R_xlen_t j = 0; j < b->k; j++)
    for (; i < b->end[j]; i++) b->dense[i] = b->val[j];
}

static void builder_grow(rle_builder *b) {
  R_xlen_t cap = b->cap ? b->cap * 2 : 16;
  if (cap > b->max_k) cap = b->max_k;
  int *val = (int*) R_alloc(cap, sizeof(int));
  R_xlen_t *end = (R_xlen_t*) R_alloc(cap, sizeof(R_xlen_t));
  if (b->k) {
    memcpy(val, b->val, b->k * sizeof(int));
    memcpy(end, b->end, b->k * sizeof(R_xlen_t));
  }
  b->val = val;
  b->end = end;
  b->cap = cap;
}

static R_INLINE void builder_add(rle_builder *b, int v, R_xlen_t len) {
  if (len <= 0) return;
  if (len > b->n - b->pos) error("internal error: run table exceeds its size");
  if (!b->dense) {
    if (b->k && b->val[b->k - 1] == v) {
      b->end[b->k - 1] = (b->pos += len);
      return;
    }
    if (b->k < b->max_k) {
      if (b->k == b->cap) builder_grow(b);
      b->val[b->k] = v;
      b->end[b->k] = (b->pos += len);
      b->k++;
      return;
    }
    builder_to_dense(b);
  }
  int *d = b->dense + b->pos;
  for (R_xlen_t i = 0; i < len; i++) d[i] = v;
  b->pos += len;
}

static SEXP new_rle(SEXP values, SEXP ends) {
  SEXP data = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(data, 0, values);
  SET_VECTOR_ELT(data, 1, ends);
  MARK_NOT_MUTABLE(values);
  MARK_NOT_MUTABLE(ends);
  MARK_NOT_MUTABLE(data);
  SEXP out = R_new_altrep(rle_class, data, R_NilValue);
  UNPROTECT(1);
  return out;
}

/* The result is unprotected: the builder's slot remains on the stack. */
static SEXP builder_finish(rle_builder *b, int force) {
  if (b->pos != b->n) error("internal error: incomplete run table");
  if (!b->dense) {
    if (b->n == 0 && !force) {
      REPROTECT(b->out = allocVector(INTSXP, 0), b->ipx);
    } else {
      SEXP values = PROTECT(allocVector(INTSXP, b->k));
      SEXP ends = PROTECT(allocVector(b->n > INT_MAX ? REALSXP : INTSXP, b->k));
      if (b->k) memcpy(INTEGER(values), b->val, b->k * sizeof(int));
      if (TYPEOF(ends) == INTSXP) {
        int *e = INTEGER(ends);
        for (R_xlen_t j = 0; j < b->k; j++) e[j] = (int) b->end[j];
      } else {
        double *e = REAL(ends);
        for (R_xlen_t j = 0; j < b->k; j++) e[j] = (double) b->end[j];
      }
      REPROTECT(b->out = new_rle(values, ends), b->ipx);
      UNPROTECT(2);
    }
  }
  vmaxset(b->vmax);
  return b->out;
}

/* ── ALTREP methods ─────────────────────────────────────────────────────── */

static R_xlen_t rle_Length(SEXP x) {
  if (RLE_DENSE(x) != R_NilValue) return XLENGTH(RLE_DENSE(x));
  rle_table t = rle_get_table(x);
  return t.k ? rle_end(&t, t.k - 1) : 0;
}

static SEXP rle_Duplicate(SEXP x, Rboolean deep) {
  if (IS_STALE(x)) return duplicate(RLE_DENSE(x));
  /* The run table is immutable, so it can be shared */
  return R_new_altrep(rle_class, RLE_TABLE(x), R_NilValue);
}

static SEXP rle_Serialized_state(SEXP x) {
  /* NULL serialises as an ordinary integer */
  return IS_STALE(x) ? NULL : RLE_TABLE(x);
}

static SEXP rle_Unserialize(SEXP cls, SEXP state) {
  /* States could come from anywhere, so check all invariants */
  if (TYPEOF(state) != VECSXP || XLENGTH(state) != 2)
    error("invalid serialised precision");
  SEXP values = VECTOR_ELT(state, 0), ends = VECTOR_ELT(state, 1);
  if (TYPEOF(values) != INTSXP ||
      (TYPEOF(ends) != INTSXP && TYPEOF(ends) != REALSXP) ||
      XLENGTH(values) != XLENGTH(ends))
    error("invalid serialised precision");
  double prev = 0;
  for (R_xlen_t j = 0; j < XLENGTH(values); j++) {
    double e = TYPEOF(ends) == INTSXP ? INTEGER_ELT(ends, j) : REAL_ELT(ends, j);
    if (!(e > prev) || e > (double) R_XLEN_T_MAX || e != (double) (R_xlen_t) e ||
        (j && INTEGER_ELT(values, j) == INTEGER_ELT(values, j - 1)))
      error("invalid serialised precision");
    prev = e;
  }
  return new_rle(values, ends);
}

static Rboolean rle_Inspect(SEXP x, int pre, int deep, int pvec,
                            void (*inspect_subtree)(SEXP, int, int, int)) {
  if (IS_STALE(x)) {
    Rprintf(" precision_rle [len: %.0f, stale]\n", (double) XLENGTH(x));
  } else {
    rle_table t = rle_get_table(x);
    Rprintf(" precision_rle [len: %.0f, runs: %.0f, %s]\n",
            (double) XLENGTH(x), (double) t.k,
            RLE_DENSE(x) == R_NilValue ? "compact" : "materialised");
  }
  return TRUE;
}

static void *rle_Dataptr(SEXP x, Rboolean writable) {
  if (RLE_DENSE(x) == R_NilValue) {
    R_xlen_t n = XLENGTH(x);
    SEXP dense = PROTECT(allocVector(INTSXP, n));
    rle_table t = rle_get_table(x);
    rle_fill(&t, 0, n, INTEGER(dense));
    R_set_altrep_data2(x, dense);
    UNPROTECT(1);
  }
  if (writable && !IS_STALE(x)) R_set_altrep_data1(x, R_NilValue);
  return (void*) INTEGER(RLE_DENSE(x));
}

static const void *rle_Dataptr_or_null(SEXP x) {
  SEXP dense = RLE_DENSE(x);
  return dense == R_NilValue ? NULL : (const void*) INTEGER_RO(dense);
}

static int rle_Elt(SEXP x, R_xlen_t i) {
  if (RLE_DENSE(x) != R_NilValue) return INTEGER_ELT(RLE_DENSE(x), i);
  rle_table t = rle_get_table(x);
  return t.values[rle_find(&t, i)];
}

static R_xlen_t rle_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  R_xlen_t size = XLENGTH(x);
  if (i < 0 || i >= size || n <= 0) return 0;
  if (n > size - i) n = size - i;
  if (RLE_DENSE(x) != R_NilValue) {
    memcpy(buf, INTEGER_RO(RLE_DENSE(x)) + i, n * sizeof(int));
  } else {
    rle_table t = rle_get_table(x);
    rle_fill(&t, i, n, buf);
  }
  return n;
}

static int rle_No_NA(SEXP x) {
  if (IS_STALE(x)) return 0;
  rle_table t = rle_get_table(x);
  for (R_xlen_t j = 0; j < t.k; j++)
    if (t.values[j] == NA_INTEGER) return 0;
  return 1;
}

static int rle_Is_sorted(SEXP x) {
  if (!rle_No_NA(x)) return UNKNOWN_SORTEDNESS;
  rle_table t = rle_get_table(x);
  int incr = 1, decr = 1;
  for (R_xlen_t j = 1; j < t.k; j++) {
    if (t.values[j] > t.values[j - 1]) decr = 0; else incr = 0;
  }
  if (incr) return SORTED_INCR;
  if (decr) return SORTED_DECR;
  return KNOWN_UNSORTED;
}

/* NULL defers to R's usual method, giving its warnings (empty, overflow) */
static SEXP rle_Sum(SEXP x, Rboolean narm) {
  if (IS_STALE(x)) return NULL;
  rle_table t = rle_get_table(x);
  const double limit = 4503599627370496.0; /* 2^52: sums are exact below this */
  double sum = 0;
  R_xlen_t start = 0;
  for (R_xlen_t j = 0; j < t.k; j++) {
    R_xlen_t end = rle_end(&t, j);
    if (t.values[j] == NA_INTEGER) {
      if (!narm) return ScalarInteger(NA_INTEGER);
    } else {
      double add = (double) t.values[j] * (double) (end - start);
      sum += add;
      if (fabs(add) > limit || fabs(sum) > limit) return NULL;
    }
    start = end;
  }
  if (fabs(sum) > INT_MAX) return NULL;
  return ScalarInteger((int) sum);
}

static SEXP rle_extreme(SEXP x, Rboolean narm, int sign) {
  if (IS_STALE(x)) return NULL;
  rle_table t = rle_get_table(x);
  int found = 0, out = 0;
  for (R_xlen_t j = 0; j < t.k; j++) {
    int v = t.values[j];
    if (v == NA_INTEGER) {
      if (!narm) return ScalarInteger(NA_INTEGER);
    } else if (!found || (sign > 0 ? v > out : v < out)) {
      out = v;
      found = 1;
    }
  }
  return found ? ScalarInteger(out) : NULL;
}

static SEXP rle_Min(SEXP x, Rboolean narm) { return rle_extreme(x, narm, -1); }
static SEXP rle_Max(SEXP x, Rboolean narm) { return rle_extreme(x, narm, 1); }

static SEXP rle_Extract_subset(SEXP x, SEXP indx, SEXP call) {
  if (IS_STALE(x)) return NULL;
  int type = TYPEOF(indx);
  if (type != INTSXP && type != REALSXP) return NULL;

  enum { BUF = 512 };
  int ibuf[BUF];
  double dbuf[BUF];
  R_xlen_t m = XLENGTH(indx), size = XLENGTH(x);
  rle_table t = rle_get_table(x);
  R_xlen_t run = 0;

  rle_builder b;
  builder_init(&b, m, 0);
  for (R_xlen_t at = 0; at < m; ) {
    R_xlen_t got = m - at < BUF ? m - at : BUF;
    if (type == INTSXP) got = INTEGER_GET_REGION(indx, at, got, ibuf);
    else got = REAL_GET_REGION(indx, at, got, dbuf);
    if (got <= 0) error("internal error: could not read the subscript");
    for (R_xlen_t s = 0; s < got; s++) {
      R_xlen_t i = -1;
      if (type == INTSXP) {
        if (ibuf[s] != NA_INTEGER) i = (R_xlen_t) ibuf[s] - 1;
      } else if (R_FINITE(dbuf[s]) && dbuf[s] >= 1 && dbuf[s] < size + 1.0) {
        i = (R_xlen_t) dbuf[s] - 1;
      }
      int v = NA_INTEGER;
      if (i >= 0 && i < size) {
        /* Stay in the same run, or step to the next, else search */
        if (i >= rle_end(&t, run) || (run && i < rle_end(&t, run - 1))) {
          if (run + 1 < t.k && i >= rle_end(&t, run) && i < rle_end(&t, run + 1))
            run++;
          else
            run = rle_find(&t, i);
        }
        v = t.values[run];
      }
      builder_add(&b, v, 1);
    }
    at += got;
  }
  SEXP out = builder_finish(&b, 0);
  UNPROTECT(1);
  return out;
}

void rle_init_class(DllInfo *dll) {
  rle_class = R_make_altinteger_class("precision_rle", "precision", dll);

  R_set_altrep_Length_method(rle_class, rle_Length);
  R_set_altrep_Duplicate_method(rle_class, rle_Duplicate);
  R_set_altrep_Serialized_state_method(rle_class, rle_Serialized_state);
  R_set_altrep_Unserialize_method(rle_class, rle_Unserialize);
  R_set_altrep_Inspect_method(rle_class, rle_Inspect);

  R_set_altvec_Dataptr_method(rle_class, rle_Dataptr);
  R_set_altvec_Dataptr_or_null_method(rle_class, rle_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(rle_class, rle_Extract_subset);

  R_set_altinteger_Elt_method(rle_class, rle_Elt);
  R_set_altinteger_Get_region_method(rle_class, rle_Get_region);
  R_set_altinteger_Is_sorted_method(rle_class, rle_Is_sorted);
  R_set_altinteger_No_NA_method(rle_class, rle_No_NA);
  R_set_altinteger_Sum_method(rle_class, rle_Sum);
  R_set_altinteger_Min_method(rle_class, rle_Min);
  R_set_altinteger_Max_method(rle_class, rle_Max);
}

/* ── kernels ────────────────────────────────────────────────────────────── */

#define CHECK_INTERRUPT(i) \
  do { if (((++(i)) & 0xFFFF) == 0) R_CheckUserInterrupt(); } while (0)

static void check_int(SEXP x, const char *arg) {
  if (TYPEOF(x) != INTSXP) error("`%s` must be an integer vector", arg);
}

/* NA_INTEGER is INT_MIN, so needs explicit handling to be ignored by min() */
static R_INLINE int coarsest(int a, int b) {
  if (a == NA_INTEGER) return b;
  if (b == NA_INTEGER) return a;
  return a < b ? a : b;
}

SEXP rle_compress(SEXP x) {
  check_int(x, "x");
  if (rle_is_compact(x)) return x;
  rle_iter it;
  rle_iter_init(&it, x);

  /* Avoid copying an ordinary vector which cannot be compressed */
  if (!ALTREP(x)) {
    if (it.n < RLE_MIN_SIZE) return x;
    R_xlen_t k = 0, max_k = it.n / RLE_MIN_RATIO, tick = 0;
    int v;
    while (k <= max_k && rle_iter_next(&it, &v)) { k++; CHECK_INTERRUPT(tick); }
    if (k > max_k) return x;
    rle_iter_init(&it, x);
  }

  rle_builder b;
  builder_init(&b, it.n, 0);
  R_xlen_t tick = 0, start = 0;
  int v;
  while (rle_iter_next(&it, &v)) {
    builder_add(&b, v, it.pos - start);
    start = it.pos;
    CHECK_INTERRUPT(tick);
  }
  SEXP out = builder_finish(&b, 0);
  UNPROTECT(1);
  return out;
}

static R_xlen_t get_length(SEXP x, R_xlen_t i) {
  double len;
  if (TYPEOF(x) == INTSXP) {
    int v = INTEGER_ELT(x, i);
    len = v == NA_INTEGER ? -1 : v;
  } else {
    len = REAL_ELT(x, i);
  }
  if (!(len >= 0) || len > (double) R_XLEN_T_MAX || len != (double) (R_xlen_t) len)
    error("run lengths must be non-negative whole numbers");
  return (R_xlen_t) len;
}

static R_xlen_t get_size(SEXP x) {
  if ((TYPEOF(x) != INTSXP && TYPEOF(x) != REALSXP) || XLENGTH(x) != 1)
    error("`size` must be a single number");
  return get_length(x, 0);
}

SEXP rle_make(SEXP values, SEXP lengths, SEXP force) {
  check_int(values, "values");
  if (TYPEOF(lengths) != INTSXP && TYPEOF(lengths) != REALSXP)
    error("`lengths` must be numeric");
  R_xlen_t k = XLENGTH(values), n = 0, tick = 0;
  if (XLENGTH(lengths) != k) error("`values` and `lengths` must be the same size");
  for (R_xlen_t j = 0; j < k; j++) {
    R_xlen_t len = get_length(lengths, j);
    if (len > R_XLEN_T_MAX - n) error("the result would be too long");
    n += len;
  }
  int f = asLogical(force) == TRUE;
  rle_builder b;
  builder_init(&b, n, f);
  for (R_xlen_t j = 0; j < k; j++) {
    builder_add(&b, INTEGER_ELT(values, j), get_length(lengths, j));
    CHECK_INTERRUPT(tick);
  }
  SEXP out = builder_finish(&b, f);
  UNPROTECT(1);
  return out;
}

/* list(values, lengths) */
SEXP rle_runs(SEXP x) {
  check_int(x, "x");
  rle_iter it;
  rle_iter_init(&it, x);
  R_xlen_t k = 0, tick = 0;
  int v;
  if (it.dense) {
    while (rle_iter_next(&it, &v)) { k++; CHECK_INTERRUPT(tick); }
    rle_iter_init(&it, x);
  } else {
    k = it.tab.k;
  }
  SEXP out = PROTECT(allocVector(VECSXP, 2));
  SEXP values = SET_VECTOR_ELT(out, 0, allocVector(INTSXP, k));
  SEXP lengths = SET_VECTOR_ELT(
    out, 1, allocVector(it.n > INT_MAX ? REALSXP : INTSXP, k)
  );
  int *pv = INTEGER(values);
  int *pi = TYPEOF(lengths) == INTSXP ? INTEGER(lengths) : NULL;
  double *pd = pi ? NULL : REAL(lengths);
  R_xlen_t start = 0;
  for (R_xlen_t j = 0; j < k && rle_iter_next(&it, &v); j++) {
    pv[j] = v;
    if (pi) pi[j] = (int) (it.pos - start); else pd[j] = (double) (it.pos - start);
    start = it.pos;
    CHECK_INTERRUPT(tick);
  }
  UNPROTECT(1);
  return out;
}

/* A new ordinary integer vector, leaving x unmaterialised */
SEXP rle_expand(SEXP x) {
  check_int(x, "x");
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(allocVector(INTSXP, n));
  int *d = INTEGER(out);
  if (rle_is_compact(x)) {
    rle_table t = rle_get_table(x);
    rle_fill(&t, 0, n, d);
  } else if (n) {
    memcpy(d, INTEGER_RO(x), n * sizeof(int));
  }
  UNPROTECT(1);
  return out;
}

/* A new vector sharing the run table (if compact), which can be materialised or
 * modified without affecting x */
SEXP rle_fresh(SEXP x) {
  check_int(x, "x");
  if (!rle_is_compact(x)) return x;
  return R_new_altrep(rle_class, RLE_TABLE(x), R_NilValue);
}

/* For tests: how is this vector stored? */
SEXP rle_state(SEXP x) {
  if (!IS_RLE(x)) return mkString("ordinary");
  if (IS_STALE(x)) return mkString("stale");
  return mkString(RLE_DENSE(x) == R_NilValue ? "compact" : "materialised");
}

/* For tests: write to x in place (with a 1-based index) via a writable pointer */
SEXP rle_touch(SEXP x, SEXP i, SEXP value) {
  check_int(x, "x");
  INTEGER(x)[asInteger(i) - 1] = asInteger(value);
  return x;
}

/* For tests: obtain (and discard) a read-only pointer */
SEXP rle_peek(SEXP x) {
  check_int(x, "x");
  (void) INTEGER_RO(x);
  return x;
}

/* Elementwise coarsest, where x and y are each of size 1 or `size` */
SEXP rle_coarsest2(SEXP x, SEXP y, SEXP size) {
  check_int(x, "x");
  check_int(y, "y");
  R_xlen_t n = get_size(size), nx = XLENGTH(x), ny = XLENGTH(y);
  if ((nx != n && nx != 1) || (ny != n && ny != 1))
    error("can't recycle precision");
  if (x == y && nx == n && rle_is_compact(x)) return x;

  rle_iter ix, iy;
  rle_iter_init(&ix, x);
  rle_iter_init(&iy, y);
  rle_builder b;
  builder_init(&b, n, 0);

  R_xlen_t pos = 0, endx = 0, endy = 0, tick = 0;
  int vx = NA_INTEGER, vy = NA_INTEGER;
  if (nx == 1) { vx = INTEGER_ELT(x, 0); endx = n; }
  if (ny == 1) { vy = INTEGER_ELT(y, 0); endy = n; }
  while (pos < n) {
    if (endx == pos) { rle_iter_next(&ix, &vx); endx = ix.pos; }
    if (endy == pos) { rle_iter_next(&iy, &vy); endy = iy.pos; }
    R_xlen_t end = endx < endy ? endx : endy;
    builder_add(&b, coarsest(vx, vy), end - pos);
    pos = end;
    CHECK_INTERRUPT(tick);
  }
  SEXP out = builder_finish(&b, 0);
  UNPROTECT(1);
  return out;
}

SEXP rle_coarsest(SEXP x) {
  check_int(x, "x");
  rle_iter it;
  rle_iter_init(&it, x);
  int v, out = NA_INTEGER;
  R_xlen_t tick = 0;
  while (rle_iter_next(&it, &v)) {
    out = coarsest(out, v);
    CHECK_INTERRUPT(tick);
  }
  return ScalarInteger(out);
}

/* Cumulative coarsest, or cap each run */
static SEXP rle_map(SEXP x, int cap, int cumulative) {
  check_int(x, "x");
  rle_iter it;
  rle_iter_init(&it, x);
  rle_builder b;
  builder_init(&b, it.n, 0);
  R_xlen_t start = 0, tick = 0;
  int v, acc = NA_INTEGER;
  while (rle_iter_next(&it, &v)) {
    if (cumulative) v = acc = coarsest(acc, v);
    else if (v != NA_INTEGER && v > cap) v = cap;
    builder_add(&b, v, it.pos - start);
    start = it.pos;
    CHECK_INTERRUPT(tick);
  }
  SEXP out = builder_finish(&b, 0);
  UNPROTECT(1);
  return out;
}

SEXP rle_cumcoarsest(SEXP x) { return rle_map(x, 0, 1); }

/* No finer than cap, leaving NA unchanged */
SEXP rle_cap(SEXP x, SEXP cap) { return rle_map(x, asInteger(cap), 0); }
