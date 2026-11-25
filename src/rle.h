#ifndef SIGFIG_RLE_H
#define SIGFIG_RLE_H

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* A view of the run table of a compact (non-stale) RLE vector. */
typedef struct {
  R_xlen_t k;          /* number of runs */
  const int *values;
  const int *iends;    /* cumulative run ends: one of iends / dends is NULL */
  const double *dends;
} rle_table;

static R_INLINE R_xlen_t rle_end(const rle_table *t, R_xlen_t j) {
  return t->iends ? (R_xlen_t) t->iends[j] : (R_xlen_t) t->dends[j];
}

/* Is x an RLE vector whose run table can be trusted? */
Rboolean rle_is_compact(SEXP x);

/* Iterates over the runs of *any* integer vector: compact RLEs by their run
 * table, all other integers by scanning. The vector must stay protected. */
typedef struct {
  R_xlen_t n, pos;     /* total size, and start of the next run */
  rle_table tab;
  R_xlen_t j;
  const int *dense;    /* NULL when iterating over a run table */
} rle_iter;

void rle_iter_init(rle_iter *it, SEXP x);

/* Obtains the next run. Its end (exclusive) is it->pos. FALSE if finished. */
static R_INLINE Rboolean rle_iter_next(rle_iter *it, int *value) {
  if (it->pos >= it->n) return FALSE;
  if (it->dense) {
    R_xlen_t i = it->pos;
    int v = it->dense[i++];
    while (i < it->n && it->dense[i] == v) i++;
    *value = v;
    it->pos = i;
  } else {
    *value = it->tab.values[it->j];
    it->pos = rle_end(&it->tab, it->j);
    it->j++;
  }
  return TRUE;
}

void rle_init_class(DllInfo *dll);

SEXP rle_compress(SEXP x);
SEXP rle_make(SEXP values, SEXP lengths, SEXP force);
SEXP rle_runs(SEXP x);
SEXP rle_expand(SEXP x);
SEXP rle_fresh(SEXP x);
SEXP rle_state(SEXP x);
SEXP rle_touch(SEXP x, SEXP i, SEXP value);
SEXP rle_peek(SEXP x);
SEXP rle_coarsest2(SEXP x, SEXP y, SEXP size);
SEXP rle_coarsest(SEXP x);
SEXP rle_cumcoarsest(SEXP x);
SEXP rle_cap(SEXP x, SEXP cap);

void round_init(void);
SEXP prec_round(SEXP x, SEXP p, SEXP size);
SEXP prec_format(SEXP x, SEXP p);

#endif
