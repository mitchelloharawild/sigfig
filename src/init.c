#include "rle.h"
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef call_entries[] = {
  CALLDEF(rle_compress, 1),
  CALLDEF(rle_make, 3),
  CALLDEF(rle_runs, 1),
  CALLDEF(rle_expand, 1),
  CALLDEF(rle_fresh, 1),
  CALLDEF(rle_state, 1),
  CALLDEF(rle_touch, 3),
  CALLDEF(rle_peek, 1),
  CALLDEF(rle_coarsest2, 3),
  CALLDEF(rle_coarsest, 1),
  CALLDEF(rle_cumcoarsest, 1),
  CALLDEF(rle_cap, 2),
  CALLDEF(prec_round, 3),
  CALLDEF(prec_format, 2),
  {NULL, NULL, 0}
};

void R_init_precision(DllInfo *dll) {
  rle_init_class(dll);
  round_init();
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
