#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include <mimalloc.h>

mlsize_t *rc_alloc(mlsize_t num_bytes) {
  void *res = mi_malloc(num_bytes);
  if (res == NULL) {
    fprintf(stderr, "rc: malloc returned null\n");
    exit(-1);
  }
  return (mlsize_t*) res;
}
