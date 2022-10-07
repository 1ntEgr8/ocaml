#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include <mimalloc.h>
#include <assert.h>

mlsize_t *rc_alloc(mlsize_t num_bytes) {
  void *res = mi_malloc(num_bytes);
  if (res == NULL) {
    fprintf(stderr, "rc: malloc returned null\n");
    exit(-1);
  }
  return (mlsize_t*) res;
}

// Recursively drop the children and free the object.
void rc_drop_free( value v );

// drop_checked is called for objects whose reference count is 0 (unique) or negative (atomic)
// noinline
static void rc_drop_checked( value v, int32_t rc ) {
  assert(Is_block(v));
  if (rc == 0) {
    // free if this was the last reference
    rc_drop_free(v);
  }
  else {
    // todo: atomic drop
    Refcnt_val(v) = rc - 1;
  }
}

// drop a value
static inline void rc_drop( value v ) {
  int32_t rc;
  if (Is_long(v)) return;
  rc = Refcnt_val(v);
  if (rc <= 0) {
    rc_drop_checked(v,rc);
  }
  else {
    Refcnt_val(v) = rc - 1;
  }
}

// drop the children and free the object
void rc_drop_free( value v ) {
  assert(Is_block(v));
  if (Tag_val(v) < No_scan_tag) {
    mlsize_t wsize = Wosize_val(v);
    for(mlsize_t i = 0; i < wsize; i++) {
      rc_drop(Field(v,i));
    }
  }
  mi_free(&Field(v,-1));
}