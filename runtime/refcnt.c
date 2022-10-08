#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include <mimalloc.h>
#include <assert.h>


#if defined(__GNUC__) || defined(__clang__)
#define rc_unlikely(x)     (__builtin_expect(!!(x),false))
#define rc_likely(x)       (__builtin_expect(!!(x),true))
#elif (defined(__cplusplus) && (__cplusplus >= 202002L)) || (defined(_MSVC_LANG) && _MSVC_LANG >= 202002L)
#define rc_unlikely(x)     (x) [[unlikely]]
#define rc_likely(x)       (x) [[likely]]
#else
#define rc_unlikely(x)     (x)
#define rc_likely(x)       (x)
#endif

#define RC_STUCK          (INT32_MIN)                 /* 0x80000000 */
#define RC_STICKY         (RC_STUCK + 0x10000000)     /* 0x90000000 */
#define RC_STICKY_DROP    (RC_STICKY + 0x10000000)    /* 0xA0000000 */
#define RC_SHARED_UNIQUE  (-1)

void* rc_alloc(mlsize_t num_bytes) {
  void *res = mi_malloc(num_bytes);
  if (res == NULL) {
    fprintf(stderr, "rc: malloc returned null\n");
    exit(-1);
  }
  return res;
}

void* rc_mi_heap_alloc( mi_heap_t* heap, mlsize_t n ) {
  return mi_heap_malloc(heap, n);
}

extern void rc_drop_free( value v );

// `drop_checked` is called for objects whose reference count is 0 (unique) or negative (atomic)
// noinline
static inline void rc_drop_checked( value v, int32_t rc ) {
  assert(Is_block(v));
  if rc_likely(rc == 0) {
    // free if this was the last reference
    rc_drop_free(v);
  }
  else if (rc <= RC_STICKY_DROP) {
    // nothing
  }
  else {    
    // todo: atomic drop
    Refcnt_val(v) = rc - 1;
  }
}

// Drop a value
static inline void rc_drop( value v ) {
  int32_t rc;
  if (Is_long(v)) return;
  rc = Refcnt_val(v);
  if rc_likely(rc <= 0) {
    rc_drop_checked(v,rc);
  }
  else {
    Refcnt_val(v) = rc - 1;
  }
}

// Recursively drop the children and free the object. `v` must be a pointer.
void rc_drop_free( value v ) {
  tag_t t;
  assert(Is_block(v));
  t = Tag_val(v);
  if rc_likely(t < No_scan_tag) {
    mlsize_t first_field;
    mlsize_t wsize = Wosize_val(v);
    if rc_unlikely(t == Closure_tag) {
      first_field = Start_env_closinfo(Closinfo_val(v));
    } else {
      first_field = 0;
    }
    for(mlsize_t i = first_field; i < wsize; i++) {
      rc_drop(Field(v,i));
    }
  }
  mi_free(Hp_val(v));
}



