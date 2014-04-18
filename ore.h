#ifndef _ORE_H_
#include "mpc.h"
#include "khash.h"
#include "klist.h"
#include "kstring.h"

typedef enum {
  ORE_TYPE_NIL,
  ORE_TYPE_BOOL,
  ORE_TYPE_INT,
  ORE_TYPE_FLOAT,
  ORE_TYPE_STRING,
  ORE_TYPE_ARRAY,
  ORE_TYPE_HASH,
  ORE_TYPE_FUNC,
  ORE_TYPE_CFUNC,
  ORE_TYPE_ENV,
  ORE_TYPE_CLASS,
  ORE_TYPE_CCLASS,
  ORE_TYPE_OBJECT,
} ore_type;

typedef enum {
  ORE_ERROR_NONE,
  ORE_ERROR_EXCEPTION,
  ORE_ERROR_RETURN,
  ORE_ERROR_BREAK,
  ORE_ERROR_CONTINUE,
} ore_error;

typedef struct _ore_string {
  char* p;
  int l;
  int ref;
} ore_string;

typedef struct _ore_array {
  void* p;
  int ref;
} ore_array;

typedef struct _ore_hash {
  void* p;
  int ref;
} ore_hash;

typedef struct _ore_func {
  void* ore;
  int num_in;
  union {
    void* c;
    void* o;
  } x;
  void* u;
} ore_func;

typedef struct _ore_env {
  void* p;
  int ref;
} ore_env;

typedef struct _ore_class {
  const char* n;
  void* t;
} ore_class;

typedef struct _ore_cclass {
  const char* n;
  void* e;
} ore_cclass;

typedef struct _ore_object {
  ore_type t;
  void* c;
  void* e;
  int ref;
} ore_object;

typedef struct _ore_value {
  ore_type t;
  union {
    int b;
    int i;
    double d;
    ore_string* s;
    ore_array* a;
    ore_hash* h;
    ore_func f;
    ore_env* e;
    ore_class* c;
    ore_cclass* x;
    ore_object* o;
  } v;
} ore_value;

void ore_value_free(void* p);

KHASH_MAP_INIT_STR(value, ore_value)
KLIST_INIT(value, ore_value, ore_value_free)

typedef struct _ore_context {
  khash_t(value)* env;
  struct _ore_context* parent;
  khash_t(value)* ct;
  void* c;
  int err;
} ore_context;

typedef ore_value (*ore_cfunc_t)(ore_context*, int, ore_value*, void*);

ore_context* ore_new(ore_context*);
void ore_destroy(ore_context*);

void ore_define(ore_context*, const char*, ore_value);
void ore_define_cfunc(ore_context*, const char*, int, ore_cfunc_t, void*);
void ore_set(ore_context*, const char*, ore_value);
ore_value ore_prop(ore_context*, const char*);
ore_value ore_get(ore_context*, const char*);
ore_value ore_func_call(ore_context*, ore_value, int, ore_value*);

ore_value orex_define_class(ore_context*, const char*);
void orex_define_method(ore_context*, ore_value, const char*, int, ore_cfunc_t, void*);
#endif /* _ORE_H_ */
