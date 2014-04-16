#include "mpc.h"
#include "khash.h"
#include "klist.h"

#define STRUCTURE \
"                                                                       \n" \
"number    : /-?[0-9]+(\\.[0-9]*)?(e[0-9]+)?/ ;                         \n" \
"factor    : '(' <lexp> ')'                                             \n" \
"          | <number>                                                   \n" \
"          | <string>                                                   \n" \
"          | <array>                                                    \n" \
"          | <hash>                                                     \n" \
"          | <ident> ;                                                  \n" \
"string    : /\"[^\"]*\"/ ;                                             \n" \
"item      : <factor> '[' <lexp> ']' ;                                  \n" \
"call      : <ident> '(' <lexp>? (',' <lexp>)* ')' ;                    \n" \
"anoncall  : <factor> '(' <lexp>? (',' <lexp>)* ')' ;                   \n" \
"array     : '[' <lexp>? (',' <lexp>)* ']' ;                            \n" \
"pair      : <string> ':' <lexp> ;                                      \n" \
"hash      : '{' <pair>? (',' <pair>)* '}' ;                            \n" \
"ident     : /[a-zA-Z][a-zA-Z0-9_]*/ ;                                  \n" \
"                                                                       \n" \
"term      : (<lambda> | <item> | <call> | <anoncall>                                " \
"        | <factor> (('*' | '/' | '%') <factor>)*) ;                    \n" \
"lexp      : <term> (('+' | '-') <term>)* ;                             \n" \
"let_v     : <ident> '=' <lexp> ';' ;                                   \n" \
"let_a     : <item> '=' <lexp> ';' ;                                    \n" \
"var       : \"var\" <ident> '=' <lexp> ';' ;                           \n" \
"vararg    : \"...\" ;                                                  \n" \
"stmts     : <stmt>* ;                                                  \n" \
"                                                                       \n" \
"lambda    : \"func\"                                                     " \
"        '(' <ident>? (<vararg> | (',' <ident>)*) ')' '{' <stmts> '}' ; \n" \
"func      : \"func\" <ident>                                             " \
"        '(' <ident>? (<vararg> | (',' <ident>)*) ')' '{' <stmts> '}' ; \n" \
"                                                                       \n" \
"return    : \"return\" <lexp> ';' ;                                    \n" \
"comment   : /#[^\n]*/ ;                                                \n" \
"eof       : /$/ ;                                                      \n" \
"stmt      : (<let_v> | <let_a> | <var> | (<lexp> ';')                    " \
"            | <func> | <return> | <comment>) ;                         \n" \
"program   : <stmts> <eof> ;                                            \n"

void ore_value_free(void* p);

#define is_a(t, a) (strstr(t->tag, a) != NULL)

typedef enum {
  ORE_TYPE_NIL,
  ORE_TYPE_INT,
  ORE_TYPE_FLOAT,
  ORE_TYPE_STR,
  ORE_TYPE_ARRAY,
  ORE_TYPE_HASH,
  ORE_TYPE_FUNC,
  ORE_TYPE_CFUNC
} ore_type;

typedef enum {
  ORE_ERROR_NONE,
  ORE_ERROR_EXCEPTION,
  ORE_ERROR_RETURN,
  ORE_ERROR_BREAK,
  ORE_ERROR_CONTINUE,
} ore_error;

typedef mpc_ast_t* ore_func;

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

typedef struct _ore_value {
  ore_type t;
  union {
    int i;
    double d;
    ore_string* s;
    ore_array* a;
    ore_hash* h;
    struct {
      void* env;
      int num_in;
      union {
        void* c;
        ore_func o;
      } x;
    } f;
  } v;
} ore_value;

void ore_value_ref(ore_value);
void ore_value_unref(ore_value);

KHASH_MAP_INIT_STR(value, ore_value)
KLIST_INIT(value, ore_value, ore_value_free)

typedef struct _ore_context {
  khash_t(value)* env;
  klist_t(value)* unnamed;
  struct _ore_context* parent;
  int err;
} ore_context;

typedef ore_value (*ore_cfunc_t)(ore_context*, int, ore_value*);

ore_value ore_call(ore_context*, mpc_ast_t*);
ore_value ore_eval(ore_context*, mpc_ast_t*);
ore_context* ore_new(ore_context*);
void ore_destroy(ore_context*);

int verbose = 0;

void
ore_value_real_free(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_STR:
      if (verbose)
        printf("free str %s\n", v.v.s->p);
      free(v.v.s->p);
      free(v.v.s);
      v.v.s = NULL;
      break;
    case ORE_TYPE_ARRAY:
      if (verbose)
        printf("free array %p\n", v.v.a->p);
      kl_destroy(value, v.v.a->p);
      free(v.v.a);
      v.v.a = NULL;
      break;
    case ORE_TYPE_HASH:
      if (verbose)
        printf("free hash %p\n", v.v.h->p);
      kh_destroy(value, v.v.h->p);
      free(v.v.h);
      v.v.h = NULL;
      break;
  }
  v.t = ORE_TYPE_NIL;
}

void
ore_value_free(void *p) {
  ore_value_unref(*(ore_value*) p);
}

void
ore_value_ref(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_STR:
      v.v.s->ref++;
      if (verbose)
        printf("ref str %d %p\n", v.v.s->ref, v.v.s->p);
      break;
    case ORE_TYPE_ARRAY:
      v.v.a->ref++;
      if (verbose)
        printf("ref array %d %p\n", v.v.a->ref, v.v.a->p);
      break;
    case ORE_TYPE_HASH:
      v.v.h->ref++;
      if (verbose)
        printf("ref hash %d %p\n", v.v.h->ref, v.v.h->p);
      break;
  }
}

void
ore_value_unref(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_STR:
      if (verbose)
        printf("unref str %d %s\n", v.v.s->ref, v.v.s->p);
      if (--v.v.s->ref <= 0)
        ore_value_real_free(v);
      break;
    case ORE_TYPE_ARRAY:
      if (verbose)
        printf("unref array %d %p\n", v.v.a->ref, v.v.a->p);
      if (--v.v.a->ref <= 0)
        ore_value_real_free(v);
      break;
    case ORE_TYPE_HASH:
      if (verbose)
        printf("unref hash %d %p\n", v.v.h->ref, v.v.h->p);
      if (--v.v.h->ref <= 0)
        ore_value_real_free(v);
      break;
  }
}

const char*
ore_value_str(ore_value v) {
  return v.v.s->p;
}

ore_value
ore_value_nil() {
  ore_value v = { ORE_TYPE_NIL, 0 };
  return v;
}

int
ore_is_nil(ore_value v) {
  return v.t == ORE_TYPE_NIL;
}

ore_value
ore_parse_num(ore_context* ore, const char* s) {
  ore_value v;
  if (!strchr(s, '.')) {
    v.t = ORE_TYPE_INT;
    v.v.i = atoi(s);
  } else {
    v.t = ORE_TYPE_FLOAT;
    v.v.d = atof(s);
  }
  return v;
}

ore_value
ore_value_array_from_klist(klist_t(value)* p) {
  ore_value v = { ORE_TYPE_ARRAY };
  v.v.a = (ore_array*) malloc(sizeof(ore_array));
  v.v.a->ref = 0;
  v.v.a->p = p;
  return v;
}

ore_value
ore_value_hash_from_khash(khash_t(value)* p) {
  ore_value v = { ORE_TYPE_HASH };
  v.v.h = (ore_hash*) malloc(sizeof(ore_hash));
  v.v.h->ref = 0;
  v.v.h->p = p;
  return v;
}

ore_value
ore_value_str_from_ptr(char* p, int l) {
  ore_value v = { ORE_TYPE_STR };
  v.v.s = (ore_string*) malloc(sizeof(ore_string));
  v.v.s->ref = 0;
  v.v.s->l = l;
  v.v.s->p = p;
  return v;
}

ore_value
ore_parse_str(ore_context* ore, const char* s) {
  ore_value v = { ORE_TYPE_STR };
  size_t l = strlen(s) - 2;
  v.v.s = (ore_string*) malloc(sizeof(ore_string));
  v.v.s->ref = 0;
  v.v.s->l = l;
  v.v.s->p = calloc(1, l + 1);
  strncpy(v.v.s->p, s + 1, l);
  return v;
}

ore_value
ore_len(ore_context* ore, int num_in, ore_value* args) {
  ore_value v = { ORE_TYPE_INT };
  switch (args[0].t) {
    case ORE_TYPE_STR:
      v.v.i = strlen(args[0].v.s->p);
      return v;
    case ORE_TYPE_ARRAY:
      {
        klist_t(value)* a = (klist_t(value)*) args[0].v.a;
        kliter_t(value)* k;
        kliter_t(value)* b = kl_begin(a);
        int n = 0;
        for (k = b; k != kl_end(a); k = kl_next(k)) n++;
        v.v.i = n;
      }
      return v;
  }
  fprintf(stderr, "Argument should be string or array\n");
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

const char*
ore_kind(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_NIL:
      return "nil";
    case ORE_TYPE_INT:
      return "int";
    case ORE_TYPE_FLOAT:
      return "float";
    case ORE_TYPE_STR:
      return "string";
    case ORE_TYPE_CFUNC:
      return "func";
    case ORE_TYPE_FUNC:
      return "func";
    case ORE_TYPE_ARRAY:
      return "array";
    case ORE_TYPE_HASH:
      return "hash";
  }
  return "unknown";
}
ore_value
ore_print(ore_context* ore, int num_in, ore_value* args) {
  int i;
  for (i = 0; i < num_in; i++) {
    if (i != 0) printf(", ");
    ore_value v = args[i];
    switch (v.t) {
      case ORE_TYPE_NIL:
        printf("nil");
        break;
      case ORE_TYPE_INT:
        printf("%d", v.v.i);
        break;
      case ORE_TYPE_FLOAT:
        printf("%f", v.v.d);
        break;
      case ORE_TYPE_STR:
        printf("%s", v.v.s->p);
        break;
      case ORE_TYPE_ARRAY:
        {
          klist_t(value)* a = (klist_t(value)*) v.v.a;
          kliter_t(value)* k;
          kliter_t(value)* b = kl_begin(a);
          printf("[");
          for (k = b; k != kl_end(a); k = kl_next(k)) {
            if (k != b) {
              printf(",");
            }
            ore_value pa[] = { kl_val(k) };
            ore_print(ore, 1, pa);
          }
          printf("]");
        }
        break;
      case ORE_TYPE_HASH:
        {
          khash_t(value)* h = (khash_t(value)*) v.v.h;
          khiter_t k, b = kh_begin(h);
          int n = 0;
          printf("{");
          for (k = b; k != kh_end(h); k++) {
		    if (!kh_exist(h, k)) continue;						\
            if (n > 0) {
              printf(",");
            }
            const char* key = kh_key(h, k);
            printf("%s: ", key);
            ore_value pa[] = { kh_val(h, k) };
            ore_print(ore, 1, pa);
            n++;
          }
          printf("}");
        }
        break;
      case ORE_TYPE_FUNC:
        printf("<func-0x%p>", v.v.f.x.o);
        break;
      case ORE_TYPE_CFUNC:
        printf("<func-0x%p>", v.v.f.x.c);
        break;
      default:
        printf("<unknown>");
        break;
    }
  }
}

ore_value
ore_println(ore_context* ore, int num_in, ore_value* args) {
  ore_print(ore, num_in, args);
  printf("\n");
  return ore_value_nil();
}

ore_value
ore_p(ore_value v) {
  ore_println(NULL, 1, &v);
}

ore_value
ore_get(ore_context* ore, const char* name) {
  ore_context* p = ore;
  if (!p)
    return ore_value_nil();
  khint_t k;
  while (p) {
    k = kh_get(value, p->env, name);
    if (k != kh_end(p->env)) {
      return kh_value(p->env, k);
    }
    p = p->parent;
  }
  fprintf(stderr, "Unknown identifier '%s'\n", name);
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

void
ore_set(ore_context* ore, const char* name, ore_value v) {
  khint_t k;
  int r;
  while (ore) {
    k = kh_get(value, ore->env, name);
    if (k != kh_end(ore->env) || ore->parent == NULL) {
      ore_value old = kh_value(ore->env, k);
      ore_value_unref(old);
      ore_value_ref(v);
      k = kh_put(value, ore->env, name, &r);
      kh_value(ore->env, k) = v;
      return;
    }
    ore = ore->parent;
  }
}

ore_value
ore_define(ore_context* ore, const char* name, ore_value v) {
  khint_t k = kh_get(value, ore->env, name);
  int r;
  if (k != kh_end(ore->env)) {
    ore_value old = kh_value(ore->env, k);
    ore_value_unref(old);
  }
  k = kh_put(value, ore->env, name, &r);
  ore_value_ref(v);
  kh_value(ore->env, k) = v;
}

void
ore_define_cfunc(ore_context* ore, const char* name, int num_in, ore_cfunc_t c) {
  int r = 0;
  khint_t k = kh_put(value, ore->env, name, &r);
  ore_value v = { ORE_TYPE_CFUNC };
  v.v.f.env = ore;
  v.v.f.num_in = num_in;
  v.v.f.x.c = c;
  ore_define(ore, name, v);
}

ore_value
ore_call(ore_context* ore, mpc_ast_t *t) {
  ore_value fn;
  if (is_a(t->children[0], "ident")) {
    fn = ore_get(ore, t->children[0]->contents);
  } else {
    fn = ore_eval(ore, t->children[0]);
  }
  if (ore->err != ORE_ERROR_NONE)
    return ore_value_nil();
  if (fn.t != ORE_TYPE_FUNC && fn.t != ORE_TYPE_CFUNC) {
    fprintf(stderr, "Unknown function '%s'\n", t->children[0]->contents);
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_value v = ore_value_nil();
  switch (fn.t) {
    case ORE_TYPE_CFUNC:
      {
        int num_in = t->children_num / 2 - 1, n = 0, i;
        if (fn.v.f.num_in != -1 && num_in != fn.v.f.num_in) {
          fprintf(stderr, "Number of arguments mismatch: %d for %d\n",
            num_in, fn.v.f.num_in);
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
        ore_value* args = (ore_value*) malloc(sizeof(ore_value) * num_in);
        for (i = 2; i < t->children_num - 1; i += 2) {
          args[n++] = ore_eval(ore, t->children[i]);
        }
        v = ((ore_cfunc_t)fn.v.f.x.c) ((ore_context*) fn.v.f.env, num_in, args);
        free(args);
      }
      break;
    case ORE_TYPE_FUNC:
      {
        int num_in = t->children_num / 2 - 1, n = 0, i;
        if (fn.v.f.num_in != -1 && num_in != fn.v.f.num_in) {
          fprintf(stderr, "Number of arguments mismatch: %d for %d\n",
            num_in, fn.v.f.num_in);
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
        ore_value* args = (ore_value*) malloc(sizeof(ore_value) * num_in);
        for (i = 2; i < t->children_num - 1; i += 2) {
          args[n++] = ore_eval(ore, t->children[i]);
        }

        ore_context* env = ore_new((ore_context*) fn.v.f.env);
        mpc_ast_t* stmts = NULL;
        mpc_ast_t* f = fn.v.f.x.o;
        n = 0;
        int vararg = 0;
        for (i = 2; i < f->children_num; i++) {
          if (is_a(f->children[i], "vararg")) {
            klist_t(value)* a = kl_init(value);
            int j;
            for (j = 0; j < num_in; j++) {
              *kl_pushp(value, a) = args[j];
            }
            ore_define(env, f->children[i-1]->contents, ore_value_array_from_klist(a));
          } else if (is_a(f->children[i], "ident")) {
            if (n < num_in)
              ore_define(env, f->children[i]->contents, args[n++]);
          } else if (is_a(f->children[i], "char")) {
            if (f->children[i]->contents[0] == '{') {
              i++;
              break;
            }
          }
        }
        for (; i < f->children_num; i++) {
          if (stmts == NULL && !is_a(f->children[i], "char")) {
            stmts = f->children[i];
          }
        }
        if (stmts) {
          v = ore_eval(env, stmts);
          if (ore->err != ORE_ERROR_EXCEPTION)
            ore->err = ORE_ERROR_NONE;
          *kl_pushp(value, ore->unnamed) = v;
        }
        free(args);
        ore_destroy(env);
      }
      break;
  }
  return v;
}

ore_value* ore_index_ref(ore_context* ore, ore_value v, ore_value e, int update) {
  if (v.t == ORE_TYPE_ARRAY) {
    if (e.t != ORE_TYPE_INT) {
      fprintf(stderr, "Array index should be int\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return NULL;
    }
    klist_t(value)* a = (klist_t(value)*) v.v.a->p;
    int n = 0;
    kliter_t(value)* k;
    kliter_t(value)* b = kl_begin(a);
    for (k = b; k != kl_end(a); k = kl_next(k)) {
      if (n == e.v.i) {
        if (update) {
          ore_value old = kl_val(k);
          ore_value_unref(old);
        }
        return &kl_val(k);
      }
      n++;
    }
    fprintf(stderr, "Out of bounds for array\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return NULL;
  }
  if (v.t == ORE_TYPE_HASH) {
    if (e.t != ORE_TYPE_STR) {
      fprintf(stderr, "Hash index should be string\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return NULL;
    }
    khash_t(value)* h = (khash_t(value)*) v.v.h->p;
    if (update) {
      int r;
      khint_t k = kh_get(value, h, e.v.s->p);
      if (k != kh_end(ore->env)) {
        ore_value old = kh_value(ore->env, k);
        ore_value_unref(old);
      }
      k = kh_put(value, h, e.v.s->p, &r);
      return &kh_value(h, k);
    } else {
      khint_t k = kh_get(value, h, e.v.s->p);
      if (k != kh_end(ore->env)) {
        return &kh_value(h, k);
      }
    }
    return NULL;
  }
  fprintf(stderr, "Unknown index operation for %s\n", ore_kind(v));
  ore->err = ORE_ERROR_EXCEPTION;
  return NULL;
}


ore_value
ore_eval(ore_context* ore, mpc_ast_t* t) {
  int i, r;
  if (verbose)
    printf("tag: %s\n", t->tag);
  if (is_a(t, "eof") || is_a(t, "comment")) {
    return ore_value_nil();
  }
  if (is_a(t, "number")) {
    return ore_parse_num(ore, t->contents);
  }
  if (is_a(t, "string")) {
    return ore_parse_str(ore, t->contents);
  }
  if (is_a(t, "array")) {
    klist_t(value)* a = kl_init(value);
    for (i = 1; i < t->children_num - 1; i += 2) {
      ore_value e = ore_eval(ore, t->children[i]);
      *kl_pushp(value, a) = e;
    }
    return ore_value_array_from_klist(a);
  }
  if (is_a(t, "hash")) {
    khash_t(value)* h = kh_init(value);
    for (i = 1; i < t->children_num - 1; i += 2) {
      ore_value key = ore_eval(ore, t->children[i]->children[0]);
      ore_value val = ore_eval(ore, t->children[i]->children[2]);
      khint_t k = kh_put(value, h, key.v.s->p, &r);
      kh_value(h, k) = val;
    }
    return ore_value_hash_from_khash(h);
  }
  if (is_a(t, "item")) {
    ore_value v = ore_eval(ore, t->children[0]);
    ore_value e = ore_eval(ore, t->children[2]);
    ore_value* r = ore_index_ref(ore, v, e, 0);
    if (r == NULL) {
      return ore_value_nil();
    }
    return *r;
  }
  if (is_a(t, "ident")) {
    return ore_get(ore, t->contents);
  }
  if (is_a(t, "factor")) {
    return ore_eval(ore, t->children[1]);
  }
  if (is_a(t, "lexp") || is_a(t, "term")) {
    ore_value v = ore_eval(ore, t->children[0]);
    for (i = 1; i < t->children_num; i += 2) {
      char* op = t->children[i]->contents;
      ore_value rhs = ore_eval(ore, t->children[i+1]);
      switch (v.t) {
        case ORE_TYPE_INT:
          {
            int iv = rhs.t == ORE_TYPE_INT ? rhs.v.i : rhs.t == ORE_TYPE_FLOAT ? (int) rhs.v.d : 0;
            if (strcmp(op, "+") == 0) { v.v.i += iv; }
            else if (strcmp(op, "-") == 0) { v.v.i -= iv; }
            else if (strcmp(op, "*") == 0) { v.v.i *= iv; }
            else if (strcmp(op, "/") == 0) { v.v.i /= iv; }
            else if (strcmp(op, "%") == 0) { v.v.i %= iv; }
            else {
              fprintf(stderr, "Unknown operation '%s' for int\n", op);
              ore->err = ORE_ERROR_EXCEPTION;
              return ore_value_nil();
            }
          }
          break;
        case ORE_TYPE_FLOAT:
          {
            double fv = rhs.t == ORE_TYPE_INT ? (double) rhs.v.i : rhs.t == ORE_TYPE_FLOAT ? rhs.v.d : 0;
            if (strcmp(op, "+") == 0) { v.v.d += fv; }
            else if (strcmp(op, "-") == 0) { v.v.d -= fv; }
            else if (strcmp(op, "*") == 0) { v.v.d *= fv; }
            else if (strcmp(op, "/") == 0) { v.v.d /= fv; }
            else if (strcmp(op, "%") == 0) { v.v.d = ((int) v.v.d % (int) fv); }
            else {
              fprintf(stderr, "Unknown operation '%s' for float\n", op);
              ore->err = ORE_ERROR_EXCEPTION;
              return ore_value_nil();
            }
          }
          break;
        case ORE_TYPE_STR:
          {
            char buf[32], *p = buf;
            if (strcmp(op, "+") == 0) {
              if (rhs.t == ORE_TYPE_INT)
                sprintf(buf, "%i", rhs.v.i);
              else if (rhs.t == ORE_TYPE_FLOAT)
                sprintf(buf, "%f", rhs.v.d);
              else if (rhs.t == ORE_TYPE_STR)
                p = rhs.v.s->p;

              size_t l = strlen(p) + strlen(v.v.s->p) + 1;
              char* s = calloc(1, l);
              strcpy(s, v.v.s->p);
              strcat(s, p);
              v = ore_value_str_from_ptr(s, l);
            } else {
              fprintf(stderr, "Unknown operation '%s' for string\n", op);
              ore->err = ORE_ERROR_EXCEPTION;
              return ore_value_nil();
            }
          }
          break;
      }
    }
    return v;
  }
  if (is_a(t, "let_v")) {
    ore_value v = ore_eval(ore, t->children[2]);
    ore_set(ore, t->children[0]->contents, v);
    return v;
  }
  if (is_a(t, "let_a")) {
    ore_value lhs = ore_eval(ore, t->children[0]->children[0]);
    ore_value i = ore_eval(ore, t->children[0]->children[2]);
    ore_value rhs = ore_eval(ore, t->children[2]);
    ore_value* r = ore_index_ref(ore, lhs, i, 1);
    if (r == NULL) {
      return ore_value_nil();
    }
    ore_value_ref(rhs);
    *r = rhs;
    return rhs;
  }
  if (is_a(t, "var")) {
    ore_value v = ore_eval(ore, t->children[3]);
    ore_define(ore, t->children[1]->contents, v);
    return v;
  }
  if (is_a(t, "func")) {
    ore_value v = { ORE_TYPE_FUNC };
    v.v.f.env = ore;
    v.v.f.num_in = -1;
    v.v.f.x.o = t;
    ore_set(ore, t->children[1]->contents, v);
    return v;
  }
  if (is_a(t, "lambda")) {
    ore_value v = { ORE_TYPE_FUNC };
    v.v.f.env = ore;
    v.v.f.num_in = -1;
    v.v.f.x.o = t;
    return v;
  }
  if (is_a(t, "call")) {
    return ore_call(ore, t);
  }
  if (is_a(t, "return")) {
    ore_value v = ore_eval(ore, t->children[1]);
    ore->err = ORE_ERROR_RETURN;
    ore_value_ref(v);
    return v;
  }
  if (is_a(t, "stmts") || t->tag[0] == '>') {
    ore_value v;
    for (i = 0; i < t->children_num; i++) {
      v = ore_eval(ore, t->children[i]);
      if (ore->err != ORE_ERROR_NONE) {
        return v;
      }
    }
    return ore_value_nil();
  }
  if (is_a(t, "stmt")) {
    return ore_eval(ore, t->children[0]);
  }
  if (is_a(t, "char") && !strcmp(t->contents, ";")) {
    return ore_value_nil();
  }
  fprintf(stderr, "Unknown operation '%s'\n", t->tag);
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

ore_context*
ore_new(ore_context* parent) {
  ore_context* ore = (ore_context*) malloc(sizeof(ore_context));
  ore->env = kh_init(value);
  ore->unnamed = kl_init(value);
  ore->err = ORE_ERROR_NONE;
  ore->parent = parent;
  return ore;
}

void
ore_destroy(ore_context* ore) {
  ore_value v;
  kh_foreach_value(ore->env, v, ore_value_unref(v));
  kl_destroy(value, ore->unnamed);
}

int
parse_args(int argc, char **argv) {
  int i;
  for (i = 1; i < argc; i++) {
    if (argv[i][0] != '-') break;
    switch (argv[i][1]) {
    case 'v':
      verbose = 1;
      break;
    default:
      return -1;
    }
  }
  if (i == argc) return 0;
  return i;
}

void
usage(char* prog) {
  fprintf(stderr, "usage of %s: file\n", prog);
}

int main(int argc, char **argv) {
  int f = parse_args(argc, argv);
  if (f < 0) {
    usage(argv[0]);
    exit(1);
  }

  mpc_parser_t* Number   = mpc_new("number");
  mpc_parser_t* Factor   = mpc_new("factor");
  mpc_parser_t* String   = mpc_new("string");
  mpc_parser_t* Array    = mpc_new("array");
  mpc_parser_t* Pair     = mpc_new("pair");
  mpc_parser_t* Hash     = mpc_new("hash");
  mpc_parser_t* Ident    = mpc_new("ident");
  mpc_parser_t* Term     = mpc_new("term");
  mpc_parser_t* Lexp     = mpc_new("lexp");
  mpc_parser_t* LetV     = mpc_new("let_v");
  mpc_parser_t* Value    = mpc_new("value");
  mpc_parser_t* Item     = mpc_new("item");
  mpc_parser_t* LetA     = mpc_new("let_a");
  mpc_parser_t* Var      = mpc_new("var");
  mpc_parser_t* Vararg   = mpc_new("vararg");
  mpc_parser_t* Func     = mpc_new("func");
  mpc_parser_t* Lambda   = mpc_new("lambda");
  mpc_parser_t* Call     = mpc_new("call");
  mpc_parser_t* Anoncall = mpc_new("anoncall");
  mpc_parser_t* Return   = mpc_new("return");
  mpc_parser_t* Comment  = mpc_new("comment");
  mpc_parser_t* Eof      = mpc_new("eof");
  mpc_parser_t* Stmt     = mpc_new("stmt");
  mpc_parser_t* Stmts    = mpc_new("stmts");
  mpc_parser_t* Program  = mpc_new("program");

  mpc_err_t* err = mpca_lang(MPC_LANG_DEFAULT, STRUCTURE,
      Number, Factor, String, Array, Pair, Hash, Ident,
      Term, Lexp, LetV, Value, Item, LetA, Var, Vararg,
      Lambda, Func, Call, Anoncall, Return, Comment, Eof,
      Stmt, Stmts, Program);
  if (err != NULL) {
    mpc_err_print(err);
    mpc_err_delete(err);
    goto leave;
  }

  mpc_result_t result;
  ore_context* ore = ore_new(NULL);
  ore_define_cfunc(ore, "print", -1, ore_print);
  ore_define_cfunc(ore, "println", -1, ore_println);
  ore_define_cfunc(ore, "len", 1, ore_len);
  if (f > 0) {
    if (!mpc_parse_contents(argv[f], Program, &result)) {
      mpc_err_print(result.error);
      mpc_err_delete(result.error);
    } else {
      if (verbose)
        mpc_ast_print(result.output);
      ore_eval(ore, result.output);
      mpc_ast_delete(result.output);
    }
  } else {
    char buf[BUFSIZ];
    mpc_ast_t* root = mpc_ast_new(">", "");
    while (1) {
      printf("> ");
      if (!fgets(buf, sizeof(buf), stdin)) {
        break;
      }
      int l = strlen(buf);
      if (l > 0 && buf[l-1] == '\n') { buf[l-1] = 0; l--; }
      if (l == 0) continue;
      if (!mpc_parse(argv[0], buf, Stmt, &result)) {
        mpc_err_print(result.error);
        mpc_err_delete(result.error);
        continue;
      }
      if (verbose)
        mpc_ast_print(result.output);
      ore_eval(ore, result.output);
      mpc_ast_add_child(root, result.output);
    }
    mpc_ast_delete(root);
  }
  ore_destroy(ore);

leave:
  mpc_cleanup(25,
      Number, Factor, String, Array, Pair, Hash, Ident,
      Term, Lexp, LetV, Value, Item, LetA, Var, Vararg,
      Lambda, Func, Call, Anoncall, Return, Comment, Eof,
      Stmt, Stmts, Program);
  return 0;
}

// vim:set et:
