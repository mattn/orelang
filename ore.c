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

typedef struct _ore_value {
  ore_type t;
  union {
    int i;
    double d;
    char* s;
    void* a;
    void* h;
    struct {
      void* env;
      int num_in;
      union {
        void* c;
        ore_func o;
      } x;
    } f;
  } v;
  int ref;
} ore_value;

void ore_value_ref(ore_value* v);
void ore_value_unref(ore_value* v);

KHASH_MAP_INIT_STR(ident, ore_value)

KLIST_INIT(ident, ore_value, ore_value_free)

typedef struct _ore_context {
  khash_t(ident)* env;
  klist_t(ident)* unnamed;
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
ore_value_real_free(ore_value* v) {
  switch (v->t) {
    case ORE_TYPE_STR:
      if (verbose)
        printf("free %d, %p, %s\n", v->ref, v->v.s, v->v.s);
      free(v->v.s);
      v->v.s = NULL;
      break;
    case ORE_TYPE_FUNC:
      break;
  }
  v->t = ORE_TYPE_NIL;
}

void
ore_value_free(void *p) {
  ore_value* v = (ore_value*) p;
  ore_value_unref(v);
}

void
ore_value_ref(ore_value *v) {
  v->ref++;
}

void
ore_value_unref(ore_value* v) {
  if (--v->ref <= 0) {
    ore_value_real_free(v);
  }
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
ore_parse_str(ore_context* ore, const char* s) {
  ore_value v = { ORE_TYPE_STR };
  size_t l = strlen(s) - 2;
  v.v.s = calloc(1, l + 1);
  strncpy(v.v.s, s + 1, l);
  v.v.s[l] = 0;
  return v;
}

ore_value
ore_len(ore_context* ore, int num_in, ore_value* args) {
  ore_value v = { ORE_TYPE_INT };
  switch (args[0].t) {
    case ORE_TYPE_STR:
      v.v.i = strlen(args[0].v.s);
      return v;
    case ORE_TYPE_ARRAY:
      {
        klist_t(ident)* a = (klist_t(ident)*) args[0].v.a;
        kliter_t(ident)* k;
        kliter_t(ident)* b = kl_begin(a);
        int n = 0;
        for (k = b; k != kl_end(a); k = kl_next(k)) n++;
        v.v.i = n;
      }
      return v;
    default:
      break;
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
        printf("%s", v.v.s);
        break;
      case ORE_TYPE_ARRAY:
        {
          klist_t(ident)* a = (klist_t(ident)*) v.v.a;
          kliter_t(ident)* k;
          kliter_t(ident)* b = kl_begin(a);
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
          khash_t(ident)* h = (khash_t(ident)*) v.v.h;
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
  if (!ore)
    return ore_value_nil();
  khint_t k;
  while (ore) {
    k = kh_get(ident, ore->env, name);
    if (k != kh_end(ore->env)) {
      return kh_value(ore->env, k);
    }
    ore = ore->parent;
  }
  return ore_value_nil();
}

void
ore_ref(ore_context* ore, const char* name) {
  khint_t k;
  int r;
  while (ore) {
    k = kh_get(ident, ore->env, name);
    if (k != kh_end(ore->env)) {
      ore_value v = kh_value(ore->env, k);
      ore_value_ref(&v);
      k = kh_put(ident, ore->env, name, &r);
      kh_value(ore->env, k) = v;
      return;
    }
    ore = ore->parent;
  }
}

void
ore_set(ore_context* ore, const char* name, ore_value* v) {
  khint_t k;
  int r;
  while (ore) {
    k = kh_get(ident, ore->env, name);
    if (k != kh_end(ore->env)) {
      ore_value old = kh_value(ore->env, k);
      ore_value_unref(&old);
      kh_value(ore->env, k) = old; // update ref
      k = kh_put(ident, ore->env, name, &r);
      ore_value_ref(v);
      kh_value(ore->env, k) = *v;
      return;
    }
    if (ore->parent == NULL) {
      k = kh_put(ident, ore->env, name, &r);
      ore_value_ref(v);
      kh_value(ore->env, k) = *v;
      return;
    }
    ore = ore->parent;
  }
}

ore_value
ore_define(ore_context* ore, const char* name, ore_value v) {
  khint_t k = kh_get(ident, ore->env, name);
  int r;
  if (k != kh_end(ore->env)) {
    ore_value old = kh_value(ore->env, k);
    ore_value_unref(&old);
  }
  k = kh_put(ident, ore->env, name, &r);
  ore_value_ref(&v);
  kh_value(ore->env, k) = v;
}

void
ore_define_cfunc(ore_context* ore, const char* name, int num_in, ore_cfunc_t c) {
  int r = 0;
  khint_t k = kh_put(ident, ore->env, name, &r);
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
            ore_value vargs = { ORE_TYPE_ARRAY };
            klist_t(ident)* a = kl_init(ident);
            vargs.v.a = a;
            int j;
            for (j = 0; j < num_in; j++) {
              *kl_pushp(ident, a) = args[j];
            }
            ore_define(env, f->children[i-1]->contents, vargs);
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
          *kl_pushp(ident, ore->unnamed) = v;
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
    klist_t(ident)* a = (klist_t(ident)*) v.v.a;
    int n = 0;
    kliter_t(ident)* k;
    kliter_t(ident)* b = kl_begin(a);
    for (k = b; k != kl_end(a); k = kl_next(k)) {
      if (n == e.v.i) {
        if (update) {
          ore_value old = kl_val(k);
          ore_value_unref(&old);
          kl_val(k) = old; // update ref
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
    khash_t(ident)* h = (khash_t(ident)*) v.v.h;
    if (update) {
      int r;
      khint_t k = kh_get(ident, h, e.v.s);
      if (k != kh_end(ore->env)) {
        ore_value old = kh_value(ore->env, k);
        ore_value_unref(&old);
        kh_value(ore->env, k) = old; // update ref
      }
      k = kh_put(ident, h, e.v.s, &r);
      return &kh_value(h, k);
    } else {
      khint_t k = kh_get(ident, h, e.v.s);
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
    ore_value v = { ORE_TYPE_ARRAY };
    klist_t(ident)* a = kl_init(ident);
    v.v.a = a;
    for (i = 1; i < t->children_num - 1; i += 2) {
      ore_value e = ore_eval(ore, t->children[i]);
      *kl_pushp(ident, a) = e;
    }
    return v;
  }
  if (is_a(t, "hash")) {
    ore_value v = { ORE_TYPE_HASH };
    khash_t(ident)* h = kh_init(ident);
    v.v.h = h;
    for (i = 1; i < t->children_num - 1; i += 2) {
      ore_value key = ore_eval(ore, t->children[i]->children[0]);
      ore_value val = ore_eval(ore, t->children[i]->children[2]);
      khint_t k = kh_put(ident, h, key.v.s, &r);
      kh_value(h, k) = val;
    }
    return v;
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
                p = rhs.v.s;

              size_t l = strlen(p) + strlen(v.v.s) + 1;
              char* s = calloc(1, l);
              strcpy(s, v.v.s);
              strcat(s, p);
              v.t = ORE_TYPE_STR;
              v.v.s = s;
              v.ref = 0;
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
    if (is_a(t->children[2], "ident"))
      ore_ref(ore, t->children[2]->contents);
    ore_set(ore, t->children[0]->contents, &v);
    return v;
  }
  if (is_a(t, "let_a")) {
    ore_value lhs = ore_eval(ore, t->children[0]->children[0]);
    ore_value i = ore_eval(ore, t->children[0]->children[2]);
    ore_value rhs = ore_eval(ore, t->children[2]);
    if (is_a(t->children[2], "ident"))
      ore_ref(ore, t->children[2]->contents);
    ore_value* r = ore_index_ref(ore, lhs, i, 1);
    if (r == NULL) {
      return ore_value_nil();
    }
    ore_value_ref(&rhs);
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
    ore_set(ore, t->children[1]->contents, &v);
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
    ore_value_ref(&v);
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
  ore->env = kh_init(ident);
  ore->unnamed = kl_init(ident);
  ore->err = ORE_ERROR_NONE;
  ore->parent = parent;
  return ore;
}

void
ore_destroy(ore_context* ore) {
  ore_value v;
  kh_foreach_value(ore->env, v, ore_value_unref(&v));
  kl_destroy(ident, ore->unnamed);
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
