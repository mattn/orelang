#include "mpc.h"
#include "khash.h"
#include "klist.h"

#define STRUCTURE \
"                                                                       \n" \
"number    : /-?[0-9]+(\\.[0-9]*)?(e[0-9]+)?/ ;                         \n" \
"factor    : '(' <lexp> ')'                                             \n" \
"          | <number>                                                   \n" \
"          | <string>                                                   \n" \
"          | <lambda>                                                   \n" \
"          | <call>                                                     \n" \
"          | <ident> ;                                                  \n" \
"string    : /\"[^\"]*\"/ ;                                             \n" \
"ident     : /[a-zA-Z][a-zA-Z0-9_]*/ ;                                  \n" \
"                                                                       \n" \
"term      : <factor> (('*' | '/' | '%') <factor>)* ;                   \n" \
"lexp      : <term> (('+' | '-') <term>)* ;                             \n" \
"let       : <ident> '=' <lexp> ';' ;                                   \n" \
"vararg    : \"...\" ;                                                  \n" \
"stmts     : <stmt>* ;                                                  \n" \
"                                                                       \n" \
"lambda    : \"func\"                                                     " \
"        '(' <ident>? (<vararg> | (',' <ident>)*) ')' '{' <stmts> '}' ; \n" \
"func      : \"func\" <ident>                                             " \
"        '(' <ident>? (<vararg> | (',' <ident>)*) ')' '{' <stmts> '}' ; \n" \
"                                                                       \n" \
"call      : <ident> '(' <lexp>? (',' <lexp>)* ')' ;                    \n" \
"return    : \"return\" <lexp> ';' ;                                    \n" \
"comment   : /#[^\n]*/ ;                                                \n" \
"eof       : /$/ ;                                                      \n" \
"stmt      : (<let> | (<call> ';') | <func> | <comment>) ;              \n" \
"program   : <stmts> <eof> ;                                            \n"

void ore_value_free(void* p);

#define is_a(t, a) (strstr(t->tag, a) != NULL)

typedef enum {
  ORE_TYPE_NIL,
  ORE_TYPE_INT,
  ORE_TYPE_FLOAT,
  ORE_TYPE_STR,
  ORE_TYPE_FUNC,
  ORE_TYPE_CFUNC
} ore_type;

typedef mpc_ast_t* ore_func;

typedef struct _ore_value {
  ore_type t;
  union {
    int i;
    double d;
    char* s;
    struct {
      int num_in;
      union {
        void* c;
        ore_func o;
      } x;
    } f;
  } v;
  int ref;
} ore_value;

KHASH_MAP_INIT_STR(ident, ore_value)

KLIST_INIT(ident, ore_value, ore_value_free)

typedef struct _ore_context {
  khash_t(ident)* env;
  struct _ore_context* parent;
} ore_context;

typedef ore_value (*ore_cfunc_t)(ore_context*, int, ore_value*);
void ore_value_ref(ore_value v);
void ore_value_unref(ore_value v);

ore_value ore_call(ore_context*, mpc_ast_t*);
ore_value ore_eval(ore_context*, mpc_ast_t*);
ore_context* ore_new(ore_context*);
void ore_destroy(ore_context*);

void
ore_value_real_free(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_STR:
      free(v.v.s);
      v.v.s = NULL;
      break;
    case ORE_TYPE_FUNC:
      break;
  }
  v.t = ORE_TYPE_NIL;
}


void
ore_value_free(void *p) {
  ore_value* v = (ore_value*) p;
  printf("FREE %d\n", v->t);
  ore_value_unref(*v);
}

void
ore_value_ref(ore_value v) {
  v.ref++;
}

void
ore_value_unref(ore_value v) {
  if (--v.ref <= 0) {
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
ore_strlen(ore_context* ore, int num_in, ore_value* args) {
  if (args[0].t != ORE_TYPE_STR) {
    fprintf(stderr, "Argument should be string\n");
    return ore_value_nil();
  }
  ore_value v = { ORE_TYPE_INT };
  v.v.i = strlen(args[0].v.s);
  return v;
}

ore_value
ore_println(ore_context* ore, int num_in, ore_value* args) {
  int i;
  for (i = 0; i < num_in; i++) {
    if (i != 0) printf(", ");
    switch (args[i].t) {
      case ORE_TYPE_NIL:
        printf("nil");
        break;
      case ORE_TYPE_INT:
        printf("%d", args[i].v.i);
        break;
      case ORE_TYPE_FLOAT:
        printf("%f", args[i].v.d);
        break;
      case ORE_TYPE_STR:
        printf("%s", args[i].v.s);
        break;
      case ORE_TYPE_FUNC:
        printf("<func-0x%p>", args[i].v.f.x.o);
        break;
      case ORE_TYPE_CFUNC:
        printf("<func-0x%p>", args[i].v.f.x.c);
        break;
      default:
        printf("<unknown>");
        break;
    }
  }
  printf("\n");
  return ore_value_nil();
}

ore_value
ore_p(ore_value v) {
  ore_println(NULL, 1, &v);
}

ore_value
ore_define_cfunc(ore_context* ore, const char* name, int num_in, ore_cfunc_t c) {
  int r = 0;
  khint_t k = kh_put(ident, ore->env, name, &r);
  ore_value v = { ORE_TYPE_CFUNC };
  v.v.f.num_in = num_in;
  v.v.f.x.c = c;
  kh_value(ore->env, k) = v;
  return v;
}

ore_value
ore_get(ore_context* ore, const char* name) {
  if (!ore)
    return ore_value_nil();
  khint_t k;
  while (ore) {
    k = kh_get(ident, ore->env, name);
    if (k != kh_end(ore->env)) {
      break;
    }
    ore = ore->parent;
  }
  return kh_value(ore->env, k);
}

void
ore_set(ore_context* ore, const char* name, ore_value v) {
  khint_t k;
  int r;
  while (ore) {
    k = kh_get(ident, ore->env, name);
    if (k != kh_end(ore->env)) {
      ore_value old = kh_value(ore->env, k);
      ore_value_unref(old);
      k = kh_put(ident, ore->env, name, &r);
      kh_value(ore->env, k) = v;
      break;
    }
    if (ore->parent == NULL) {
      k = kh_put(ident, ore->env, name, &r);
      kh_value(ore->env, k) = v;
      break;
    }
    ore = ore->parent;
  }
  ore_value_ref(v);
}

ore_value
ore_define(ore_context* ore, const char* name, ore_value v) {
  khint_t k = kh_get(ident, ore->env, name);
  int r;
  if (k != kh_end(ore->env)) {
    ore_value old = kh_value(ore->env, k);
    ore_value_unref(old);
  }
  k = kh_put(ident, ore->env, name, &r);
  kh_value(ore->env, k) = v;
  ore_value_ref(v);
}

ore_value
ore_call(ore_context* ore, mpc_ast_t *t) {
  ore_value fn = ore_get(ore, t->children[0]->contents);
  if (ore_is_nil(fn)) {
    fprintf(stderr, "Unknown function '%s'\n", t->children[0]->contents);
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
          return ore_value_nil();
        }
        ore_value* args = (ore_value*) malloc(sizeof(ore_value) * num_in);
        for (i = 2; i < t->children_num - 1; i += 2) {
          args[n++] = ore_eval(ore, t->children[i]);
        }
        v = ((ore_cfunc_t)fn.v.f.x.c) (ore, num_in, args);
        free(args);
      }
      break;
    case ORE_TYPE_FUNC:
      {
        // TODO: ここをちゃんと実装して引数を渡せる様にする
        int i;
        
        ore_context* env = ore_new(ore);
        mpc_ast_t* cs = fn.v.f.x.o;
        if (cs) {
          for (i = 0; i < cs->children_num; i++) {
            ore_eval(env, cs->children[i]);
          }
        }
        ore_destroy(env);
      }
      break;
  }
  return v;
}

ore_value
ore_eval(ore_context* ore, mpc_ast_t* t) {
  int i, r;
  ore_value v;
  if (is_a(t, "eof") || is_a(t, "comment")) {
    return ore_value_nil();
  }
  if (is_a(t, "number")) {
    return ore_parse_num(ore, t->contents);
  }
  if (is_a(t, "string")) {
    return ore_parse_str(ore, t->contents);
  }
  if (is_a(t, "ident")) {
    return ore_get(ore, t->contents);
  }
  if (is_a(t, "factor")) {
    return ore_eval(ore, t->children[1]);
  }
  if (is_a(t, "lexp") || is_a(t, "term")) {
    v = ore_eval(ore, t->children[0]);
    for (i = 1; i < t->children_num; i += 2) {
      char* op = t->children[i]->contents;
      ore_value rhs = ore_eval(ore, t->children[i+1]);
      switch (v.t) {
        case ORE_TYPE_INT:
          {
            int iv = rhs.t == ORE_TYPE_INT ? rhs.v.i : rhs.t == ORE_TYPE_FLOAT ? (int) rhs.v.d : 0;
            if (strcmp(op, "+") == 0) { v.v.i += iv; }
            if (strcmp(op, "-") == 0) { v.v.i -= iv; }
            if (strcmp(op, "*") == 0) { v.v.i *= iv; }
            if (strcmp(op, "/") == 0) { v.v.i /= iv; }
            if (strcmp(op, "%") == 0) { v.v.i %= iv; }
          }
          break;
        case ORE_TYPE_FLOAT:
          {
            double fv = rhs.t == ORE_TYPE_INT ? (double) rhs.v.i : rhs.t == ORE_TYPE_FLOAT ? rhs.v.d : 0;
            if (strcmp(op, "+") == 0) { v.v.d += fv; }
            if (strcmp(op, "-") == 0) { v.v.d -= fv; }
            if (strcmp(op, "*") == 0) { v.v.d *= fv; }
            if (strcmp(op, "/") == 0) { v.v.d /= fv; }
            if (strcmp(op, "%") == 0) { v.v.d = ((int) v.v.d % (int) fv); }
          }
          break;
      }
    }
    return v;
  }
  if (is_a(t, "let")) {
    ore_value v = ore_eval(ore, t->children[2]);
    ore_set(ore, t->children[1]->contents, v);
    return v;
  }
  if (is_a(t, "func")) {
    ore_value v = { ORE_TYPE_FUNC };
    int i, num_in = 0;
    mpc_ast_t* stmts = NULL;
    for (i = 3; i < t->children_num; i++) {
      if (is_a(t->children[i], "ident"))
        num_in++;
      else if (is_a(t->children[i], "stmts"))
        stmts = t->children[i];
    }
    v.v.f.num_in = num_in;
    v.v.f.x.o = stmts;
    ore_set(ore, t->children[1]->contents, v);
    return v;
  }
  if (is_a(t, "lambda")) {
    ore_value v = { ORE_TYPE_FUNC };
    int i, num_in = 0;
    mpc_ast_t* stmts = NULL;
    for (i = 3; i < t->children_num; i++) {
      if (is_a(t->children[i], "ident"))
        num_in++;
      else if (is_a(t->children[i], "stmts"))
        stmts = t->children[i];
    }
    v.v.f.num_in = num_in;
    v.v.f.x.o = stmts;
    return v;
  }
  if (is_a(t, "call")) {
    return ore_call(ore, t);
  }
  if (is_a(t, "stmts") || t->tag[0] == '>') {
    for (i = 0; i < t->children_num; i++) {
      ore_eval(ore, t->children[i]);
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
  return ore_value_nil();
}

ore_context*
ore_new(ore_context* parent) {
  ore_context* ore = (ore_context*) malloc(sizeof(ore_context));
  ore->env = kh_init(ident);
  ore->parent = parent;
  return ore;
}

void
ore_destroy(ore_context* ore) {
  ore_value v;
  kh_foreach_value(ore->env, v, ore_value_unref(v));
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage of %s: file\n", argv[0]);
    exit(0);
  }
  mpc_parser_t* Number  = mpc_new("number");
  mpc_parser_t* Factor  = mpc_new("factor");
  mpc_parser_t* String  = mpc_new("string");
  mpc_parser_t* Ident   = mpc_new("ident");
  mpc_parser_t* Term    = mpc_new("term");
  mpc_parser_t* Lexp    = mpc_new("lexp");
  mpc_parser_t* Let     = mpc_new("let");
  mpc_parser_t* Vararg  = mpc_new("vararg");
  mpc_parser_t* Func    = mpc_new("func");
  mpc_parser_t* Lambda  = mpc_new("lambda");
  mpc_parser_t* Call    = mpc_new("call");
  mpc_parser_t* Return  = mpc_new("return");
  mpc_parser_t* Comment = mpc_new("comment");
  mpc_parser_t* Eof     = mpc_new("eof");
  mpc_parser_t* Stmt    = mpc_new("stmt");
  mpc_parser_t* Stmts   = mpc_new("stmts");
  mpc_parser_t* Program = mpc_new("program");

  mpc_err_t* err = mpca_lang(MPC_LANG_DEFAULT, STRUCTURE,
      Number, Factor, String, Ident,
      Term, Lexp, Let, Vararg, Lambda, Func, Call, Comment, Eof,
      Stmt, Stmts, Program);
  if (err != NULL) {
    mpc_err_print(err);
    mpc_err_delete(err);
    goto leave;
  }

  mpc_result_t result;
  if (!mpc_parse_contents(argv[1], Program, &result)) {
    mpc_err_print(result.error);
    mpc_err_delete(result.error);
    goto leave;
  }

  mpc_ast_print(result.output);

  ore_context* ore = ore_new(NULL);
  ore_define_cfunc(ore, "println", -1, ore_println);
  ore_define_cfunc(ore, "strlen", 1, ore_strlen);
  ore_eval(ore, result.output);
  ore_destroy(ore);

  mpc_ast_delete(result.output);

leave:
  mpc_cleanup(14,
      Number, Factor, String, Ident,
      Term, Lexp, Let, Vararg, Lambda, Func, Call, Return, Comment, Eof,
      Stmt, Stmts, Program);
  return 0;
}

// vim:set et:
