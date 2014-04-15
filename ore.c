#include "mpc.h"
#include "khash.h"
#include "klist.h"

#define STRUCTURE \
"                                                           \n" \
"number    : /-?[0-9]+(\\.[0-9]*)?(e[0-9]+)?/ ;             \n" \
"factor    : '(' <lexp> ')'                                 \n" \
"          | <number>                                       \n" \
"          | <string>                                       \n" \
"          | <lambda>                                       \n" \
"          | <ident> ;                                      \n" \
"string    : /\"[^\"]*\"/ ;                                 \n" \
"ident     : /[a-zA-Z][a-zA-Z0-9_]*/ ;                      \n" \
"                                                           \n" \
"term      : <factor> (('*' | '/' | '%') <factor>)* ;       \n" \
"lexp      : <term> (('+' | '-') <term>)* ;                 \n" \
"let       : <ident> '=' <lexp> ';' ;                       \n" \
"                                                           \n" \
"lambda    : /func/                                           " \
"        '(' <ident>? (',' <ident>)* ')' '{' <stmt>* '}' ;  \n" \
"func      : /func/ <ident>                                   " \
"        '(' <ident>? (',' <ident>)* ')' '{' <stmt>* '}' ;  \n" \
"                                                           \n" \
"call      : <ident> '(' <lexp>? (',' <lexp>)* ')' ';' ;    \n" \
"comment   : /#[^\n]*/ ;                                    \n" \
"eof       : /$/ ;                                          \n" \
"stmt      : (<let> | <call> | <func> | <comment>) ;        \n" \
"program   : <stmt>* <eof> ;                                \n"

void ore_free(void *p);

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
  union _v {
    int i;
    double d;
    char* s;
    void* c;
    ore_func f;
  } v;
} ore_value;

KHASH_MAP_INIT_STR(ident, ore_value)

KLIST_INIT(ident, ore_value, ore_free)

typedef struct _ore_context {
  khash_t(ident)* env;
  klist_t(ident)* gc;
  struct _ore_context* parent;
} ore_context;

typedef ore_value (*ore_cfunc_t)(ore_context*, int, ore_value*);

ore_value ore_call(ore_context*, mpc_ast_t*);
ore_value ore_eval(ore_context*, mpc_ast_t*);

void
ore_free(void *p) {
  ore_value* v = (ore_value*) p;
  switch (v->t) {
    case ORE_TYPE_STR:
      free(v->v.s);
      break;
    case ORE_TYPE_FUNC:
      free(v->v.c);
      break;
  }
}

ore_value
ore_value_nil() {
  ore_value v = { ORE_TYPE_NIL, 0 };
  return v;
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
  *kl_pushp(ident, ore->gc) = v;
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
      default:
        printf("<unkonwn>");
        break;
    }
  }
  printf("\n");
  return ore_value_nil();
}

ore_value
ore_define_cfunc(ore_context* ore, const char* name, ore_cfunc_t c) {
  int r = 0;
  khint_t k = kh_put(ident, ore->env, name, &r);
  ore_value v = { ORE_TYPE_CFUNC };
  v.v.c = c;
  kh_value(ore->env, k) = v;
  return v;
}

ore_value
ore_call(ore_context* ore, mpc_ast_t *t) {
  khint_t k = kh_get(ident, ore->env, t->children[0]->contents);
  if (k == kh_end(ore->env)) {
    fprintf(stderr, "Unknwn function '%s'\n", t->children[0]->contents);
    return ore_value_nil();
  }

  ore_value fn = kh_value(ore->env, k);
  ore_value v = ore_value_nil();
  switch (fn.t) {
    case ORE_TYPE_CFUNC:
      {
        int num_in = t->children_num / 2 - 1, n = 0, i;
        ore_value* args = (ore_value*) malloc(sizeof(ore_value) * num_in);
        for (i = 2; i < t->children_num - 2; i += 2) {
          args[n++] = ore_eval(ore, t->children[i]);
        }
        v = ((ore_cfunc_t)fn.v.c) (ore, num_in, args);
        free(args);
      }
      break;
    case ORE_TYPE_FUNC:
      // TODO: ここをちゃんと実装して引数を渡せる様にする
      v = ore_eval(ore, fn.v.f);
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
    khint_t k = kh_get(ident, ore->env, t->contents);
    if (k == kh_end(ore->env)) {
      return ore_value_nil();
    }
    return kh_value(ore->env, k);
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
    khint_t k = kh_put(ident, ore->env, t->children[0]->contents, &r);
    v = ore_eval(ore, t->children[2]);
    kh_value(ore->env, k) = v;
    return v;
  }
  if (is_a(t, "func")) {
    ore_value v = { ORE_TYPE_FUNC };
    v.v.f = t->children[5];
    khint_t k = kh_put(ident, ore->env, t->children[1]->contents, &r);
    kh_value(ore->env, k) = v;
    return v;
  }
  if (is_a(t, "lambda")) {
    ore_value v = { ORE_TYPE_FUNC };
    v.v.f = t->children[4];
    return v;
  }
  if (is_a(t, "call")) {
    return ore_call(ore, t);
  }
  if (t->tag[0] == '>') {
    for (i = 0; i < t->children_num; i++) {
      ore_eval(ore, t->children[i]);
    }
    return ore_value_nil();
  }
  fprintf(stderr, "Unknwn operation '%s'\n", t->tag);
  return ore_value_nil();
}

ore_context*
ore_new(ore_context* parent) {
  ore_context* ore = (ore_context*) malloc(sizeof(ore_context));
  ore->env = kh_init(ident);
  ore->gc = kl_init(ident);
  ore->parent = parent;
  return ore;
}

void
ore_destroy(ore_context* ore) {
  kl_destroy(ident, ore->gc);
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
  mpc_parser_t* Func    = mpc_new("func");
  mpc_parser_t* Lambda  = mpc_new("lambda");
  mpc_parser_t* Call    = mpc_new("call");
  mpc_parser_t* Comment = mpc_new("comment");
  mpc_parser_t* Eof     = mpc_new("eof");
  mpc_parser_t* Stmt    = mpc_new("stmt");
  mpc_parser_t* Program = mpc_new("program");

  mpc_err_t* err = mpca_lang(MPC_LANG_DEFAULT, STRUCTURE,
      Number, Factor, String, Ident,
      Term, Lexp, Let, Lambda, Func, Call, Comment, Eof,
      Stmt, Program);
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
  ore_define_cfunc(ore, "println", ore_println);
  ore_eval(ore, result.output);
  ore_destroy(ore);

  mpc_ast_delete(result.output);

leave:
  mpc_cleanup(11,
      Number, Factor, String, Ident,
      Term, Lexp, Let, Lambda, Func, Call, Comment, Eof,
      Stmt, Program);
  return 0;
}

// vim:set et:
