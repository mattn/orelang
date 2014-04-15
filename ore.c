#include "mpc.h"
#include "khash.h"
#include "klist.h"

#define STRUCTURE \
"                                                            \n" \
"  number    : /-?[0-9]+/ ;                                  \n" \
"  factor    : '(' <lexp> ')'                                \n" \
"            | <number>                                      \n" \
"            | <string>                                      \n" \
"            | <ident> ;                                     \n" \
"  string    : /\"[^\"]*\"/ ;                                \n" \
"  ident     : /[a-zA-Z][a-zA-Z0-9_]*/ ;                     \n" \
"                                                            \n" \
"  term      : <factor> (('*' | '/' | '%') <factor>)* ;      \n" \
"  lexp      : <term> (('+' | '-') <term>)* ;                \n" \
"  let       : <ident> '=' <lexp> ';' ;                      \n" \
"  call      : <ident> '(' <lexp>? (',' <lexp>)* ')' ';' ;   \n" \
"  comment   : /#[^\n]*/ ;                                   \n" \
"  eof       : /$/ ;                                         \n" \
"  stmts     : (<let> | <call> | <comment>)* <eof> ;         \n"

#define is_a(t, a) (strstr(t->tag, a) != NULL)

#define ORE_TYPE_NIL 0
#define ORE_TYPE_NUM 1
#define ORE_TYPE_STR 2
#define ORE_TYPE_FNC 3

typedef struct {
  int t;
  union _v {
    int i;
    char* s;
  } v;
} ore_value;

ore_value ORE_NIL = { ORE_TYPE_NIL, 0 };

KHASH_MAP_INIT_STR(ident, ore_value)
khash_t(ident) *env;

void v_free(void *p) {
  ore_value* v = (ore_value*) p;
  if (v->t == ORE_TYPE_STR)
    free(v->v.s);
}
KLIST_INIT(ident, ore_value, v_free)
klist_t(ident) *gc;

ore_value ore_call(ore_func ) {
}

ore_value ore_eval(mpc_ast_t* t) {
  int i;
  if (is_a(t, "eof") || is_a(t, "comment")) {
    return ORE_NIL;
  }
  if (is_a(t, "number")) {
    ore_value v = { ORE_TYPE_NUM };
    v.v.i = atoi(t->contents);
    return v;
  }
  if (is_a(t, "string")) {
    ore_value v = { ORE_TYPE_STR };
    size_t l = strlen(t->contents) - 2;
    v.v.s = calloc(1, l + 1);
    strncpy(v.v.s, t->contents + 1, l);
    v.v.s[l] = 0;
    *kl_pushp(ident, gc) = v;
    return v;
  }
  if (is_a(t, "ident")) {
    khint_t k = kh_get(ident, env, t->contents);
    if (k == kh_end(env)) {
      return ORE_NIL;
    }
    return kh_value(env, k);
  }
  if (is_a(t, "factor")) {
    return ore_eval(t->children[1]);
  }
  if (is_a(t, "lexp") || is_a(t, "term")) {
    ore_value lhs = ore_eval(t->children[0]);
    for (i = 1; i < t->children_num; i += 2) {
      char* op = t->children[i]->contents;
      ore_value rhs = ore_eval(t->children[i+1]);
      int iv = rhs.t == ORE_TYPE_NUM ? rhs.v.i : 0;
      if (strcmp(op, "+") == 0) { lhs.v.i += iv; }
      if (strcmp(op, "-") == 0) { lhs.v.i -= iv; }
      if (strcmp(op, "*") == 0) { lhs.v.i *= iv; }
      if (strcmp(op, "/") == 0) { lhs.v.i /= iv; }
      if (strcmp(op, "%") == 0) { lhs.v.i %= iv; }
    }
    return lhs;
  }
  if (is_a(t, "let")) {
    int r = 0;
    ore_value v;
    khint_t k = kh_put(ident, env, t->children[0]->contents, &r);
    v = ore_eval(t->children[2]);
    kh_value(env, k) = v;
    return v;
  }
  if (is_a(t, "call")) {
    int r = 0;
    if (!strcmp(t->children[0]->contents, "println")) {
      for (i = 2; i < t->children_num - 2; i += 2) {
        if (i != 2) printf(", ");
        ore_value v = ore_eval(t->children[i]);
        switch (v.t) {
          case ORE_TYPE_NIL:
            printf("nil");
            break;
          case ORE_TYPE_NUM:
            printf("%d", v.v.i);
            break;
          case ORE_TYPE_STR:
            printf("%s", v.v.s);
            break;
        }
      }
      printf("\n");
    } else {
      fprintf(stderr, "Unknwn function '%s'\n", t->children[0]->contents);
    }
    return ORE_NIL;
  }
  if (t->tag[0] == '>') {
    for (i = 0; i < t->children_num; i++) {
      ore_eval(t->children[i]);
    }
    return ORE_NIL;
  }
  fprintf(stderr, "Unknwn operation '%s'\n", t->tag);
  return ORE_NIL;
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
  mpc_parser_t* Call    = mpc_new("call");
  mpc_parser_t* Comment = mpc_new("comment");
  mpc_parser_t* Eof     = mpc_new("eof");
  mpc_parser_t* Stmts   = mpc_new("stmts");

  mpc_err_t* err = mpca_lang(MPC_LANG_DEFAULT, STRUCTURE,
      Number, Factor, String, Ident, Term, Lexp, Let, Call, Comment, Eof, Stmts);
  if (err != NULL) {
    mpc_err_print(err);
    mpc_err_delete(err);
    goto leave;
  }

  env = kh_init(ident);
  gc = kl_init(ident);

  mpc_result_t result;
  if (!mpc_parse_contents(argv[1], Stmts, &result)) {
    mpc_err_print(result.error);
    mpc_err_delete(result.error);
    goto leave;
  }

  mpc_ast_print(result.output);
  ore_eval(result.output);
  mpc_ast_delete(result.output);
  kl_destroy(ident, gc);

leave:
  mpc_cleanup(11,
      Number, Factor, String, Ident, Term, Lexp, Let, Call, Comment, Eof, Stmts);
  return 0;
}
