#include "ore.h"
#include <math.h>
#include <ctype.h>

#ifdef _WIN32
#include <stdlib.h>
#else
extern char **environ;
#endif

#define STRUCTURE \
"                                                                        \n" \
"number     : /-?([0-9]+(\\.[0-9]*)?(e[0-9]+)?|0x[0-9a-fA-F]+)/ ;        \n" \
"true       : \"true\" ;                                                 \n" \
"false      : \"false\" ;                                                \n" \
"nil        : \"nil\" ;                                                  \n" \
"factor     : '(' <lexp> ')'                                             \n" \
"           | '!' <postfix>                                              \n" \
"           | <number>                                                   \n" \
"           | '-' <postfix>                                              \n" \
"           | <string>                                                   \n" \
"           | <array>                                                    \n" \
"           | <hash>                                                     \n" \
"           | <regexp>                                                   \n" \
"           | <true>                                                     \n" \
"           | <false>                                                    \n" \
"           | <nil>                                                      \n" \
"           | <call>                                                     \n" \
"           | <new>                                                      \n" \
"           | <ident> ;                                                  \n" \
"string     : /\"(\\\\.|[^\"])*\"/ | /'(\\\\.|[^'])*'/ ;                  \n" \
"regexp     : /\\/(\\\\.|[^\\/])*\\// ;                                  \n" \
"item       : <factor> ('[' <lexp> ']')+ ;                               \n" \
"prop       : <factor> ('.' <ident>)+ ;                                  \n" \
"postfix    : (<methodcall> | <item> | <prop> | <anoncall>                 " \
"         | <call> | <factor>) ;                                         \n" \
"call       : <ident> '(' <lexp>? (',' <lexp>)* ')' ;                    \n" \
"anoncall   : <factor> '(' <lexp>? (',' <lexp>)* ')' ;                   \n" \
"methodcall : <prop> '(' <lexp>? (',' <lexp>)* ')' ;                     \n" \
"array      : '[' <lexp>? (',' <lexp>)* ']' ;                            \n" \
"pair       : <string> ':' <lexp> ;                                      \n" \
"hash       : '{' <pair>? (',' <pair>)* '}' ;                            \n" \
"ident      : /[a-zA-Z_][a-zA-Z0-9_]*/ ;                                 \n" \
"pows       : <postfix> (\"**\" <pows>)? ;                               \n" \
"term       : (<lambda> | <pows> (('*' | '/' | '%') <pows>)*) ;          \n" \
"arith      : <term> (('+' | '-') <term>)* ;                             \n" \
"bits       : <arith> ((\"<<\" | \">>\" | '&' | '^' | '|') <arith>)* ;   \n" \
"cmpexp     : <bits> ((\"!=\" | \"==\" | \"<=\" | \"<\" | \">=\"           " \
"         | \">\" | \"=~\") <bits>)? ;                                   \n" \
"logic      : <cmpexp> ((\"&&\" | \"||\") <cmpexp>)* ;                   \n" \
"lexp       : <logic> ('?' <lexp> ':' <lexp>)? ;                         \n" \
"incdec     : <ident> (\"++\" | \"--\") ;                                \n" \
"let_o      : (\"=\" | \"+=\" | \"-=\" | \"*=\" | \"/=\" | \"%=\") ;     \n" \
"let_v      : <ident> <let_o> <lexp> ';' ;                               \n" \
"let_a      : <item> <let_o> <lexp> ';' ;                                \n" \
"let_p      : <prop> <let_o> <lexp> ';' ;                                \n" \
"else_if    : \"else\" \"if\" '(' <lexp> ')' '{' <stmts> '}' ;           \n" \
"else       : \"else\" '{' <stmts> '}' ;                                 \n" \
"if_stmt    : \"if\" '(' <lexp> ')' '{' <stmts> '}' ;                    \n" \
"if         : <if_stmt> <else_if>* <else>? ;                             \n" \
"while      : \"while\" '(' <lexp> ')' '{' <stmts> '}' ;                 \n" \
"for_in     : \"for\" '(' <ident> \"in\" <lexp> ')' '{' <stmts> '}' ;    \n" \
"let_s      : <ident> <let_o> <lexp> ;                                   \n" \
"for_c      : \"for\" '(' (<let_v> | <var> | ';') <lexp>? ';'              " \
"         (<incdec> | <let_s>)? ')' '{' <stmts> '}' ;                    \n" \
"var        : \"var\" <ident> '=' <lexp> ';' ;                           \n" \
"vararg     : \"...\" ;                                                  \n" \
"lambda     : \"func\"                                                     " \
"         '(' <ident>? (<vararg> | (',' <ident>)*) ')' '{' <stmts> '}' ; \n" \
"func       : \"func\" <ident>                                             " \
"         '(' <ident>? (<vararg> | (',' <ident>)*) ')' '{' <stmts> '}' ; \n" \
"template   : (<var> | <func>)* ;                                        \n" \
"class      : \"class\" <ident> '{' <template> '}' ;                     \n" \
"class_ext  : \"class\" <ident> \"extends\" <ident> '{' <template> '}' ; \n" \
"new        : \"new\" <ident> '(' <lexp>? (',' <lexp>)* ')' ;            \n" \
"                                                                        \n" \
"throw      : \"throw\" <lexp> ';' ;                                     \n" \
"try        : \"try\" '{' <stmts> '}'                                      " \
"         \"catch\" ('(' <ident> ')')? '{' <stmts> '}' ;                 \n" \
"break      : \"break\" ';' ;                                            \n" \
"continue   : \"continue\" ';' ;                                         \n" \
"return     : \"return\" <lexp> ';' ;                                    \n" \
"comment    : /#[^\n]*/ ;                                                \n" \
"eof        : /$/ ;                                                      \n" \
"stmt       : (<let_v> | <let_a> | <let_p> | <incdec> ';' | <var> | <if>   " \
"         | <while> | <for_in> | <for_c>                                   " \
"         | <func> | <class_ext> | <class> | <return> | <break>          \n" \
"         | <continue> | <try> | <throw> | <lexp> ';' | <comment>) ;     \n" \
"stmts      : <stmt>* ;                                                  \n" \
"program    : <stmts> <eof> ;                                            \n"

#define is_a(t, a) (strstr(t->tag, a) != NULL)

enum {
  ORE_TAG_UNKNOWN = 0,
  ORE_TAG_EOF, ORE_TAG_TRUE, ORE_TAG_FALSE, ORE_TAG_NIL,
  ORE_TAG_NUMBER, ORE_TAG_STRING, ORE_TAG_ARRAY, ORE_TAG_HASH,
  ORE_TAG_REGEXP, ORE_TAG_ITEM, ORE_TAG_PROP, ORE_TAG_IDENT,
  ORE_TAG_CALL, ORE_TAG_NEW, ORE_TAG_LAMBDA,
  ORE_TAG_FACTOR, ORE_TAG_LEXP_TERM,
  ORE_TAG_LET_V, ORE_TAG_LET_A, ORE_TAG_LET_P, ORE_TAG_INCDEC,
  ORE_TAG_VAR, ORE_TAG_FUNC,
  ORE_TAG_CLASS_EXT, ORE_TAG_CLASS,
  ORE_TAG_RETURN, ORE_TAG_BREAK, ORE_TAG_CONTINUE,
  ORE_TAG_TRY, ORE_TAG_THROW,
  ORE_TAG_IF_STMT, ORE_TAG_IF, ORE_TAG_WHILE, ORE_TAG_FOR_IN, ORE_TAG_FOR_C,
  ORE_TAG_STMTS, ORE_TAG_STMT, ORE_TAG_SEMI,
};

static int
ore_classify_tag(mpc_ast_t* t) {
  if (t->tag[0] == '>') return ORE_TAG_STMTS;
  if (is_a(t, "eof") || is_a(t, "comment")) return ORE_TAG_EOF;
  if (is_a(t, "true")) return ORE_TAG_TRUE;
  if (is_a(t, "false")) return ORE_TAG_FALSE;
  if (is_a(t, "nil")) return ORE_TAG_NIL;
  if (is_a(t, "number")) return ORE_TAG_NUMBER;
  if (is_a(t, "string")) return ORE_TAG_STRING;
  if (is_a(t, "array")) return ORE_TAG_ARRAY;
  if (is_a(t, "hash")) return ORE_TAG_HASH;
  if (is_a(t, "regexp")) return ORE_TAG_REGEXP;
  if (is_a(t, "item")) return ORE_TAG_ITEM;
  if (is_a(t, "prop")) return ORE_TAG_PROP;
  if (is_a(t, "ident")) return ORE_TAG_IDENT;
  if (is_a(t, "call")) return ORE_TAG_CALL;
  if (is_a(t, "new")) return ORE_TAG_NEW;
  if (is_a(t, "lambda")) return ORE_TAG_LAMBDA;
  if (is_a(t, "factor")) return ORE_TAG_FACTOR;
  if (is_a(t, "lexp") || is_a(t, "term") || is_a(t, "arith") ||
      is_a(t, "cmpexp") || is_a(t, "logic") ||
      is_a(t, "pows") || is_a(t, "bits")) return ORE_TAG_LEXP_TERM;
  if (is_a(t, "incdec")) return ORE_TAG_INCDEC;
  if (is_a(t, "let_v") || is_a(t, "let_s")) return ORE_TAG_LET_V;
  if (is_a(t, "let_a")) return ORE_TAG_LET_A;
  if (is_a(t, "let_p")) return ORE_TAG_LET_P;
  if (is_a(t, "var")) return ORE_TAG_VAR;
  if (is_a(t, "func")) return ORE_TAG_FUNC;
  if (is_a(t, "class_ext")) return ORE_TAG_CLASS_EXT;
  if (is_a(t, "class")) return ORE_TAG_CLASS;
  if (is_a(t, "return")) return ORE_TAG_RETURN;
  if (is_a(t, "break")) return ORE_TAG_BREAK;
  if (is_a(t, "continue")) return ORE_TAG_CONTINUE;
  if (is_a(t, "throw")) return ORE_TAG_THROW;
  if (is_a(t, "try")) return ORE_TAG_TRY;
  if (is_a(t, "if_stmt")) return ORE_TAG_IF_STMT;
  if (is_a(t, "if")) return ORE_TAG_IF;
  if (is_a(t, "while")) return ORE_TAG_WHILE;
  if (is_a(t, "for_in")) return ORE_TAG_FOR_IN;
  if (is_a(t, "for_c")) return ORE_TAG_FOR_C;
  if (is_a(t, "stmts") || is_a(t, "template")) return ORE_TAG_STMTS;
  if (is_a(t, "stmt")) return ORE_TAG_STMT;
  if (is_a(t, "char") && !strcmp(t->contents, ";")) return ORE_TAG_SEMI;
  return ORE_TAG_UNKNOWN;
}

static int
ore_get_tag(ore_tag_cache_t* cache, mpc_ast_t* t) {
  khiter_t k = kh_get(tag, cache, (khint64_t)(uintptr_t)t);
  if (k != kh_end(cache)) return kh_value(cache, k);
  int tag_type = ore_classify_tag(t);
  int r;
  k = kh_put(tag, cache, (khint64_t)(uintptr_t)t, &r);
  kh_value(cache, k) = tag_type;
  return tag_type;
}

typedef struct {
  mpc_ast_t *root;
  mpc_parser_t *program;
} ore_parse_context;

typedef klist_t(value) ore_array_t;
typedef kliter_t(value) ore_array_iter_t;
typedef khash_t(value) ore_hash_t;
typedef khiter_t ore_hash_iter_t;

KHASH_MAP_INIT_STR(cfunc, ore_cfunc_t)

static ore_value ore_call(ore_context*, mpc_ast_t*);
static ore_value ore_eval(ore_context*, mpc_ast_t*);
static char* ore_value_to_str(ore_context*, ore_value);

static void
ore_init_func(ore_func* fn, ore_context* ore, mpc_ast_t* t) {
  int i;
  int argc = 0;
  int vararg = 0;

  fn->ore = ore;
  fn->num_in = 0;
  fn->max_in = 0;
  fn->args_begin = -1;
  fn->args_end = -1;
  fn->x.o = t;
  fn->body = NULL;
  fn->u = NULL;

  for (i = 0; i < t->children_num; i++) {
    mpc_ast_t* child = t->children[i];
    if (is_a(child, "char")) {
      if (child->contents[0] == '(') {
        fn->args_begin = i + 1;
      } else if (child->contents[0] == ')') {
        fn->args_end = i;
      } else if (child->contents[0] == '{') {
        if (i + 1 < t->children_num && t->children[i + 1]->contents[0] != '}')
          fn->body = t->children[i + 1];
        break;
      }
      continue;
    }
    if (fn->args_begin >= 0 && fn->args_end < 0) {
      if (is_a(child, "ident")) argc++;
      else if (is_a(child, "vararg")) vararg = 1;
    }
  }

  fn->num_in = argc;
  fn->max_in = vararg ? -1 : argc;
}

int verbose = 0;

static const char*
ore_kind(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_NIL:
      return "nil";
    case ORE_TYPE_BOOL:
      return "bool";
    case ORE_TYPE_INT:
      return "int";
    case ORE_TYPE_FLOAT:
      return "float";
    case ORE_TYPE_STRING:
      return "string";
    case ORE_TYPE_CFUNC:
      return "func";
    case ORE_TYPE_FUNC:
      return "func";
    case ORE_TYPE_ARRAY:
      return "array";
    case ORE_TYPE_HASH:
      return "hash";
    case ORE_TYPE_REGEXP:
      return "regexp";
    case ORE_TYPE_ENV:
      return "env";
    case ORE_TYPE_CLASS:
      return "class";
    case ORE_TYPE_CCLASS:
      return "class";
    case ORE_TYPE_OBJECT:
      return "object";
  }
  return "unknown";
}

static void
ore_value_real_free(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_STRING:
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
    case ORE_TYPE_REGEXP:
      if (verbose)
        printf("free regexp %s\n", v.v.r->p);
      free(v.v.r->p);
      free(v.v.r);
      v.v.r = NULL;
      break;
    case ORE_TYPE_ENV:
      if (verbose)
        printf("free env %p\n", v.v.e->p);
      ore_destroy((ore_context*) v.v.e->p);
      v.v.e = NULL;
      break;
    case ORE_TYPE_OBJECT:
      if (verbose)
        printf("free object %p\n", v.v.o);
      ore_value terminate = ore_prop(v.v.o->e, "__terminate__");
      if (terminate.t == ORE_TYPE_FUNC)
        ore_func_call(v.v.o->e, terminate, 0, NULL);
      free(v.v.o);
      v.v.o = NULL;
      break;
    default:
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
    case ORE_TYPE_STRING:
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
    case ORE_TYPE_REGEXP:
      v.v.r->ref++;
      if (verbose)
        printf("ref regexp %d %p\n", v.v.r->ref, v.v.r->p);
      break;
    case ORE_TYPE_ENV:
      v.v.e->ref++;
      if (verbose)
        printf("ref env %d %p\n", v.v.e->ref, v.v.e->p);
      break;
    case ORE_TYPE_OBJECT:
      v.v.o->ref++;
      if (verbose)
        printf("ref object %d %p\n", v.v.o->ref, v.v.o);
      break;
    default: 
      break;
  }
}

void
ore_value_unref(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_STRING:
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
    case ORE_TYPE_REGEXP:
      if (verbose)
        printf("unref regexp %d %p\n", v.v.r->ref, v.v.r->p);
      if (--v.v.r->ref <= 0)
        ore_value_real_free(v);
      break;
    case ORE_TYPE_ENV:
      if (verbose)
        printf("unref env %d %p\n", v.v.e->ref, v.v.e->p);
      if (--v.v.e->ref <= 0)
        ore_value_real_free(v);
      break;
    case ORE_TYPE_OBJECT:
      if (verbose)
        printf("unref object %d %p\n", v.v.o->ref, v.v.o);
      if (--v.v.o->ref <= 0)
        ore_value_real_free(v);
      break;
    default: 
      break;
  }
}

static const char*
ore_value_str_ptr(ore_value v) {
  return v.v.s->p;
}

ore_value
ore_value_nil() {
  ore_value v = { ORE_TYPE_NIL };
  return v;
}

ore_value
ore_value_true() {
  ore_value v = { ORE_TYPE_BOOL };
  v.v.b = !0;
  return v;
}

ore_value
ore_value_false() {
  ore_value v = { ORE_TYPE_BOOL };
  v.v.b = 0;
  return v;
}

static int
ore_is_true(ore_value v) {
  switch (v.t) {
    case ORE_TYPE_BOOL:
      return v.v.b != 0;
    case ORE_TYPE_INT:
      return v.v.i != 0;
    case ORE_TYPE_FLOAT:
      return v.v.d != 0;
    case ORE_TYPE_STRING:
      return v.v.s->l > 0;
    case ORE_TYPE_ARRAY:
      {
        ore_array_t* a = (ore_array_t*) v.v.a->p;
        return a->size > 0;
      }
    case ORE_TYPE_HASH:
      {
        ore_hash_t* h = (ore_hash_t*) v.v.h->p;
        ore_hash_iter_t k;
        int n = 0;
        for (k = kh_begin(h); k != kh_end(h); k++) {
          if (!kh_exist(h, k)) continue;
          n++;
        }
        return n > 0;
      }
    case ORE_TYPE_REGEXP:
      return v.v.r->l > 0;
    case ORE_TYPE_ENV:
      return 1; // TODO
    case ORE_TYPE_CLASS:
      return 1; // TODO
    case ORE_TYPE_CCLASS:
      return 1; // TODO
    case ORE_TYPE_OBJECT:
      return 1; // TODO
    default:
      break;
  }
  return 0;
}

static void
ore_err_print(mpc_err_t* err) {
  if (err->failure)
    fprintf(stderr, "%s: error: %s\n", err->filename, err->failure);
  else
    fprintf(stderr, "%s:%ld:%ld: syntax error\n", err->filename, err->state.row+1, err->state.col+1);
}

static ore_value
ore_parse_num(ore_context* ore, const char* s) {
  ore_value v = {0};
  const char* p = *s == '-' ? s + 1 : s;
  if (*p == '0' && *(p+1) == 'x') {
    v.t = ORE_TYPE_INT;
    v.v.i = strtol(s, NULL, 16);
  } else if (!strchr(s, '.') && !strchr(s, 'e')) {
    v.t = ORE_TYPE_INT;
    v.v.i = atoi(s);
  } else {
    v.t = ORE_TYPE_FLOAT;
    v.v.d = atof(s);
  }
  return v;
}

static ore_value
ore_value_array_from_klist(ore_context* ore, ore_array_t* p) {
  ore_value v = { ORE_TYPE_ARRAY };
  v.v.a = (ore_array*) malloc(sizeof(ore_array));
  if (!v.v.a) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.a->ref = 0;
  v.v.a->p = p;
  return v;
}

static ore_value
ore_value_hash_from_khash(ore_context* ore, ore_hash_t* p) {
  ore_value v = { ORE_TYPE_HASH };
  v.v.h = (ore_hash*) malloc(sizeof(ore_hash));
  if (!v.v.h) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.h->ref = 0;
  v.v.h->p = p;
  return v;
}

static ore_value
ore_define_class(ore_context* ore, mpc_ast_t* tn, mpc_ast_t* tb, const char* base) {
  ore_value v = { ORE_TYPE_CLASS };
  v.v.c = (ore_class*) malloc(sizeof(ore_class));
  if (!v.v.c) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.c->n = strdup(tn->contents);
  if (!v.v.c->n) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.c->t = tb;
  v.v.c->b = base;
  ore_context* g = ore;
  while (g->parent) g = g->parent;
  ore_define(g, v.v.c->n, v);
  return v;
}

static mpc_ast_t*
ore_find_statements(mpc_ast_t* t) {
  int i;
  if (is_a(t, "template")) return t;
  for (i = 0; i < t->children_num; i++) {
    if (is_a(t->children[i], "char") && t->children[i]->contents[0] == '{') {
      if (t->children[i+1]->contents[0] != '}') {
        return t->children[i+1];
      }
    }
  }
  return NULL;
}

static int
ore_call_args_begin(mpc_ast_t* t) {
  int i;
  for (i = 0; i < t->children_num; i++) {
    if (is_a(t->children[i], "char") && t->children[i]->contents[0] == '(')
      return i + 1;
  }
  return t->children_num;
}

static int
ore_call_num_args(mpc_ast_t* t) {
  return (t->children_num - ore_call_args_begin(t)) / 2;
}

static ore_value*
ore_bind_args(ore_context* ore, mpc_ast_t* f, ore_context* this, mpc_ast_t* t) {
  int num_in = ore_call_num_args(t), n = 0, i;
  ore_value* args = (ore_value*) malloc(sizeof(ore_value) * num_in);
  if (!args) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return NULL;
  }
  for (i = 0; i < num_in; i++) {
    args[i] = ore_value_nil();
  }
  for (i = ore_call_args_begin(t); i < t->children_num - 1; i += 2) {
    args[n++] = ore_eval(ore, t->children[i]);
    if (ore->err != ORE_ERROR_NONE)
      return args;
  }

  ore_func fn;
  ore_init_func(&fn, NULL, f);
  n = 0;
  for (i = fn.args_begin; i >= 0 && i < fn.args_end; i++) {
    if (is_a(f->children[i], "vararg")) {
      ore_array_t* a = kl_init(value);
      int j;
      for (j = 0; j < num_in; j++) {
        *kl_pushp(value, a) = args[j];
      }
      ore_define(this, f->children[i-1]->contents, ore_value_array_from_klist(ore, a));
    } else if (is_a(f->children[i], "ident")) {
      if (n < num_in)
        ore_define(this, f->children[i]->contents, args[n++]);
    }
  }
  return args;
}

static ore_value
ore_find_global(ore_context* ore, const char* name) {
  ore_context* g = ore;
  while (g->parent) g = g->parent;
  return ore_prop(g, name);
}

static ore_value
ore_class_new(ore_context* ore, ore_value clazz) {
  ore_value v = { ORE_TYPE_OBJECT };
  ore_context* this = ore_new(ore);
  v.v.o = (ore_object*) malloc(sizeof(ore_object));
  if (!v.v.o) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  if (clazz.v.c->b != NULL) {
    ore_value bc = ore_find_global(ore, clazz.v.c->b);
    if (bc.t == ORE_TYPE_CLASS)
      ore_define(this, "super", ore_class_new(this, bc));
  }
  v.v.o->t = clazz.t;
  v.v.o->c = clazz.v.c;
  v.v.o->e = this;
  v.v.o->ref = -1;
  ore_eval(this, ore_find_statements(clazz.v.c->t));
  ore_define(this, "this", v);
  return v;
}

static ore_value
ore_object_new(ore_context* ore, mpc_ast_t* t) {
  ore_value clazz = ore_find_global(ore, t->children[1]->contents);
  ore_value v = { ORE_TYPE_OBJECT };
  switch (clazz.t) {
    case ORE_TYPE_CLASS:
      v = ore_class_new(ore, clazz);
      ore_value initialize = ore_prop(v.v.o->e, "__initialize__");
      if (initialize.t == ORE_TYPE_FUNC) {
        ore_value* args = ore_bind_args(ore, initialize.v.f.x.o, v.v.o->e, t);
        if (ore->err == ORE_ERROR_NONE)
          ore_func_call(v.v.o->e, initialize, ore_call_num_args(t), args);
        free(args);
      }
      break;
    case ORE_TYPE_CCLASS:
      {
        v.v.o = (ore_object*) malloc(sizeof(ore_object));
        if (!v.v.o) {
          fprintf(stderr, "failed to allocate memory\n");
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
        v.v.o->t = clazz.t;
        v.v.o->c = clazz.v.x;
        v.v.o->e = clazz.v.x->e;
        v.v.o->ref = -1;
        /*
        ore_value initialize = ore_prop(this, "__initialize__");
        if (initialize.t == ORE_TYPE_FUNC) {
          ore_value* args = ore_bind_args(ore, initialize.v.f.x.o, this, t);
          ore_func_call(this, initialize, 0, NULL);
        }
        */
      }
      break;
    default:
      fprintf(stderr, "unknown class '%s'\n", t->children[1]->contents);
      ore->err = ORE_ERROR_EXCEPTION;
      return ore_value_nil();
  }
  return v;
}

ore_value
ore_value_env_from_context(ore_context* p) {
  ore_value v = { ORE_TYPE_ENV };
  v.v.e = (ore_env*) malloc(sizeof(ore_env));
  if (!v.v.e) {
    fprintf(stderr, "failed to allocate memory\n");
    p->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.e->ref = 0;
  v.v.e->p = p;
  return v;
}

static ore_value
ore_value_str_from_ptr(ore_context* ore, char* p, int l) {
  if (p == NULL) return ore_value_nil();
  ore_value v = { ORE_TYPE_STRING };
  v.v.s = (ore_string*) malloc(sizeof(ore_string));
  if (!v.v.s) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.s->ref = 0;
  v.v.s->l = l < 0 ? strlen(p) : l;
  v.v.s->p = p;
  return v;
}

#if 0
static ore_value
ore_value_str_from_ptr_dup(ore_context* ore, char* p, int l) {
  ore_value v = { ORE_TYPE_STRING };
  v.v.s = (ore_string*) malloc(sizeof(ore_string));
  if (!v.v.s) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.s->ref = 0;
  v.v.s->l = l < 0 ? strlen(p) : l;
  char* t = malloc(v.v.s->l + 1);
  strcpy(t, p);
  v.v.s->p = t;
  return v;
}
#endif

static ore_value
ore_parse_str(ore_context* ore, const char* s) {
  ore_value v = { ORE_TYPE_STRING };
  char* t = strdup(s);
  if (!t) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  char* p = t + 1;
  char* ps = p;
  int n = 0;
  while (*p) {
    if (*p == '\\' && *(p+1)) {
      p++;
      switch (*p) {
        case 'b': *ps = '\b'; break;
        case 'f': *ps = '\f'; break;
        case 'r': *ps = '\r'; break;
        case 'n': *ps = '\n'; break;
        case 't': *ps = '\t'; break;
        default:
         *ps = *p;
      }
    } else
      *ps = *p;
    p++; ps++; n++;
  }
  *p = 0x00;
  size_t l = n - 1;
  v.v.s = (ore_string*) malloc(sizeof(ore_string));
  if (!v.v.s) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.s->ref = 0;
  v.v.s->l = l;
  v.v.s->p = calloc(1, l + 1);
  strncpy(v.v.s->p, t + 1, l);
  free(t);
  return v;
}

static ore_value
ore_cfunc_len(ore_context* ore, int num_in, ore_value* args, void* u) {
  ore_value v = { ORE_TYPE_INT };
  switch (args[0].t) {
    case ORE_TYPE_STRING:
      v.v.i = strlen(args[0].v.s->p);
      return v;
    case ORE_TYPE_ARRAY:
      {
        ore_array_t* a = (ore_array_t*) args[0].v.a->p;
        v.v.i = a->size;
      }
      return v;
    case ORE_TYPE_HASH:
      {
        ore_hash_t* h = (ore_hash_t*) args[0].v.h->p;
        ore_hash_iter_t k;
        int n = 0;
        for (k = kh_begin(h); k != kh_end(h); k++) {
          if (kh_exist(h, k)) n++;
        }
        v.v.i = n;
      }
      return v;
    default:
      break;
  }
  fprintf(stderr, "argument should be string, array or hash\n");
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

static ore_value
ore_cfunc_keys(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_HASH) {
    fprintf(stderr, "argument should be hash\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_hash_t* h = (ore_hash_t*) args[0].v.h->p;
  ore_array_t* a = kl_init(value);
  ore_hash_iter_t k;
  for (k = kh_begin(h); k != kh_end(h); k++) {
    if (!kh_exist(h, k)) continue;
    char* p = strdup(kh_key(h, k));
    *kl_pushp(value, a) = ore_value_str_from_ptr(ore, p, -1);
  }
  return ore_value_array_from_klist(ore, a);
}

static ore_value
ore_cfunc_values(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_HASH) {
    fprintf(stderr, "argument should be hash\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_hash_t* h = (ore_hash_t*) args[0].v.h->p;
  ore_array_t* a = kl_init(value);
  ore_hash_iter_t k;
  for (k = kh_begin(h); k != kh_end(h); k++) {
    if (!kh_exist(h, k)) continue;
    ore_value_ref(kh_value(h, k));
    *kl_pushp(value, a) = kh_value(h, k);
  }
  return ore_value_array_from_klist(ore, a);
}

static ore_value
ore_cfunc_has(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_HASH || args[1].t != ORE_TYPE_STRING) {
    fprintf(stderr, "arguments should be hash and string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_hash_t* h = (ore_hash_t*) args[0].v.h->p;
  khint_t k = kh_get(value, h, args[1].v.s->p);
  return k != kh_end(h) ? ore_value_true() : ore_value_false();
}

static ore_value
ore_cfunc_delete(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_HASH || args[1].t != ORE_TYPE_STRING) {
    fprintf(stderr, "arguments should be hash and string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_hash_t* h = (ore_hash_t*) args[0].v.h->p;
  khint_t k = kh_get(value, h, args[1].v.s->p);
  if (k == kh_end(h))
    return ore_value_false();
  ore_value_unref(kh_value(h, k));
  kh_del(value, h, k);
  return ore_value_true();
}

static ore_value
ore_cfunc_range(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_INT) {
    fprintf(stderr, "argument should be int\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_array_t* a = kl_init(value);
  int from = 0;
  int to = 0;
  if (num_in == 2) {
    if (args[1].t != ORE_TYPE_INT) {
      fprintf(stderr, "argument should be int\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return ore_value_nil();
    }
    from = args[0].v.i;
    to = args[1].v.i;
  } else {
    to = args[0].v.i - 1;
  }
  int j;
  for (j = from; j <= to; j++) {
    ore_value v = { ORE_TYPE_INT };
    v.v.i = j;
    *kl_pushp(value, a) = v;
  }
  return ore_value_array_from_klist(ore, a);
}

static ore_value
ore_cfunc_push(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_ARRAY) {
    fprintf(stderr, "argument should be array\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_array_t* a = (ore_array_t*) args[0].v.a->p;
  int i;
  for (i = 1; i < num_in; i++) {
    ore_value_ref(args[i]);
    *kl_pushp(value, a) = args[i];
  }
  return args[0];
}

static ore_value
ore_cfunc_pop(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_ARRAY) {
    fprintf(stderr, "argument should be array\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_array_t* a = (ore_array_t*) args[0].v.a->p;
  if (a->size == 0)
    return ore_value_nil();
  ore_array_iter_t* k = kl_begin(a);
  while (kl_next(k) != kl_end(a))
    k = kl_next(k);
  ore_value last = kl_val(k);
  kl_val(k) = ore_value_nil();
  kmp_free(value, a->mp, a->tail);
  a->tail = k;
  k->next = 0;
  a->size--;
  return last;
}

static ore_value
ore_cfunc_slice(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_ARRAY ||
      args[1].t != ORE_TYPE_INT || args[2].t != ORE_TYPE_INT) {
    fprintf(stderr, "arguments should be array, int, int\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_array_t* a = (ore_array_t*) args[0].v.a->p;
  int from = args[1].v.i, to = args[2].v.i, n = 0;
  ore_array_t* r = kl_init(value);
  ore_array_iter_t* k;
  for (k = kl_begin(a); k != kl_end(a); k = kl_next(k), n++) {
    if (n < from || n >= to) continue;
    ore_value_ref(kl_val(k));
    *kl_pushp(value, r) = kl_val(k);
  }
  return ore_value_array_from_klist(ore, r);
}

static int
ore_sort_cmp(const void* pa, const void* pb) {
  const ore_value* x = (const ore_value*) pa;
  const ore_value* y = (const ore_value*) pb;
  if (x->t == ORE_TYPE_STRING && y->t == ORE_TYPE_STRING)
    return strcmp(x->v.s->p, y->v.s->p);
  double dx = x->t == ORE_TYPE_INT ? (double) x->v.i : x->v.d;
  double dy = y->t == ORE_TYPE_INT ? (double) y->v.i : y->v.d;
  return dx < dy ? -1 : dx > dy ? 1 : 0;
}

static ore_value
ore_cfunc_sort(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_ARRAY) {
    fprintf(stderr, "argument should be array\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_array_t* a = (ore_array_t*) args[0].v.a->p;
  size_t n = a->size, i = 0;
  ore_value* buf = (ore_value*) malloc(sizeof(ore_value) * (n ? n : 1));
  if (!buf) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  int numeric = 0, str = 0;
  ore_array_iter_t* k;
  for (k = kl_begin(a); k != kl_end(a); k = kl_next(k)) {
    ore_value v = kl_val(k);
    if (v.t == ORE_TYPE_INT || v.t == ORE_TYPE_FLOAT) numeric++;
    else if (v.t == ORE_TYPE_STRING) str++;
    else {
      fprintf(stderr, "array is not sortable\n");
      ore->err = ORE_ERROR_EXCEPTION;
      free(buf);
      return ore_value_nil();
    }
    buf[i++] = v;
  }
  if (numeric && str) {
    fprintf(stderr, "cannot sort mixed numbers and strings\n");
    ore->err = ORE_ERROR_EXCEPTION;
    free(buf);
    return ore_value_nil();
  }
  qsort(buf, n, sizeof(ore_value), ore_sort_cmp);
  ore_array_t* r = kl_init(value);
  for (i = 0; i < n; i++) {
    ore_value_ref(buf[i]);
    *kl_pushp(value, r) = buf[i];
  }
  free(buf);
  return ore_value_array_from_klist(ore, r);
}

static ore_value
ore_cfunc_substr(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_STRING ||
      args[1].t != ORE_TYPE_INT || args[2].t != ORE_TYPE_INT) {
    fprintf(stderr, "arguments should be string, int, int\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  int l = args[0].v.s->l;
  int from = args[1].v.i;
  int n = args[2].v.i;
  if (from < 0) from = 0;
  if (from > l) from = l;
  if (n < 0 || from + n > l) n = l - from;
  char* p = calloc(1, n + 1);
  if (!p) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  memcpy(p, args[0].v.s->p + from, n);
  return ore_value_str_from_ptr(ore, p, n);
}

static ore_value
ore_cfunc_index(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_STRING || args[1].t != ORE_TYPE_STRING) {
    fprintf(stderr, "arguments should be string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_value v = { ORE_TYPE_INT };
  const char* found = strstr(args[0].v.s->p, args[1].v.s->p);
  v.v.i = found ? (int) (found - args[0].v.s->p) : -1;
  return v;
}

static ore_value
ore_cfunc_split(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_STRING || args[1].t != ORE_TYPE_STRING) {
    fprintf(stderr, "arguments should be string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  const char* s = args[0].v.s->p;
  const char* sep = args[1].v.s->p;
  int seplen = args[1].v.s->l;
  ore_array_t* a = kl_init(value);
  if (seplen == 0) {
    while (*s) {
      char* p = calloc(1, 2);
      p[0] = *s;
      *kl_pushp(value, a) = ore_value_str_from_ptr(ore, p, 1);
      s++;
    }
  } else {
    for (;;) {
      const char* found = strstr(s, sep);
      int n = found ? (int) (found - s) : (int) strlen(s);
      char* p = calloc(1, n + 1);
      memcpy(p, s, n);
      *kl_pushp(value, a) = ore_value_str_from_ptr(ore, p, n);
      if (!found) break;
      s = found + seplen;
    }
  }
  return ore_value_array_from_klist(ore, a);
}

static ore_value
ore_cfunc_join(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_ARRAY || args[1].t != ORE_TYPE_STRING) {
    fprintf(stderr, "arguments should be array and string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_array_t* a = (ore_array_t*) args[0].v.a->p;
  kstring_t ks = { 0, 0, NULL };
  ore_array_iter_t* k;
  ore_array_iter_t* b = kl_begin(a);
  for (k = b; k != kl_end(a); k = kl_next(k)) {
    if (k != b) kputs(args[1].v.s->p, &ks);
    char* s = ore_value_to_str(ore, kl_val(k));
    kputs(s, &ks);
    free(s);
  }
  if (ks.s == NULL)
    return ore_value_str_from_ptr(ore, calloc(1, 1), 0);
  return ore_value_str_from_ptr(ore, ks.s, ks.l);
}

static ore_value
ore_cfunc_replace(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_STRING ||
      args[1].t != ORE_TYPE_STRING || args[2].t != ORE_TYPE_STRING) {
    fprintf(stderr, "arguments should be string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  const char* s = args[0].v.s->p;
  const char* old = args[1].v.s->p;
  int oldlen = args[1].v.s->l;
  if (oldlen == 0) {
    fprintf(stderr, "empty search string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  kstring_t ks = { 0, 0, NULL };
  for (;;) {
    const char* found = strstr(s, old);
    if (!found) {
      kputs(s, &ks);
      break;
    }
    kputsn(s, found - s, &ks);
    kputs(args[2].v.s->p, &ks);
    s = found + oldlen;
  }
  if (ks.s == NULL)
    return ore_value_str_from_ptr(ore, calloc(1, 1), 0);
  return ore_value_str_from_ptr(ore, ks.s, ks.l);
}

static ore_value
ore_str_map_case(ore_context* ore, ore_value* args, int up) {
  if (args[0].t != ORE_TYPE_STRING) {
    fprintf(stderr, "argument should be string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  int l = args[0].v.s->l, i;
  char* p = calloc(1, l + 1);
  if (!p) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  for (i = 0; i < l; i++) {
    unsigned char c = (unsigned char) args[0].v.s->p[i];
    p[i] = up ? toupper(c) : tolower(c);
  }
  return ore_value_str_from_ptr(ore, p, l);
}

static ore_value
ore_cfunc_upper(ore_context* ore, int num_in, ore_value* args, void* u) {
  return ore_str_map_case(ore, args, 1);
}

static ore_value
ore_cfunc_lower(ore_context* ore, int num_in, ore_value* args, void* u) {
  return ore_str_map_case(ore, args, 0);
}

static ore_value
ore_cfunc_trim(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_STRING) {
    fprintf(stderr, "argument should be string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  const char* s = args[0].v.s->p;
  const char* e = s + args[0].v.s->l;
  while (s < e && isspace((unsigned char) *s)) s++;
  while (e > s && isspace((unsigned char) *(e - 1))) e--;
  int n = (int) (e - s);
  char* p = calloc(1, n + 1);
  if (!p) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  memcpy(p, s, n);
  return ore_value_str_from_ptr(ore, p, n);
}

static int
ore_value_num(ore_context* ore, ore_value v, double* d) {
  if (v.t == ORE_TYPE_INT) { *d = (double) v.v.i; return 0; }
  if (v.t == ORE_TYPE_FLOAT) { *d = v.v.d; return 0; }
  fprintf(stderr, "argument should be number\n");
  ore->err = ORE_ERROR_EXCEPTION;
  return -1;
}

static ore_value
ore_cfunc_to_int(ore_context* ore, int num_in, ore_value* args, void* u) {
  ore_value v = { ORE_TYPE_INT };
  switch (args[0].t) {
    case ORE_TYPE_INT:
      return args[0];
    case ORE_TYPE_FLOAT:
      v.v.i = (int) args[0].v.d;
      return v;
    case ORE_TYPE_BOOL:
      v.v.i = args[0].v.b ? 1 : 0;
      return v;
    case ORE_TYPE_STRING:
      v.v.i = (int) strtol(args[0].v.s->p, NULL, 0);
      return v;
    default:
      break;
  }
  fprintf(stderr, "cannot convert %s to int\n", ore_kind(args[0]));
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

static ore_value
ore_cfunc_to_float(ore_context* ore, int num_in, ore_value* args, void* u) {
  ore_value v = { ORE_TYPE_FLOAT };
  switch (args[0].t) {
    case ORE_TYPE_INT:
      v.v.d = (double) args[0].v.i;
      return v;
    case ORE_TYPE_FLOAT:
      return args[0];
    case ORE_TYPE_STRING:
      v.v.d = strtod(args[0].v.s->p, NULL);
      return v;
    default:
      break;
  }
  fprintf(stderr, "cannot convert %s to float\n", ore_kind(args[0]));
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

static ore_value
ore_cfunc_abs(ore_context* ore, int num_in, ore_value* args, void* u) {
  ore_value v = args[0];
  if (v.t == ORE_TYPE_INT) {
    if (v.v.i < 0) v.v.i = -v.v.i;
    return v;
  }
  if (v.t == ORE_TYPE_FLOAT) {
    v.v.d = fabs(v.v.d);
    return v;
  }
  fprintf(stderr, "argument should be number\n");
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

static ore_value
ore_cfunc_floor(ore_context* ore, int num_in, ore_value* args, void* u) {
  double d;
  if (ore_value_num(ore, args[0], &d)) return ore_value_nil();
  ore_value v = { ORE_TYPE_INT };
  v.v.i = (int) floor(d);
  return v;
}

static ore_value
ore_cfunc_ceil(ore_context* ore, int num_in, ore_value* args, void* u) {
  double d;
  if (ore_value_num(ore, args[0], &d)) return ore_value_nil();
  ore_value v = { ORE_TYPE_INT };
  v.v.i = (int) ceil(d);
  return v;
}

static ore_value
ore_cfunc_round(ore_context* ore, int num_in, ore_value* args, void* u) {
  double d;
  if (ore_value_num(ore, args[0], &d)) return ore_value_nil();
  ore_value v = { ORE_TYPE_INT };
  v.v.i = (int) floor(d + 0.5);
  return v;
}

static ore_value
ore_cfunc_sqrt(ore_context* ore, int num_in, ore_value* args, void* u) {
  double d;
  if (ore_value_num(ore, args[0], &d)) return ore_value_nil();
  ore_value v = { ORE_TYPE_FLOAT };
  v.v.d = sqrt(d);
  return v;
}

static ore_value
ore_cfunc_pow(ore_context* ore, int num_in, ore_value* args, void* u) {
  double x, y;
  if (ore_value_num(ore, args[0], &x)) return ore_value_nil();
  if (ore_value_num(ore, args[1], &y)) return ore_value_nil();
  ore_value v = { ORE_TYPE_FLOAT };
  v.v.d = pow(x, y);
  return v;
}

static ore_value
ore_cfunc_typeof(ore_context* ore, int num_in, ore_value* args, void* u) {
  return ore_value_str_from_ptr(ore, (char*) ore_kind(args[0]), -1);
}

static ore_value
ore_cfunc_load(ore_context* ore, int num_in, ore_value* args, void* u) {
  ore_parse_context* pctx = (ore_parse_context*) u;

  mpc_result_t result;
  if (args[0].t != ORE_TYPE_STRING) {
    fprintf(stderr, "argument should be string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  if (!mpc_parse_contents(ore_value_str_ptr(args[0]), pctx->program, &result)) {
    ore_err_print(result.error);
    mpc_err_delete(result.error);
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  if (verbose)
    mpc_ast_print(result.output);
  ore_eval(ore, result.output);
  mpc_ast_add_child(pctx->root, result.output);
  return ore_value_nil();
}

static ore_value
ore_cfunc_environ(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (num_in == 1) {
    if (args[0].t != ORE_TYPE_STRING || !args[0].v.s->p) {
      fprintf(stderr, "argument should be string\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return ore_value_nil();
    }
    return ore_value_str_from_ptr(ore, (char*) getenv(args[0].v.s->p), -1);
  } else {
    int i;
    ore_hash_t* h = kh_init(value);
    for (i = 0; environ[i]; i++) {
      const char* p = environ[i];
      if (p == NULL) break;
      const char* t = strchr(p, '=');
      if (t) {
        int r = 0;
        char* n = calloc(1, t-p+1);
        memcpy(n, p, t-p);
        ore_value val = ore_value_str_from_ptr(ore, (char*) t+1, -1);
        khint_t k = kh_put(value, h, n, &r);
        kh_value(h, k) = val;
      }
    }
    return ore_value_hash_from_khash(ore, h);
  }
  fprintf(stderr, "invalid argument\n");
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

static ore_value
ore_cfunc_exit(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_INT) {
    fprintf(stderr, "argument should be int\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  exit(args[0].v.i);
  return ore_value_nil();
}

static char*
ore_value_to_str(ore_context* ore, ore_value v) {
  kstring_t ks = { 0, 0, NULL };

  switch (v.t) {
    case ORE_TYPE_NIL:
      kputs("nil", &ks);
      break;
    case ORE_TYPE_BOOL:
      if (v.v.b)
        kputs("true", &ks);
      else
        kputs("false", &ks);
      break;
    case ORE_TYPE_INT:
      ksprintf(&ks, "%d", v.v.i);
      break;
    case ORE_TYPE_FLOAT:
      ksprintf(&ks, "%f", v.v.d);
      break;
    case ORE_TYPE_STRING:
      kputs(v.v.s->p, &ks);
      break;
    case ORE_TYPE_ARRAY:
      {
        ore_array_t* a = (ore_array_t*) v.v.a->p;
        ore_array_iter_t* k;
        ore_array_iter_t* b = kl_begin(a);
        kputc('[', &ks);
        for (k = b; k != kl_end(a); k = kl_next(k)) {
          if (k != b) {
            kputc(',', &ks);
          }
          char* s = ore_value_to_str(ore, kl_val(k));
          kputs(s, &ks);
          free(s);
        }
        kputc(']', &ks);
      }
      break;
    case ORE_TYPE_HASH:
      {
        ore_hash_t* h = (ore_hash_t*) v.v.h->p;
        ore_hash_iter_t k;
        int n = 0;
        kputc('{', &ks);
        for (k = kh_begin(h); k != kh_end(h); k++) {
          if (!kh_exist(h, k)) continue;
          if (n > 0) {
            kputc(',', &ks);
          }
          const char* key = kh_key(h, k);
          kputs(key, &ks);
          kputs(": ", &ks);
          kputs(ore_value_to_str(ore, kh_val(h, k)), &ks);
          n++;
        }
        kputc('}', &ks);
      }
      break;
    case ORE_TYPE_REGEXP:
      kputs(v.v.r->p, &ks);
      break;
    case ORE_TYPE_FUNC:
      ksprintf(&ks, "<func-0x%p>", v.v.f.x.o);
      break;
    case ORE_TYPE_CFUNC:
      ksprintf(&ks, "<func-0x%p>", v.v.f.x.c);
      break;
    case ORE_TYPE_ENV:
      ksprintf(&ks, "<env-0x%p>", v.v.e->p);
      break;
    case ORE_TYPE_CLASS:
      kputs(v.v.c->n, &ks);
      break;
    case ORE_TYPE_CCLASS:
      kputs(v.v.c->n, &ks);
      break;
    case ORE_TYPE_OBJECT:
      ksprintf(&ks, "<%s-0x%p>", ((ore_class*)v.v.o->c)->n, v.v.o);
      break;
    default:
      kputs("<unknown>", &ks);
      break;
  }
  return ks.s;
}

static ore_value
ore_cfunc_to_string(ore_context* ore, int num_in, ore_value* args, void* u) {
  return ore_value_str_from_ptr(ore, ore_value_to_str(ore, args[0]), -1);
}

static ore_value
ore_cfunc_print(ore_context* ore, int num_in, ore_value* args, void* u) {
  int i;
  for (i = 0; i < num_in; i++) {
    if (i != 0) printf(", ");
    ore_value v = args[i];
    char* s = ore_value_to_str(ore, v);
    printf("%s", s);
    free(s);
  }
  return ore_value_nil();
}

static ore_value
ore_cfunc_println(ore_context* ore, int num_in, ore_value* args, void* u) {
  ore_cfunc_print(ore, num_in, args, NULL);
  puts("");
  return ore_value_nil();
}

static void
ore_value_to_json(ore_context* ore, ore_value v, kstring_t* ks) {
  switch (v.t) {
    case ORE_TYPE_NIL:
      kputs("null", ks);
      break;
    case ORE_TYPE_BOOL:
      kputs(v.v.b ? "true" : "false", ks);
      break;
    case ORE_TYPE_INT:
      ksprintf(ks, "%d", v.v.i);
      break;
    case ORE_TYPE_FLOAT:
      ksprintf(ks, "%g", v.v.d);
      break;
    case ORE_TYPE_STRING:
      {
        const char* p = v.v.s->p;
        kputc('"', ks);
        while (*p) {
          switch (*p) {
            case '"': kputs("\\\"", ks); break;
            case '\\': kputs("\\\\", ks); break;
            case '\b': kputs("\\b", ks); break;
            case '\f': kputs("\\f", ks); break;
            case '\n': kputs("\\n", ks); break;
            case '\r': kputs("\\r", ks); break;
            case '\t': kputs("\\t", ks); break;
            default:
              if ((unsigned char)*p < 0x20) {
                ksprintf(ks, "\\u%04x", (unsigned char)*p);
              } else {
                kputc(*p, ks);
              }
          }
          p++;
        }
        kputc('"', ks);
      }
      break;
    case ORE_TYPE_ARRAY:
      {
        ore_array_t* a = (ore_array_t*) v.v.a->p;
        ore_array_iter_t* k;
        ore_array_iter_t* b = kl_begin(a);
        kputc('[', ks);
        for (k = b; k != kl_end(a); k = kl_next(k)) {
          if (k != b) kputc(',', ks);
          ore_value_to_json(ore, kl_val(k), ks);
        }
        kputc(']', ks);
      }
      break;
    case ORE_TYPE_HASH:
      {
        ore_hash_t* h = (ore_hash_t*) v.v.h->p;
        ore_hash_iter_t k;
        int n = 0;
        kputc('{', ks);
        for (k = kh_begin(h); k != kh_end(h); k++) {
          if (!kh_exist(h, k)) continue;
          if (n > 0) kputc(',', ks);
          /* key as JSON string */
          kputc('"', ks);
          const char* key = kh_key(h, k);
          while (*key) {
            if (*key == '"') kputs("\\\"", ks);
            else if (*key == '\\') kputs("\\\\", ks);
            else kputc(*key, ks);
            key++;
          }
          kputc('"', ks);
          kputc(':', ks);
          ore_value_to_json(ore, kh_val(h, k), ks);
          n++;
        }
        kputc('}', ks);
      }
      break;
    default:
      kputs("null", ks);
      break;
  }
}

static ore_value
ore_cfunc_json_encode(ore_context* ore, int num_in, ore_value* args, void* u) {
  kstring_t ks = { 0, 0, NULL };
  ore_value_to_json(ore, args[0], &ks);
  return ore_value_str_from_ptr(ore, ks.s, ks.l);
}

static void
json_skip_ws(const char** pp) {
  while (**pp == ' ' || **pp == '\t' || **pp == '\n' || **pp == '\r') (*pp)++;
}

static ore_value json_parse_value(ore_context* ore, const char** pp);

static ore_value
json_parse_string(ore_context* ore, const char** pp) {
  const char* p = *pp;
  if (*p != '"') {
    fprintf(stderr, "json_decode: expected '\"'\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  p++;
  kstring_t ks = { 0, 0, NULL };
  while (*p && *p != '"') {
    if (*p == '\\') {
      p++;
      switch (*p) {
        case '"': kputc('"', &ks); break;
        case '\\': kputc('\\', &ks); break;
        case '/': kputc('/', &ks); break;
        case 'b': kputc('\b', &ks); break;
        case 'f': kputc('\f', &ks); break;
        case 'n': kputc('\n', &ks); break;
        case 'r': kputc('\r', &ks); break;
        case 't': kputc('\t', &ks); break;
        case 'u':
          {
            unsigned int cp = 0;
            int i;
            for (i = 0; i < 4 && p[1]; i++) {
              p++;
              cp <<= 4;
              if (*p >= '0' && *p <= '9') cp |= *p - '0';
              else if (*p >= 'a' && *p <= 'f') cp |= *p - 'a' + 10;
              else if (*p >= 'A' && *p <= 'F') cp |= *p - 'A' + 10;
            }
            if (cp < 0x80) {
              kputc(cp, &ks);
            } else if (cp < 0x800) {
              kputc(0xc0 | (cp >> 6), &ks);
              kputc(0x80 | (cp & 0x3f), &ks);
            } else {
              kputc(0xe0 | (cp >> 12), &ks);
              kputc(0x80 | ((cp >> 6) & 0x3f), &ks);
              kputc(0x80 | (cp & 0x3f), &ks);
            }
          }
          break;
        default: kputc(*p, &ks); break;
      }
    } else {
      kputc(*p, &ks);
    }
    p++;
  }
  if (*p == '"') p++;
  *pp = p;
  if (ks.s == NULL) {
    char* empty = calloc(1, 1);
    return ore_value_str_from_ptr(ore, empty, 0);
  }
  return ore_value_str_from_ptr(ore, ks.s, ks.l);
}

static ore_value
json_parse_number(ore_context* ore, const char** pp) {
  const char* p = *pp;
  const char* start = p;
  int is_float = 0;
  if (*p == '-') p++;
  while (*p >= '0' && *p <= '9') p++;
  if (*p == '.') { is_float = 1; p++; while (*p >= '0' && *p <= '9') p++; }
  if (*p == 'e' || *p == 'E') { is_float = 1; p++; if (*p == '+' || *p == '-') p++; while (*p >= '0' && *p <= '9') p++; }
  *pp = p;
  if (is_float) {
    ore_value v = { ORE_TYPE_FLOAT };
    v.v.d = strtod(start, NULL);
    return v;
  } else {
    ore_value v = { ORE_TYPE_INT };
    v.v.i = (int) strtol(start, NULL, 10);
    return v;
  }
}

static ore_value
json_parse_array(ore_context* ore, const char** pp) {
  const char* p = *pp;
  p++; /* skip '[' */
  ore_array_t* a = kl_init(value);
  json_skip_ws(&p);
  if (*p != ']') {
    for (;;) {
      json_skip_ws(&p);
      ore_value elem = json_parse_value(ore, &p);
      if (ore->err) { *pp = p; return ore_value_nil(); }
      *kl_pushp(value, a) = elem;
      json_skip_ws(&p);
      if (*p == ',') { p++; continue; }
      break;
    }
  }
  if (*p == ']') p++;
  *pp = p;
  return ore_value_array_from_klist(ore, a);
}

static ore_value
json_parse_object(ore_context* ore, const char** pp) {
  const char* p = *pp;
  p++; /* skip '{' */
  ore_hash_t* h = kh_init(value);
  json_skip_ws(&p);
  if (*p != '}') {
    for (;;) {
      json_skip_ws(&p);
      ore_value key = json_parse_string(ore, &p);
      if (ore->err) { *pp = p; return ore_value_nil(); }
      json_skip_ws(&p);
      if (*p == ':') p++;
      json_skip_ws(&p);
      ore_value val = json_parse_value(ore, &p);
      if (ore->err) { *pp = p; return ore_value_nil(); }
      int r = 0;
      khint_t k = kh_put(value, h, key.v.s->p, &r);
      kh_value(h, k) = val;
      json_skip_ws(&p);
      if (*p == ',') { p++; continue; }
      break;
    }
  }
  if (*p == '}') p++;
  *pp = p;
  return ore_value_hash_from_khash(ore, h);
}

static ore_value
json_parse_value(ore_context* ore, const char** pp) {
  json_skip_ws(pp);
  const char* p = *pp;
  if (*p == '"') return json_parse_string(ore, pp);
  if (*p == '[') return json_parse_array(ore, pp);
  if (*p == '{') return json_parse_object(ore, pp);
  if (*p == 't' && strncmp(p, "true", 4) == 0) { *pp = p + 4; return ore_value_true(); }
  if (*p == 'f' && strncmp(p, "false", 5) == 0) { *pp = p + 5; return ore_value_false(); }
  if (*p == 'n' && strncmp(p, "null", 4) == 0) { *pp = p + 4; return ore_value_nil(); }
  if (*p == '-' || (*p >= '0' && *p <= '9')) return json_parse_number(ore, pp);
  fprintf(stderr, "json_decode: unexpected character '%c'\n", *p);
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

static ore_value
ore_cfunc_json_decode(ore_context* ore, int num_in, ore_value* args, void* u) {
  if (args[0].t != ORE_TYPE_STRING) {
    fprintf(stderr, "json_decode: argument should be string\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  const char* p = args[0].v.s->p;
  return json_parse_value(ore, &p);
}

static ore_value
ore_cfunc_dump_env(ore_context* ore, int num_in, ore_value* args, void* u) {
  int i, level = 0;
  while (ore) {
    for (i = 0; i < level; i++) printf(" ");
    ore_hash_t* h = (ore_hash_t*) ore->env;
    ore_hash_iter_t k;
    int n = 0;
    printf("%p : {", ore->env);
    for (k = kh_begin(h); k != kh_end(h); k++) {
      if (!kh_exist(h, k)) continue;
      if (n > 0) {
        printf(",");
      }
      const char* key = kh_key(h, k);
      printf("%s: ", key);
      ore_value pa[] = { kh_val(h, k) };
      ore_cfunc_print(ore, 1, pa, NULL);
      n++;
    }
    printf("}\n");
    ore = ore->parent;
    level++;
  }
  return ore_value_nil();
}

void
ore_p(ore_value v) {
  ore_cfunc_println(NULL, 1, &v, NULL);
}

ore_value
ore_prop(ore_context* ore, const char* name) {
  ore_context* p = ore;
  if (!p)
    return ore_value_nil();
  khint_t k;
  k = kh_get(value, p->env, name);
  if (k != kh_end(p->env)) {
    return kh_value(p->env, k);
  }
  return ore_value_nil();
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
  fprintf(stderr, "unknown identifier '%s'\n", name);
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

void
ore_set(ore_context* ore, const char* name, ore_value v) {
  khint_t k;
  int r = 0;
  ore_value old = ore_value_nil();
  while (ore) {
    k = kh_get(value, ore->env, name);
    if (k != kh_end(ore->env)) {
      old = kh_value(ore->env, k);
      ore_value_ref(v);
      ore_value_unref(old);
      k = kh_put(value, ore->env, name, &r);
      kh_value(ore->env, k) = v;
      return;
    }
    if (ore->parent == NULL) {
      if (k != kh_end(ore->env)) 
        old = kh_value(ore->env, k);
      ore_value_ref(v);
      ore_value_unref(old);
      k = kh_put(value, ore->env, name, &r);
      kh_value(ore->env, k) = v;
      return;
    }
    ore = ore->parent;
  }
}

void
ore_define(ore_context* ore, const char* name, ore_value v) {
  int r = 0;
  ore_value old = ore_value_nil();
  khint_t k = kh_get(value, ore->env, name);
  if (k != kh_end(ore->env))
    old = kh_value(ore->env, k);
  k = kh_put(value, ore->env, name, &r);
  ore_value_ref(v);
  kh_value(ore->env, k) = v;
  ore_value_unref(old);
}

ore_value
orex_define_class(ore_context* ore, const char* name) {
  ore_value v = { ORE_TYPE_CCLASS };
  v.v.x = (ore_cclass*) malloc(sizeof(ore_cclass));
  if (!v.v.x) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  v.v.x->n = name;
  v.v.x->e = ore_new(ore);
  ore_context* g = ore;
  while (g->parent) g = g->parent;
  ore_define(g, v.v.x->n, v);
  return v;
}

void
orex_define_method(ore_context* ore, ore_value clazz, const char* name, int num_in, int max_in, ore_cfunc_t c, void* u) {
  ore_value v = { ORE_TYPE_CFUNC };
  v.v.f.ore = clazz.v.x->e;
  v.v.f.num_in = num_in;
  v.v.f.max_in = max_in;
  v.v.f.args_begin = -1;
  v.v.f.args_end = -1;
  v.v.f.x.c = c;
  v.v.f.body = NULL;
  v.v.f.u = u;
  ore_define(clazz.v.x->e, name, v);
}

void
ore_define_cfunc(ore_context* ore, const char* name, int num_in, int max_in, ore_cfunc_t c, void* u) {
  ore_value v = { ORE_TYPE_CFUNC };
  v.v.f.ore = ore;
  v.v.f.num_in = num_in;
  v.v.f.max_in = max_in;
  v.v.f.args_begin = -1;
  v.v.f.args_end = -1;
  v.v.f.x.c = c;
  v.v.f.body = NULL;
  v.v.f.u = u;
  ore_define(ore, name, v);
}

ore_value
ore_func_call(ore_context* ore, ore_value fn, int num_in, ore_value* args) {
  if ((fn.v.f.num_in != -1 && num_in < fn.v.f.num_in) || (fn.v.f.max_in != -1 && num_in > fn.v.f.max_in)) {
    fprintf(stderr, "number of arguments mismatch: %d for %d\n",
      num_in, fn.v.f.num_in);
    fprintf(stderr, "number of arguments mismatch: %d for %d\n",
      num_in, fn.v.f.max_in);
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }

  ore_context* env = ore_new((ore_context*) fn.v.f.ore);
  mpc_ast_t* f = fn.v.f.x.o;
  int n = 0, i;
  for (i = fn.v.f.args_begin; i >= 0 && i < fn.v.f.args_end; i++) {
    if (is_a(f->children[i], "vararg")) {
      ore_array_t* a = kl_init(value);
      int j;
      for (j = 0; j < num_in; j++) {
        *kl_pushp(value, a) = args[j];
      }
      ore_define(env, f->children[i-1]->contents, ore_value_array_from_klist(ore, a));
    } else if (is_a(f->children[i], "ident")) {
      if (n < num_in)
        ore_define(env, f->children[i]->contents, args[n++]);
    }
  }
  ore_value v = ore_value_nil();
  if (fn.v.f.body) {
    v = ore_eval(env, fn.v.f.body);
    if (env->err == ORE_ERROR_EXCEPTION)
      ore->err = env->err;
  }
  return v;
}

static ore_value
ore_call(ore_context* ore, mpc_ast_t *t) {
  ore_value fn;
  const char* pfn = NULL;
  if (is_a(t->children[0], "ident")) {
    pfn = t->children[0]->contents;
    fn = ore_get(ore, pfn);
  } else if (is_a(t->children[0], "prop")) {
    pfn = t->children[0]->children[2]->contents;
    fn = ore_eval(ore, t->children[0]);
    if (ore->err != ORE_ERROR_NONE)
      return ore_value_nil();
    if (fn.t == ORE_TYPE_NIL) {
      // FIXME
      ore_context* tmp = ore;
      ore_value inst = ore_eval(tmp, t->children[0]->children[0]);
      while (inst.t == ORE_TYPE_OBJECT) {
        inst = ore_prop(inst.v.o->e, "super");
        if (inst.t == ORE_TYPE_NIL)
          break;
        fn = ore_prop(inst.v.o->e, pfn);
        if (fn.t == ORE_TYPE_FUNC || fn.t == ORE_TYPE_CFUNC) {
          break;
        }
      }
    }
  } else {
    pfn = "<anonymous>";
    fn = ore_eval(ore, t->children[0]);
  }
  if (ore->err != ORE_ERROR_NONE)
    return ore_value_nil();
  if (fn.t != ORE_TYPE_FUNC && fn.t != ORE_TYPE_CFUNC) {
    fprintf(stderr, "unknown function '%s'\n", pfn);
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
  ore_value v = ore_value_nil();
  switch (fn.t) {
    case ORE_TYPE_CFUNC:
      {
        int num_in = t->children_num / 2 - 1, n = 0, i;
        if (num_in < fn.v.f.num_in || (fn.v.f.max_in != -1 && num_in > fn.v.f.max_in)) {
          fprintf(stderr, "number of arguments mismatch: %d for %d\n",
            num_in, fn.v.f.num_in);
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
        ore_value* args = (ore_value*) malloc(sizeof(ore_value) * num_in);
        if (!args) {
          fprintf(stderr, "failed to allocate memory\n");
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
        for (i = 2; i < t->children_num - 1; i += 2) {
          args[n++] = ore_eval(ore, t->children[i]);
          if (ore->err != ORE_ERROR_NONE) {
            free(args);
            return ore_value_nil();
          }
        }
        v = ((ore_cfunc_t)fn.v.f.x.c) ((ore_context*) ore, num_in, args, fn.v.f.u);
        free(args);
      }
      break;
    case ORE_TYPE_FUNC:
      {
        ore_context* env = ore_new((ore_context*) fn.v.f.ore);
        if (fn.v.f.body) {
          ore_value* args = ore_bind_args(ore, fn.v.f.x.o, env, t);
          if (ore->err == ORE_ERROR_NONE)
            v = ore_eval(env, fn.v.f.body);
          if (env->err == ORE_ERROR_EXCEPTION)
            ore->err = env->err;
          free(args);
        }
      }
      break;
    default:
      fprintf(stderr, "invalid function call\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return ore_value_nil();
  }
  return v;
}

static ore_value*
ore_index_ref(ore_context* ore, ore_value v, ore_value e, int update) {
  if (v.t == ORE_TYPE_ARRAY) {
    if (e.t != ORE_TYPE_INT) {
      fprintf(stderr, "array index should be int\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return NULL;
    }
    ore_array_t* a = (ore_array_t*) v.v.a->p;
    int n = 0;
    ore_array_iter_t* k;
    for (k = kl_begin(a); k != kl_end(a); k = kl_next(k)) {
      if (n == e.v.i) {
        return &kl_val(k);
      }
      n++;
    }
    fprintf(stderr, "out of bounds for array\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return NULL;
  }
  if (v.t == ORE_TYPE_HASH) {
    if (e.t != ORE_TYPE_STRING) {
      fprintf(stderr, "hash index should be string\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return NULL;
    }
    ore_hash_t* h = (ore_hash_t*) v.v.h->p;
    if (update) {
      int r = 0;
      khint_t k = kh_put(value, h, e.v.s->p, &r);
      if (r) kh_value(h, k) = ore_value_nil();
      return &kh_value(h, k);
    } else {
      khint_t k = kh_get(value, h, e.v.s->p);
      if (k != kh_end(h)) {
        return &kh_value(h, k);
      }
    }
    return NULL;
  }
  if (v.t == ORE_TYPE_OBJECT) {
    if (e.t != ORE_TYPE_STRING) {
      fprintf(stderr, "object index should be string\n");
      ore->err = ORE_ERROR_EXCEPTION;
      return NULL;
    }
    ore_context* this = (ore_context*) v.v.o->e;
    if (update) {
      int r = 0;
      khint_t k = kh_put(value, this->env, e.v.s->p, &r);
      if (r) kh_value(this->env, k) = ore_value_nil();
      return &kh_value(this->env, k);
    } else {
      khint_t k = kh_get(value, this->env, e.v.s->p);
      if (k != kh_end(this->env)) {
        return &kh_value(this->env, k);
      }
    }
    return NULL;
  }
  fprintf(stderr, "invalid operation for %s\n", ore_kind(v));
  ore->err = ORE_ERROR_EXCEPTION;
  return NULL;
}

static ore_value
ore_expr0(ore_context* ore, ore_value lhs, const char* op, ore_value rhs) {
  if (lhs.t== ORE_TYPE_INT && rhs.t == ORE_TYPE_FLOAT) {
    double d = (double) lhs.v.i;
    lhs.t = ORE_TYPE_FLOAT;
    lhs.v.d = d;
  }
  switch (lhs.t) {
    case ORE_TYPE_INT:
      if (rhs.t == ORE_TYPE_INT || rhs.t== ORE_TYPE_FLOAT)
      {
        int iv = rhs.t == ORE_TYPE_INT ? rhs.v.i : rhs.t == ORE_TYPE_FLOAT ? (int) rhs.v.d : 0;
        if (op[0] == '*' && op[1] == '*') {
          if (iv < 0) {
            double d = pow((double) lhs.v.i, (double) iv);
            lhs.t = ORE_TYPE_FLOAT;
            lhs.v.d = d;
          } else {
            int r = 1, b = lhs.v.i, e = iv;
            while (e > 0) {
              if (e & 1) r *= b;
              b *= b;
              e >>= 1;
            }
            lhs.v.i = r;
          }
        }
        else if (!strcmp(op, "<<")) { lhs.v.i <<= iv; }
        else if (!strcmp(op, ">>")) { lhs.v.i >>= iv; }
        else if (!strcmp(op, "&")) { lhs.v.i &= iv; }
        else if (!strcmp(op, "^")) { lhs.v.i ^= iv; }
        else if (!strcmp(op, "|")) { lhs.v.i |= iv; }
        else if (*op == '+') { lhs.v.i += iv; }
        else if (*op == '-') { lhs.v.i -= iv; }
        else if (*op == '*') { lhs.v.i *= iv; }
        else if (*op == '/' || *op == '%') {
          if (iv == 0) {
            fprintf(stderr, "division by zero\n");
            ore->err = ORE_ERROR_EXCEPTION;
            return ore_value_nil();
          }
          if (*op == '/') lhs.v.i /= iv;
          else lhs.v.i %= iv;
        }
        else {
          fprintf(stderr, "unknown operator '%s' for int\n", op);
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
      } else {
        fprintf(stderr, "unknown operator '%s' for int\n", op);
        ore->err = ORE_ERROR_EXCEPTION;
        return ore_value_nil();
      }
      break;
    case ORE_TYPE_FLOAT:
      if (rhs.t == ORE_TYPE_INT || rhs.t== ORE_TYPE_FLOAT)
      {
        double fv = rhs.t == ORE_TYPE_INT ? (double) rhs.v.i : rhs.t == ORE_TYPE_FLOAT ? rhs.v.d : 0;
        if (op[0] == '*' && op[1] == '*') { lhs.v.d = pow(lhs.v.d, fv); }
        else if (*op == '+') { lhs.v.d += fv; }
        else if (*op == '-') { lhs.v.d -= fv; }
        else if (*op == '*') { lhs.v.d *= fv; }
        else if (*op == '/') { lhs.v.d /= fv; }
        else if (*op == '%') {
          if (fv == 0) {
            fprintf(stderr, "division by zero\n");
            ore->err = ORE_ERROR_EXCEPTION;
            return ore_value_nil();
          }
          lhs.v.d = fmod(lhs.v.d, fv);
        }
        else {
          fprintf(stderr, "unknown operator '%s' for float\n", op);
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
      } else {
        fprintf(stderr, "unknown operator '%s' for float\n", op);
        ore->err = ORE_ERROR_EXCEPTION;
        return ore_value_nil();
      }
      break;
    case ORE_TYPE_STRING:
      {
        char buf[512], *p = buf;
        if (*op == '+') {
          if (rhs.t == ORE_TYPE_INT)
            snprintf(buf, sizeof(buf), "%i", rhs.v.i);
          else if (rhs.t == ORE_TYPE_FLOAT)
            snprintf(buf, sizeof(buf), "%f", rhs.v.d);
          else if (rhs.t == ORE_TYPE_STRING)
            p = rhs.v.s->p;
          else {
            fprintf(stderr, "unknown operator '%s' for string\n", op);
            ore->err = ORE_ERROR_EXCEPTION;
            return ore_value_nil();
          }

          size_t l = strlen(p) + strlen(lhs.v.s->p);
          char* s = calloc(1, l + 1);
          strcpy(s, lhs.v.s->p);
          strcat(s, p);
          lhs = ore_value_str_from_ptr(ore, s, l);
        } else {
          fprintf(stderr, "unknown operator '%s' for string\n", op);
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
      }
      break;
    default:
      fprintf(stderr, "unknown operator '%s' for %s\n", op, ore_kind(lhs));
      ore->err = ORE_ERROR_EXCEPTION;
      return ore_value_nil();
  }
  return lhs;
}

static int
ore_is_cmp_op(const char* op) {
  return !strcmp(op, "==") || !strcmp(op, "!=") ||
         !strcmp(op, "<") || !strcmp(op, "<=") ||
         !strcmp(op, ">") || !strcmp(op, ">=") ||
         !strcmp(op, "=~");
}

static ore_value ore_cmp(ore_context*, ore_value, char*, ore_value);

static ore_value
ore_expr(ore_context* ore, mpc_ast_t* t) {
  int i;
  ore_value lhs = ore_eval(ore, t->children[0]);
  if (ore->err != ORE_ERROR_NONE)
    return ore_value_nil();
  for (i = 1; i < t->children_num; i += 2) {
    char* op = t->children[i]->contents;
    if (!strcmp(op, "&&") || !strcmp(op, "||")) {
      int l = ore_is_true(lhs);
      if (*op == '&' ? !l : l) {
        lhs = l ? ore_value_true() : ore_value_false();
        continue;
      }
      ore_value rhs = ore_eval(ore, t->children[i+1]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      lhs = ore_is_true(rhs) ? ore_value_true() : ore_value_false();
      continue;
    }
    ore_value rhs = ore_eval(ore, t->children[i+1]);
    if (ore->err != ORE_ERROR_NONE)
      return ore_value_nil();
    if (ore_is_cmp_op(op))
      lhs = ore_cmp(ore, lhs, op, rhs);
    else
      lhs = ore_expr0(ore, lhs, op, rhs);
  }
  return lhs;
}

static int
ore_cmp_eq(ore_context* ore, ore_value lhs, ore_value rhs) {
  switch (lhs.t) {
    case ORE_TYPE_NIL:
      return rhs.t == ORE_TYPE_NIL;
    case ORE_TYPE_BOOL:
      if (rhs.t == ORE_TYPE_BOOL && lhs.v.b == rhs.v.b) return 1;
      return 0;
    case ORE_TYPE_INT:
      if (rhs.t == ORE_TYPE_INT && lhs.v.i == rhs.v.i) return 1;
      if (rhs.t == ORE_TYPE_FLOAT && lhs.v.i == rhs.v.d) return 1;
      return 0;
    case ORE_TYPE_FLOAT:
      if (rhs.t == ORE_TYPE_INT && lhs.v.d == rhs.v.i) return 1;
      if (rhs.t == ORE_TYPE_FLOAT && lhs.v.d == rhs.v.d) return 1;
      return 0;
    case ORE_TYPE_STRING:
      if (lhs.t == rhs.t && lhs.v.s->l == rhs.v.s->l &&
          !memcmp(lhs.v.s->p, rhs.v.s->p, lhs.v.s->l))
        return 1;
      return 0;
    case ORE_TYPE_ARRAY:
      if (lhs.t == rhs.t && lhs.v.a->p == rhs.v.a->p)
        return 1;
      return 0;
    case ORE_TYPE_HASH:
      if (lhs.t == rhs.t && lhs.v.h->p == rhs.v.h->p)
        return 1;
      return 0;
    case ORE_TYPE_REGEXP:
      if (lhs.t == rhs.t && lhs.v.r->l == rhs.v.r->l &&
          !memcmp(lhs.v.r->p, rhs.v.r->p, lhs.v.r->l))
        return 1;
      return 0;
    case ORE_TYPE_FUNC:
      if (lhs.t == rhs.t && lhs.v.f.x.o == rhs.v.f.x.o)
        return 1;
      return 0;
    case ORE_TYPE_CFUNC:
      if (lhs.t == rhs.t && lhs.v.f.x.c == rhs.v.f.x.c)
        return 1;
      return 0;
    case ORE_TYPE_ENV:
      if (lhs.t == rhs.t && lhs.v.e->p == rhs.v.e->p)
        return 1;
      return 0;
    default:
      break;
  }
  return 0;
}

static ore_value
ore_match_regexp(ore_context* ore, ore_value lhs, ore_value rhs) {
  if (lhs.t == ORE_TYPE_STRING && rhs.t == ORE_TYPE_REGEXP) {
    struct slre_cap caps[10];
    memset(caps, 0, sizeof(caps));
    if (slre_match(rhs.v.r->p, lhs.v.s->p, lhs.v.s->l, caps, 10, 0) > 0) {
      int i;
      ore_array_t* a = kl_init(value);
      for (i = 0; i < 10; i++) {
        if (caps[i].ptr == NULL) break;
        char* p = calloc(1, caps[i].len + 1);
        memcpy(p, caps[i].ptr, caps[i].len);
        *kl_pushp(value, a) = ore_value_str_from_ptr(ore, p, caps[i].len);
      }
      return ore_value_array_from_klist(ore, a);
    }
  }
  return ore_value_nil();
}

static int
ore_cmp_lessmore(ore_context* ore, ore_value lhs, ore_value rhs) {
  if (lhs.t == ORE_TYPE_INT && rhs.t == ORE_TYPE_INT)
    return lhs.v.i < rhs.v.i ? -1 : lhs.v.i > rhs.v.i ? 1 : 0;
  if ((lhs.t == ORE_TYPE_INT || lhs.t == ORE_TYPE_FLOAT) &&
      (rhs.t == ORE_TYPE_INT || rhs.t == ORE_TYPE_FLOAT)) {
    double l = lhs.t == ORE_TYPE_INT ? (double) lhs.v.i : lhs.v.d;
    double r = rhs.t == ORE_TYPE_INT ? (double) rhs.v.i : rhs.v.d;
    return l < r ? -1 : l > r ? 1 : 0;
  }
  fprintf(stderr, "invalid operator\n");
  ore->err = ORE_ERROR_EXCEPTION;
  return 0;
}

static ore_value
ore_cmp(ore_context* ore, ore_value lhs, char* op, ore_value rhs) {
  if (!strcmp(op, "==")) return ore_cmp_eq(ore, lhs, rhs) ? ore_value_true() : ore_value_false();
  if (!strcmp(op, "!=")) return !ore_cmp_eq(ore, lhs, rhs) ? ore_value_true() : ore_value_false();
  if (!strcmp(op, "<")) return ore_cmp_lessmore(ore, lhs, rhs) < 0 ? ore_value_true() : ore_value_false();
  if (!strcmp(op, "<=")) return ore_cmp_lessmore(ore, lhs, rhs) <= 0 ? ore_value_true() : ore_value_false();
  if (!strcmp(op, ">")) return ore_cmp_lessmore(ore, lhs, rhs) > 0 ? ore_value_true() : ore_value_false();
  if (!strcmp(op, ">=")) return ore_cmp_lessmore(ore, lhs, rhs) >= 0 ? ore_value_true() : ore_value_false();
  if (!strcmp(op, "=~")) return ore_match_regexp(ore, lhs, rhs);
  return ore_value_false();
}

static ore_value
ore_eval(ore_context* ore, mpc_ast_t* t) {
  int i, r;
  if (!t) return ore_value_nil();

  khiter_t k;
  k = kh_get(ast, ore->ast, (khint64_t)(uintptr_t)t);
  if (k != kh_end(ore->ast)) {
    return kh_value(ore->ast, k);
  }

  switch (ore_get_tag(ore->tags, t)) {
  case ORE_TAG_EOF:
    return ore_value_nil();
  case ORE_TAG_TRUE:
    return ore_value_true();
  case ORE_TAG_FALSE:
    return ore_value_false();
  case ORE_TAG_NIL:
    return ore_value_nil();
  case ORE_TAG_NUMBER:
    {
      ore_value v = ore_parse_num(ore, t->contents);
      k = kh_put(ast, ore->ast, (khint64_t)(uintptr_t)t, &r);
      kh_value(ore->ast, k) = v;
      return v;
    }
  case ORE_TAG_STRING:
    {
      ore_value v = ore_parse_str(ore, t->contents);
      k = kh_put(ast, ore->ast, (khint64_t)(uintptr_t)t, &r);
      kh_value(ore->ast, k) = v;
      return v;
    }
  case ORE_TAG_ARRAY:
    {
      ore_array_t* a = kl_init(value);
      for (i = 1; i < t->children_num - 1; i += 2) {
        *kl_pushp(value, a) = ore_eval(ore, t->children[i]);
        if (ore->err != ORE_ERROR_NONE)
          return ore_value_nil();
      }
      return ore_value_array_from_klist(ore, a);
    }
  case ORE_TAG_HASH:
    {
      ore_hash_t* h = kh_init(value);
      for (i = 1; i < t->children_num - 1; i += 2) {
        ore_value key = ore_eval(ore, t->children[i]->children[0]);
        if (ore->err != ORE_ERROR_NONE)
          return ore_value_nil();
        ore_value val = ore_eval(ore, t->children[i]->children[2]);
        if (ore->err != ORE_ERROR_NONE)
          return ore_value_nil();
        khint_t k = kh_put(value, h, key.v.s->p, &r);
        kh_value(h, k) = val;
      }
      return ore_value_hash_from_khash(ore, h);
    }
  case ORE_TAG_REGEXP:
    {
      ore_value v = ore_parse_str(ore, t->contents);
      v.t = ORE_TYPE_REGEXP;
      k = kh_put(ast, ore->ast, (khint64_t)(uintptr_t)t, &r);
      kh_value(ore->ast, k) = v;
      return v;
    }
  case ORE_TAG_ITEM:
    {
      ore_value v = ore_eval(ore, t->children[0]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      for (i = 2; i < t->children_num; i += 3) {
        ore_value key = ore_eval(ore, t->children[i]);
        if (v.t == ORE_TYPE_STRING) {
          if (key.t != ORE_TYPE_INT) {
            fprintf(stderr, "string index should be int\n");
            ore->err = ORE_ERROR_EXCEPTION;
            return ore_value_nil();
          }
          if (key.v.i < 0 || key.v.i >= v.v.s->l) {
            fprintf(stderr, "out of bounds for string\n");
            ore->err = ORE_ERROR_EXCEPTION;
            return ore_value_nil();
          }
          char* p = calloc(1, 2);
          p[0] = v.v.s->p[key.v.i];
          v = ore_value_str_from_ptr(ore, p, 1);
          continue;
        }
        ore_value* r = ore_index_ref(ore, v, key, 0);
        v = r == NULL ? ore_value_nil() : *r;
      }
      return v;
    }
  case ORE_TAG_PROP:
    {
      ore_value v = ore_eval(ore, t->children[0]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      ore_context* this = v.t == ORE_TYPE_OBJECT ? (ore_context*) v.v.o->e : NULL;
      for (i = 2; i < t->children_num; i += 2) {
        if (v.t != ORE_TYPE_OBJECT) {
          fprintf(stderr, "invalid operation for %s\n", ore_kind(v));
          ore->err = ORE_ERROR_EXCEPTION;
          return ore_value_nil();
        }
        v = ore_prop(this, t->children[i]->contents);
        if (v.t == ORE_TYPE_OBJECT) this = (ore_context*) v.v.o->e;
      }
      return v;
    }
  case ORE_TAG_IDENT:
    return ore_get(ore, t->contents);
  case ORE_TAG_CALL:
    return ore_call(ore, t);
  case ORE_TAG_NEW:
    return ore_object_new(ore, t);
  case ORE_TAG_LAMBDA:
    {
      ore_value v = { ORE_TYPE_FUNC };
      ore_init_func(&v.v.f, ore, t);
      return v;
    }
  case ORE_TAG_FACTOR:
    if (t->children[0]->contents[0] == '!') {
      ore_value v = ore_eval(ore, t->children[1]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      return ore_is_true(v) ? ore_value_false() : ore_value_true();
    }
    if (t->children[0]->contents[0] == '-') {
      ore_value v = ore_eval(ore, t->children[1]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      if (v.t == ORE_TYPE_INT) { v.v.i = -v.v.i; return v; }
      if (v.t == ORE_TYPE_FLOAT) { v.v.d = -v.v.d; return v; }
      fprintf(stderr, "unknown operator '-' for %s\n", ore_kind(v));
      ore->err = ORE_ERROR_EXCEPTION;
      return ore_value_nil();
    }
    return ore_eval(ore, t->children[1]);
  case ORE_TAG_LEXP_TERM:
    if (t->children_num == 5 &&
        is_a(t->children[1], "char") && t->children[1]->contents[0] == '?') {
      ore_value c = ore_eval(ore, t->children[0]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      return ore_eval(ore, ore_is_true(c) ? t->children[2] : t->children[4]);
    }
    return ore_expr(ore, t);
  case ORE_TAG_INCDEC:
    {
      const char* name = t->children[0]->contents;
      ore_value v = ore_get(ore, name);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      int inc = t->children[1]->contents[0] == '+';
      if (v.t == ORE_TYPE_INT) v.v.i += inc ? 1 : -1;
      else if (v.t == ORE_TYPE_FLOAT) v.v.d += inc ? 1 : -1;
      else {
        fprintf(stderr, "unknown operator '%s' for %s\n",
          t->children[1]->contents, ore_kind(v));
        ore->err = ORE_ERROR_EXCEPTION;
        return ore_value_nil();
      }
      ore_set(ore, name, v);
      return v;
    }
  case ORE_TAG_LET_V:
    {
      const char* op = t->children[1]->contents;
      ore_value rhs = ore_eval(ore, t->children[2]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      ore_value lhs = *op != '=' ?
        ore_expr0(ore, ore_eval(ore, t->children[0]), op, rhs) : rhs;
      ore_set(ore, t->children[0]->contents, lhs);
      return lhs;
    }
  case ORE_TAG_LET_A:
    {
      ore_value lhs = ore_eval(ore, t->children[0]->children[0]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      const char* op = t->children[1]->contents;
      ore_value* r = NULL;
      for (i = 2; i < t->children[0]->children_num - 1; i += 3) {
        ore_value key = ore_eval(ore, t->children[0]->children[i]);
        int last = i + 3 >= t->children[0]->children_num - 1;
        r = ore_index_ref(ore, lhs, key, last);
        lhs = r == NULL ? ore_value_nil() : *r;
      }
      if (r == NULL) {
        return ore_value_nil();
      }
      ore_value rhs = ore_eval(ore, t->children[2]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      if (*op != '=') {
        rhs = ore_expr0(ore, lhs, op, rhs);
        if (ore->err != ORE_ERROR_NONE)
          return ore_value_nil();
      }
      ore_value_ref(rhs);
      ore_value_unref(lhs);
      *r = rhs;
      return rhs;
    }
  case ORE_TAG_LET_P:
    {
      ore_value lhs = ore_eval(ore, t->children[0]->children[0]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      const char* op = t->children[1]->contents;
      ore_value* r = NULL;
      for (i = 2; i < t->children[0]->children_num; i += 2) {
        ore_value key = ore_value_str_from_ptr(ore, t->children[0]->children[i]->contents, -1);
        int last = i + 2 >= t->children[0]->children_num;
        r = ore_index_ref(ore, lhs, key, last);
        lhs = r == NULL ? ore_value_nil() : *r;
      }
      if (r == NULL) {
        return ore_value_nil();
      }
      ore_value rhs = ore_eval(ore, t->children[2]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      if (*op != '=') {
        rhs = ore_expr0(ore, lhs, op, rhs);
        if (ore->err != ORE_ERROR_NONE)
          return ore_value_nil();
      }
      ore_value_ref(rhs);
      ore_value_unref(lhs);
      *r = rhs;
      return rhs;
    }
  case ORE_TAG_VAR:
    {
      ore_value v = ore_eval(ore, t->children[3]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      ore_define(ore, t->children[1]->contents, v);
      return v;
    }
  case ORE_TAG_FUNC:
    {
      ore_value v = { ORE_TYPE_FUNC };
      ore_init_func(&v.v.f, ore, t);
      ore_define(ore, t->children[1]->contents, v);
      return v;
    }
  case ORE_TAG_CLASS_EXT:
    return ore_define_class(ore, t->children[1], t->children[5], t->children[3]->contents);
  case ORE_TAG_CLASS:
    return ore_define_class(ore, t->children[1], t->children[3], NULL);
  case ORE_TAG_RETURN:
    {
      ore_value v = ore_eval(ore, t->children[1]);
      ore->err = ORE_ERROR_RETURN;
      ore_value_ref(v);
      return v;
    }
  case ORE_TAG_THROW:
    {
      ore_value v = ore_eval(ore, t->children[1]);
      if (ore->err != ORE_ERROR_NONE)
        return ore_value_nil();
      ore_context* root = ore;
      while (root->parent) root = root->parent;
      ore_value_ref(v);
      ore_value_unref(root->exc);
      root->exc = v;
      ore->err = ORE_ERROR_EXCEPTION;
      return ore_value_nil();
    }
  case ORE_TAG_TRY:
    {
      mpc_ast_t *tryb = NULL, *catchb = NULL, *evar = NULL;
      int seen_catch = 0;
      for (i = 0; i < t->children_num; i++) {
        mpc_ast_t* c = t->children[i];
        if (is_a(c, "string")) {
          if (!strcmp(c->contents, "catch")) seen_catch = 1;
          continue;
        }
        if (is_a(c, "char") && c->contents[0] == '{') {
          mpc_ast_t* n = i + 1 < t->children_num ? t->children[i+1] : NULL;
          if (n && !(is_a(n, "char") && n->contents[0] == '}')) {
            if (seen_catch) catchb = n;
            else tryb = n;
          }
          continue;
        }
        if (is_a(c, "ident")) evar = c;
      }
      ore_context* root = ore;
      while (root->parent) root = root->parent;
      ore_context* env = ore_new(ore);
      ore_value v = ore_eval(env, tryb);
      if (env->err != ORE_ERROR_EXCEPTION) {
        if (env->err != ORE_ERROR_NONE) {
          ore->err = env->err;
          ore_destroy(env);
          return v;
        }
        ore_destroy(env);
        return ore_value_nil();
      }
      ore_destroy(env);
      ore_context* env2 = ore_new(ore);
      ore_value exc = root->exc;
      root->exc = ore_value_nil();
      if (evar)
        ore_define(env2, evar->contents, exc);
      ore_value_unref(exc);
      v = ore_eval(env2, catchb);
      if (env2->err != ORE_ERROR_NONE) {
        ore->err = env2->err;
        ore_destroy(env2);
        return v;
      }
      ore_destroy(env2);
      return ore_value_nil();
    }
  case ORE_TAG_BREAK:
    ore->err = ORE_ERROR_BREAK;
    return ore_value_nil();
  case ORE_TAG_CONTINUE:
    ore->err = ORE_ERROR_CONTINUE;
    return ore_value_nil();
  case ORE_TAG_IF_STMT:
    if (ore_is_true(ore_eval(ore, t->children[2]))) {
      return ore_eval(ore, ore_find_statements(t));
    }
    return ore_value_nil();
  case ORE_TAG_IF:
    {
      int i;
      for (i = 0; i < t->children_num; i++) {
        int r = 0;
        mpc_ast_t* f = t->children[i];
        if (is_a(f, "if_stmt")) {
          r = ore_is_true(ore_eval(ore, f->children[2]));
        } else if (is_a(f, "else_if")) {
          r = ore_is_true(ore_eval(ore, f->children[3]));
        } else {
          r = 1;
        }
        if (r)
          return ore_eval(ore, ore_find_statements(f));
      }
      return ore_value_nil();
    }
  case ORE_TAG_WHILE:
    {
      ore_context* env = ore_new(ore);
      ore_value v = ore_value_nil();
      while (ore_is_true(ore_eval(env, t->children[2]))) {
        v = ore_eval(env, ore_find_statements(t));
        if (env->err != ORE_ERROR_NONE) {
          if (env->err == ORE_ERROR_CONTINUE) {
            env->err = ORE_ERROR_NONE;
            continue;
          }
          break;
        }
      }
      if (env->err == ORE_ERROR_RETURN || env->err == ORE_ERROR_EXCEPTION) {
        ore->err = env->err;
        ore_destroy(env);
        return v;
      }
      ore_destroy(env);
      return ore_value_nil();
    }
  case ORE_TAG_FOR_IN:
    {
      ore_value l = ore_eval(ore, t->children[4]);
      if (l.t == ORE_TYPE_HASH) {
        ore_hash_t* h = (ore_hash_t*) l.v.h->p;
        ore_hash_iter_t hk;
        ore_context* env = ore_new(ore);
        ore_value v = ore_value_nil();
        for (hk = kh_begin(h); hk != kh_end(h); hk++) {
          if (!kh_exist(h, hk)) continue;
          char* p = strdup(kh_key(h, hk));
          ore_define(env, t->children[2]->contents, ore_value_str_from_ptr(ore, p, -1));
          v = ore_eval(env, ore_find_statements(t));
          if (env->err != ORE_ERROR_NONE) {
            if (env->err == ORE_ERROR_CONTINUE) {
              env->err = ORE_ERROR_NONE;
              continue;
            }
            break;
          }
        }
        if (env->err == ORE_ERROR_RETURN || env->err == ORE_ERROR_EXCEPTION) {
          ore->err = env->err;
          ore_destroy(env);
          return v;
        }
        ore_destroy(env);
        return ore_value_nil();
      }
      if (l.t != ORE_TYPE_ARRAY) {
        fprintf(stderr, "expected array or hash for argument\n");
        ore->err = ORE_ERROR_EXCEPTION;
        return ore_value_nil();
      }
      ore_array_t* a = (ore_array_t*) l.v.a->p;
      ore_array_iter_t *k;
      ore_context* env = ore_new(ore);
      ore_value v = ore_value_nil();
      for (k = kl_begin(a); k != kl_end(a); k = kl_next(k)) {
        ore_define(env, t->children[2]->contents, kl_val(k));
        v = ore_eval(env, ore_find_statements(t));
        if (env->err != ORE_ERROR_NONE) {
          if (env->err == ORE_ERROR_CONTINUE) {
            env->err = ORE_ERROR_NONE;
            continue;
          }
          break;
        }
      }
      if (env->err == ORE_ERROR_RETURN || env->err == ORE_ERROR_EXCEPTION) {
        ore->err = env->err;
        ore_destroy(env);
        return v;
      }
      ore_destroy(env);
      return ore_value_nil();
    }
  case ORE_TAG_FOR_C:
    {
      mpc_ast_t *init = NULL, *cond = NULL, *step = NULL;
      for (i = 0; i < t->children_num; i++) {
        mpc_ast_t* c = t->children[i];
        if (is_a(c, "char")) {
          if (c->contents[0] == '{') break;
          continue;
        }
        if (is_a(c, "string")) continue;
        if (is_a(c, "let_v") || is_a(c, "var")) init = c;
        else if (is_a(c, "incdec") || is_a(c, "let_s")) step = c;
        else cond = c;
      }
      ore_context* env = ore_new(ore);
      ore_value v = ore_value_nil();
      if (init)
        ore_eval(env, init);
      while (env->err == ORE_ERROR_NONE) {
        if (cond) {
          int r = ore_is_true(ore_eval(env, cond));
          if (env->err != ORE_ERROR_NONE || !r) break;
        }
        v = ore_eval(env, ore_find_statements(t));
        if (env->err != ORE_ERROR_NONE) {
          if (env->err == ORE_ERROR_CONTINUE)
            env->err = ORE_ERROR_NONE;
          else
            break;
        }
        if (step)
          ore_eval(env, step);
      }
      if (env->err == ORE_ERROR_RETURN || env->err == ORE_ERROR_EXCEPTION) {
        ore->err = env->err;
        ore_destroy(env);
        return v;
      }
      ore_destroy(env);
      return ore_value_nil();
    }
  case ORE_TAG_STMTS:
    {
      ore_value v = ore_value_nil();
      for (i = 0; i < t->children_num; i++) {
        if (is_a(t->children[i], "char") && !strcmp(t->children[i]->contents, ";"))
          continue;
        v = ore_eval(ore, t->children[i]);
        if (ore->err != ORE_ERROR_NONE)
          return v;
      }
      return v;
    }
  case ORE_TAG_STMT:
    return ore_eval(ore, t->children[0]);
  case ORE_TAG_SEMI:
    return ore_value_nil();
  default:
    fprintf(stderr, "unknown operation '%s'\n", t->contents);
    ore->err = ORE_ERROR_EXCEPTION;
    return ore_value_nil();
  }
}

ore_context*
ore_new(ore_context* parent) {
  ore_context* ore = (ore_context*) malloc(sizeof(ore_context));
  if (!ore) {
    fprintf(stderr, "failed to allocate memory\n");
    return NULL;
  }
  ore->env = kh_init(value);
  ore->err = ORE_ERROR_NONE;
  ore->exc = ore_value_nil();
  ore->parent = parent;
  if (parent) {
    ore_context* root = parent;
    while (root->parent) root = root->parent;
    ore->ast = root->ast;
    ore->tags = root->tags;
  } else {
    ore->ast = kh_init(ast);
    ore->tags = kh_init(tag);
  }
  return ore;
}

#define unref_code(v) { \
  ore_value_unref(v); \
  if (verbose) \
    printf("unref %s\n", ore_kind(v)); \
};

void
ore_destroy(ore_context* ore) {
  kh_destroy(value, ore->env);
  if (!ore->parent) {
    kh_destroy(ast, ore->ast);
    kh_destroy(tag, ore->tags);
  }
  free(ore);
}

static int
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

static void
usage(char* prog) {
  fprintf(stderr, "usage of %s: file\n", prog);
}

int
main(int argc, char **argv) {
  int f = parse_args(argc, argv);
  if (f < 0) {
    usage(argv[0]);
    exit(1);
  }

  mpc_parser_t* m_true       = mpc_new("true");
  mpc_parser_t* m_false      = mpc_new("false");
  mpc_parser_t* m_nil        = mpc_new("nil");
  mpc_parser_t* m_number     = mpc_new("number");
  mpc_parser_t* m_factor     = mpc_new("factor");
  mpc_parser_t* m_string     = mpc_new("string");
  mpc_parser_t* m_array      = mpc_new("array");
  mpc_parser_t* m_pair       = mpc_new("pair");
  mpc_parser_t* m_hash       = mpc_new("hash");
  mpc_parser_t* m_regexp     = mpc_new("regexp");
  mpc_parser_t* m_ident      = mpc_new("ident");
  mpc_parser_t* m_postfix    = mpc_new("postfix");
  mpc_parser_t* m_pows       = mpc_new("pows");
  mpc_parser_t* m_term       = mpc_new("term");
  mpc_parser_t* m_arith      = mpc_new("arith");
  mpc_parser_t* m_bits       = mpc_new("bits");
  mpc_parser_t* m_cmpexp     = mpc_new("cmpexp");
  mpc_parser_t* m_logic      = mpc_new("logic");
  mpc_parser_t* m_value      = mpc_new("value");
  mpc_parser_t* m_item       = mpc_new("item");
  mpc_parser_t* m_prop       = mpc_new("prop");
  mpc_parser_t* m_lexp       = mpc_new("lexp");
  mpc_parser_t* m_incdec     = mpc_new("incdec");
  mpc_parser_t* m_leto       = mpc_new("let_o");
  mpc_parser_t* m_letv       = mpc_new("let_v");
  mpc_parser_t* m_leta       = mpc_new("let_a");
  mpc_parser_t* m_letp       = mpc_new("let_p");
  mpc_parser_t* m_if         = mpc_new("if");
  mpc_parser_t* m_ifstmt     = mpc_new("if_stmt");
  mpc_parser_t* m_elseif     = mpc_new("else_if");
  mpc_parser_t* m_else       = mpc_new("else");
  mpc_parser_t* m_while      = mpc_new("while");
  mpc_parser_t* m_forin      = mpc_new("for_in");
  mpc_parser_t* m_lets       = mpc_new("let_s");
  mpc_parser_t* m_forc       = mpc_new("for_c");
  mpc_parser_t* m_throw      = mpc_new("throw");
  mpc_parser_t* m_try        = mpc_new("try");
  mpc_parser_t* m_break      = mpc_new("break");
  mpc_parser_t* m_continue   = mpc_new("continue");
  mpc_parser_t* m_var        = mpc_new("var");
  mpc_parser_t* m_vararg     = mpc_new("vararg");
  mpc_parser_t* m_lambda     = mpc_new("lambda");
  mpc_parser_t* m_func       = mpc_new("func");
  mpc_parser_t* m_template   = mpc_new("template");
  mpc_parser_t* m_class      = mpc_new("class");
  mpc_parser_t* m_classext   = mpc_new("class_ext");
  mpc_parser_t* m_new        = mpc_new("new");
  mpc_parser_t* m_call       = mpc_new("call");
  mpc_parser_t* m_anoncall   = mpc_new("anoncall");
  mpc_parser_t* m_methodcall = mpc_new("methodcall");
  mpc_parser_t* m_return     = mpc_new("return");
  mpc_parser_t* m_comment    = mpc_new("comment");
  mpc_parser_t* m_eof        = mpc_new("eof");
  mpc_parser_t* m_stmt       = mpc_new("stmt");
  mpc_parser_t* m_stmts      = mpc_new("stmts");
  mpc_parser_t* m_program    = mpc_new("program");

#define NODES \
m_true,\
m_false,\
m_nil,\
m_number,\
m_factor,\
m_string,\
m_array,\
m_pair,\
m_hash,\
m_regexp,\
m_ident,\
m_postfix,\
m_pows,\
m_term,\
m_arith,\
m_bits,\
m_cmpexp,\
m_logic,\
m_lexp,\
m_value,\
m_item,\
m_prop,\
m_incdec,\
m_leto,\
m_leta,\
m_letv,\
m_letp,\
m_if,\
m_ifstmt,\
m_elseif,\
m_else,\
m_while,\
m_forin,\
m_lets,\
m_forc,\
m_throw,\
m_try,\
m_break,\
m_continue,\
m_var,\
m_vararg,\
m_lambda,\
m_func,\
m_template,\
m_class,\
m_classext,\
m_new,\
m_call,\
m_anoncall,\
m_methodcall,\
m_return,\
m_comment,\
m_eof,\
m_stmt,\
m_stmts,\
m_program

  mpc_err_t* err = mpca_lang(MPCA_LANG_DEFAULT, STRUCTURE, NODES);
  if (err != NULL) {
    ore_err_print(err);
    mpc_err_delete(err);
    goto leave;
  }

  mpc_result_t result;
  ore_parse_context pc;
  pc.root = mpc_ast_new(">", "");
  pc.program = m_program;

  ore_context* ore = ore_new(NULL);
  ore_define_cfunc(ore, "dump_env", 0, 0, ore_cfunc_dump_env, NULL);
  ore_define_cfunc(ore, "to_string", 1, 1, ore_cfunc_to_string, NULL);
  ore_define_cfunc(ore, "print", 0, -1, ore_cfunc_print, NULL);
  ore_define_cfunc(ore, "println", 0, -1, ore_cfunc_println, NULL);
  ore_define_cfunc(ore, "puts", 0, -1, ore_cfunc_println, NULL);
  ore_define_cfunc(ore, "len", 1, 1, ore_cfunc_len, NULL);
  ore_define_cfunc(ore, "range", 1, 2, ore_cfunc_range, NULL);
  ore_define_cfunc(ore, "push", 2, -1, ore_cfunc_push, NULL);
  ore_define_cfunc(ore, "pop", 1, 1, ore_cfunc_pop, NULL);
  ore_define_cfunc(ore, "slice", 3, 3, ore_cfunc_slice, NULL);
  ore_define_cfunc(ore, "sort", 1, 1, ore_cfunc_sort, NULL);
  ore_define_cfunc(ore, "keys", 1, 1, ore_cfunc_keys, NULL);
  ore_define_cfunc(ore, "values", 1, 1, ore_cfunc_values, NULL);
  ore_define_cfunc(ore, "has", 2, 2, ore_cfunc_has, NULL);
  ore_define_cfunc(ore, "delete", 2, 2, ore_cfunc_delete, NULL);
  ore_define_cfunc(ore, "substr", 3, 3, ore_cfunc_substr, NULL);
  ore_define_cfunc(ore, "index", 2, 2, ore_cfunc_index, NULL);
  ore_define_cfunc(ore, "split", 2, 2, ore_cfunc_split, NULL);
  ore_define_cfunc(ore, "join", 2, 2, ore_cfunc_join, NULL);
  ore_define_cfunc(ore, "replace", 3, 3, ore_cfunc_replace, NULL);
  ore_define_cfunc(ore, "upper", 1, 1, ore_cfunc_upper, NULL);
  ore_define_cfunc(ore, "lower", 1, 1, ore_cfunc_lower, NULL);
  ore_define_cfunc(ore, "trim", 1, 1, ore_cfunc_trim, NULL);
  ore_define_cfunc(ore, "to_int", 1, 1, ore_cfunc_to_int, NULL);
  ore_define_cfunc(ore, "to_float", 1, 1, ore_cfunc_to_float, NULL);
  ore_define_cfunc(ore, "abs", 1, 1, ore_cfunc_abs, NULL);
  ore_define_cfunc(ore, "floor", 1, 1, ore_cfunc_floor, NULL);
  ore_define_cfunc(ore, "ceil", 1, 1, ore_cfunc_ceil, NULL);
  ore_define_cfunc(ore, "round", 1, 1, ore_cfunc_round, NULL);
  ore_define_cfunc(ore, "sqrt", 1, 1, ore_cfunc_sqrt, NULL);
  ore_define_cfunc(ore, "pow", 2, 2, ore_cfunc_pow, NULL);
  ore_define_cfunc(ore, "typeof", 1, 1, ore_cfunc_typeof, NULL);
  ore_define_cfunc(ore, "load", 1, 1, ore_cfunc_load, &pc);
  ore_define_cfunc(ore, "environ", 0, 1, ore_cfunc_environ, &pc);
  ore_define_cfunc(ore, "exit", 1, 1, ore_cfunc_exit, NULL);
  ore_define_cfunc(ore, "json_encode", 1, 1, ore_cfunc_json_encode, NULL);
  ore_define_cfunc(ore, "json_decode", 1, 1, ore_cfunc_json_decode, NULL);
  ore_array_t* args = kl_init(value);
  int i;
  for (i = f+1; i < argc; i++) {
    char* parg = strdup(argv[i]);
    if (!parg) {
      fprintf(stderr, "failed to allocate memory\n");
      exit(1);
    }
    *kl_pushp(value, args) = ore_value_str_from_ptr(ore, parg, -1);
  }
  ore_define(ore, "args", ore_value_array_from_klist(ore, args));

  if (f > 0) {
    if (!mpc_parse_contents(argv[f], m_program, &result)) {
      ore_err_print(result.error);
      mpc_err_delete(result.error);
    } else {
      if (verbose)
        mpc_ast_print(result.output);
      mpc_ast_add_child(pc.root, result.output);
      ore_eval(ore, result.output);
      if (ore->err == ORE_ERROR_EXCEPTION && ore->exc.t != ORE_TYPE_NIL) {
        char* s = ore_value_to_str(ore, ore->exc);
        fprintf(stderr, "uncaught exception: %s\n", s);
        free(s);
      }
      mpc_ast_delete(result.output);
    }
  } else {
    char buf[BUFSIZ];
    while (1) {
      printf("> ");
      if (!fgets(buf, sizeof(buf), stdin)) {
        break;
      }
      int l = strlen(buf);
      if (l > 0 && buf[l-1] == '\n') { buf[l-1] = 0; l--; }
      if (l == 0) continue;
      if (!mpc_parse(argv[0], buf, m_stmt, &result)) {
        ore_err_print(result.error);
        mpc_err_delete(result.error);
        continue;
      }
      if (verbose)
        mpc_ast_print(result.output);
      ore_eval(ore, result.output);
      mpc_ast_add_child(pc.root, result.output);
    }
    mpc_ast_delete(pc.root);
  }
  ore_destroy(ore);

leave:
  mpc_cleanup(56, NODES);
  return 0;
}

// vim:set et:
