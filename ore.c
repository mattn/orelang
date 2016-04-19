#include "ore.h"

extern char **environ;

#define STRUCTURE \
"                                                                        \n" \
"number     : /-?([0-9]+(\\.[0-9]*)?(e[0-9]+)?|0x[0-9a-fA-F]+)/ ;        \n" \
"true       : \"true\" ;                                                 \n" \
"false      : \"false\" ;                                                \n" \
"nil        : \"nil\" ;                                                  \n" \
"factor     : '(' <lexp> ')'                                             \n" \
"           | <number>                                                   \n" \
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
"string     : /\"(\\\\.|[^\"])*\"/ ;                                     \n" \
"regexp     : /\\/(\\\\.|[^\\/])*\\// ;                                  \n" \
"item       : <factor> ('[' <lexp> ']')+ ;                               \n" \
"prop       : <factor> ('.' <ident>)+ ;                                  \n" \
"cmp        : <factor>                                                     " \
"         (\"!=\" | \"==\" | \"<=\" | \"<\" | \">=\" | \">\" | \"=~\")     " \
"         <factor> ;                                                     \n" \
"call       : <ident> '(' <lexp>? (',' <lexp>)* ')' ;                    \n" \
"anoncall   : <factor> '(' <lexp>? (',' <lexp>)* ')' ;                   \n" \
"methodcall : <prop> '(' <lexp>? (',' <lexp>)* ')' ;                     \n" \
"array      : '[' <lexp>? (',' <lexp>)* ']' ;                            \n" \
"pair       : <string> ':' <lexp> ;                                      \n" \
"hash       : '{' <pair>? (',' <pair>)* '}' ;                            \n" \
"ident      : /[a-zA-Z_][a-zA-Z0-9_]*/ ;                                 \n" \
"                                                                        \n" \
"term       : (<lambda> | <item> | <methodcall> | <prop> | <cmp>           " \
"         | <anoncall> | <call>                                            " \
"         | <factor> (('*' | '/' | '%' | '+' | '-') <factor>)* ) ;       \n" \
"lexp       : <term> (('+' | '-') <term>)* ;                             \n" \
"let_o      : (\"=\" | \"+=\" | \"-=\" | \"*=\" | \"/=\") ;              \n" \
"let_v      : <ident> <let_o> <lexp> ';' ;                               \n" \
"let_a      : <item> <let_o> <lexp> ';' ;                                \n" \
"let_p      : <prop> <let_o> <lexp> ';' ;                                \n" \
"else_if    : \"else\" \"if\" '(' <lexp> ')' '{' <stmts> '}' ;           \n" \
"else       : \"else\" '{' <stmts> '}' ;                                 \n" \
"if_stmt    : \"if\" '(' <lexp> ')' '{' <stmts> '}' ;                    \n" \
"if         : <if_stmt> <else_if>* <else>? ;                             \n" \
"while      : \"while\" '(' <lexp> ')' '{' <stmts> '}' ;                 \n" \
"for_in     : \"for\" '(' <ident> \"in\" <lexp> ')' '{' <stmts> '}' ;    \n" \
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
"break      : \"break\" ';' ;                                            \n" \
"continue   : \"continue\" ';' ;                                         \n" \
"return     : \"return\" <lexp> ';' ;                                    \n" \
"comment    : /#[^\n]*/ ;                                                \n" \
"eof        : /$/ ;                                                      \n" \
"stmt       : (<let_v> | <let_a> | <let_p> | <var> | <if>                  " \
"         | <while> | <for_in>                                             " \
"         | <func> | <class_ext> | <class> | <return> | <break>          \n" \
"         | <continue> | <lexp> ';' | <comment>) ;                       \n" \
"stmts      : <stmt>* ;                                                  \n" \
"program    : <stmts> <eof> ;                                            \n"

#define is_a(t, a) (strstr(t->tag, a) != NULL)

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
        int n = 0;
        ore_array_t* a = (ore_array_t*) v.v.a->p;
        ore_array_iter_t *k;
        for (k = kl_begin(a); k != kl_end(a); k = kl_next(k)) n++;
        return n > 0;
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
  if (*s == '0' && *(s+1) == 'x') {
    v.t = ORE_TYPE_INT;
    v.v.i = strtol(s, NULL, 16);
  } else if (!strchr(s, '.')) {
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

static ore_value*
ore_bind_args(ore_context* ore, mpc_ast_t* f, ore_context* this, mpc_ast_t* t) {
  int num_in = t->children_num / 2 - 1, n = 0, i;
  ore_value* args = (ore_value*) malloc(sizeof(ore_value) * num_in);
  if (!args) {
    fprintf(stderr, "failed to allocate memory\n");
    ore->err = ORE_ERROR_EXCEPTION;
    return NULL;
  }
  for (i = 0; i < num_in; i++) {
    args[i] = ore_value_nil();
  }
  i = 0;
  for (i = 0; i < t->children_num; i++) {
    if (is_a(t->children[i], "char") && t->children[i]->contents[0] == '(') {
      i++;
      break;
    }
  }
  for (; i < t->children_num; i++) {
    if (is_a(t->children[i], "char")) {
      if (t->children[i]->contents[0] == ')') {
        i++;
        break;
      }
    } else
      args[n++] = ore_eval(ore, t->children[i]);
  }

  for (i = 0; i < f->children_num; i++) {
    if (is_a(f->children[i], "char") && f->children[i]->contents[0] == '(') {
      i++;
      break;
    }
  }
  n = 0;
  for (; i < f->children_num; i++) {
    if (is_a(f->children[i], "char") && f->children[i]->contents[0] == ')') {
      i++;
      break;
    }
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
        ore_func_call(v.v.o->e, initialize, t->children_num / 2 - 1, args);
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
        ore_array_iter_t *k;
        int n = 0;
        for (k = kl_begin(a); k != kl_end(a); k = kl_next(k)) n++;
        v.v.i = n;
      }
      return v;
    default:
      break;
  }
  fprintf(stderr, "argument should be string or array\n");
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
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
  if (num_in == 1 && args[0].v.s->p) {
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
  v.v.f.x.c = c;
  v.v.f.u = u;
  ore_define(clazz.v.x->e, name, v);
}

void
ore_define_cfunc(ore_context* ore, const char* name, int num_in, int max_in, ore_cfunc_t c, void* u) {
  ore_value v = { ORE_TYPE_CFUNC };
  v.v.f.ore = ore;
  v.v.f.num_in = num_in;
  v.v.f.max_in = max_in;
  v.v.f.x.c = c;
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
  mpc_ast_t* stmts = NULL;
  mpc_ast_t* f = fn.v.f.x.o;
  int n = 0, i;
  for (i = 2; i < f->children_num; i++) {
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
  ore_value v = ore_value_nil();
  if (stmts) {
    v = ore_eval(env, stmts);
    if (env->err == ORE_ERROR_EXCEPTION)
      ore->err = env->err;
    char buf[64];
    sprintf(buf, "0x%p", env->env);
    ore_define(ore, buf, ore_value_env_from_context(env));
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
    if (fn.t == ORE_TYPE_NIL) {
      // FIXME
      ore_context* tmp = ore;
      ore_value inst = ore_eval(tmp, t->children[0]->children[0]);
      while (1) {
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
        }
        v = ((ore_cfunc_t)fn.v.f.x.c) ((ore_context*) ore, num_in, args, fn.v.f.u);
        free(args);
      }
      break;
    case ORE_TYPE_FUNC:
      {
        ore_context* env = ore_new((ore_context*) fn.v.f.ore);
        mpc_ast_t* stmts = ore_find_statements(fn.v.f.x.o);
        if (stmts) {
          ore_value* args = ore_bind_args(ore, fn.v.f.x.o, env, t);
          v = ore_eval(env, stmts);
          if (env->err == ORE_ERROR_EXCEPTION)
            ore->err = env->err;
          char buf[64];
          sprintf(buf, "0x%p", env->env);
          ore_define(ore, buf, ore_value_env_from_context(env));
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
        if (update) {
          ore_value old = kl_val(k);
          ore_value_unref(old);
        }
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
  if (v.t == ORE_TYPE_OBJECT) {
    ore_context* this = (ore_context*) v.v.o->e;
    if (update) {
      int r = 0;
      khint_t k = kh_get(value, this->env, e.v.s->p);
      if (k != kh_end(ore->env)) {
        ore_value old = kh_value(ore->env, k);
        ore_value_unref(old);
      }
      k = kh_put(value, this->env, e.v.s->p, &r);
      return &kh_value(this->env, k);
    } else {
      khint_t k = kh_get(value, this->env, e.v.s->p);
      if (k != kh_end(ore->env)) {
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
        if (*op == '+') { lhs.v.i += iv; }
        else if (*op == '-') { lhs.v.i -= iv; }
        else if (*op == '*') { lhs.v.i *= iv; }
        else if (*op == '/') { lhs.v.i /= iv; }
        else if (*op == '%') { lhs.v.i %= iv; }
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
        if (*op == '+') { lhs.v.d += fv; }
        else if (*op == '-') { lhs.v.d -= fv; }
        else if (*op == '*') { lhs.v.d *= fv; }
        else if (*op == '/') { lhs.v.d /= fv; }
        else if (*op == '%') { lhs.v.d = ((int) lhs.v.d % (int) fv); }
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
        char buf[32], *p = buf;
        if (*op == '+') {
          if (rhs.t == ORE_TYPE_INT)
            sprintf(buf, "%i", rhs.v.i);
          else if (rhs.t == ORE_TYPE_FLOAT)
            sprintf(buf, "%f", rhs.v.d);
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

static ore_value
ore_expr(ore_context* ore, mpc_ast_t* t) {
  int i;
  ore_value lhs = ore_eval(ore, t->children[0]);
  for (i = 1; i < t->children_num; i += 2) {
    char* op = t->children[i]->contents;
    ore_value rhs = ore_eval(ore, t->children[i+1]);
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
  if (lhs.t != rhs.t) return 0;
  switch (lhs.t) {
    case ORE_TYPE_INT:
      return lhs.v.i - rhs.v.i;
    case ORE_TYPE_FLOAT:
      return lhs.v.d - rhs.v.d;
    default:
      break;
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
  if (!strcmp(op, "<=")) return ore_cmp_lessmore(ore, lhs, rhs) >= 0 ? ore_value_true() : ore_value_false();
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

  if (is_a(t, "eof") || is_a(t, "comment")) {
    return ore_value_nil();
  }
  if (is_a(t, "true")) {
    return ore_value_true();
  }
  if (is_a(t, "false")) {
    return ore_value_false();
  }
  if (is_a(t, "nil")) {
    return ore_value_nil();
  }
  if (is_a(t, "number")) {
	ore_value v = ore_parse_num(ore, t->contents);
	k = kh_put(ast, ore->ast, (khint64_t)(uintptr_t)t, &r);
	kh_value(ore->ast, k) = v;
    return v;
  }
  if (is_a(t, "string")) {
	ore_value v = ore_parse_str(ore, t->contents);
	k = kh_put(ast, ore->ast, (khint64_t)(uintptr_t)t, &r);
	kh_value(ore->ast, k) = v;
    return v;
  }
  if (is_a(t, "array")) {
    ore_array_t* a = kl_init(value);
    for (i = 1; i < t->children_num - 1; i += 2) {
      *kl_pushp(value, a) = ore_eval(ore, t->children[i]);
    }
    return ore_value_array_from_klist(ore, a);
  }
  if (is_a(t, "hash")) {
    ore_hash_t* h = kh_init(value);
    for (i = 1; i < t->children_num - 1; i += 2) {
      ore_value key = ore_eval(ore, t->children[i]->children[0]);
      ore_value val = ore_eval(ore, t->children[i]->children[2]);
      khint_t k = kh_put(value, h, key.v.s->p, &r);
      kh_value(h, k) = val;
    }
    return ore_value_hash_from_khash(ore, h);
  }
  if (is_a(t, "regexp")) {
    ore_value v = ore_parse_str(ore, t->contents);
    v.t = ORE_TYPE_REGEXP;
	k = kh_put(ast, ore->ast, (khint64_t)(uintptr_t)t, &r);
	kh_value(ore->ast, k) = v;
    return v;
  }
  if (is_a(t, "item")) {
    ore_value v = ore_eval(ore, t->children[0]);
    for (i = 2; i < t->children_num; i += 3) {
      ore_value key = ore_eval(ore, t->children[i]);
      ore_value* r = ore_index_ref(ore, v, key, 0);
      v = r == NULL ? ore_value_nil() : *r;
    }
    return v;
  }
  if (is_a(t, "prop")) {
    ore_value v = ore_eval(ore, t->children[0]);
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
  if (is_a(t, "ident")) {
    return ore_get(ore, t->contents);
  }
  if (is_a(t, "factor")) {
    return ore_eval(ore, t->children[1]);
  }
  if (is_a(t, "lexp") || is_a(t, "term")) {
    return ore_expr(ore, t);
  }
  if (is_a(t, "let_v")) {
    const char* op = t->children[1]->contents;
    ore_value rhs = ore_eval(ore, t->children[2]);
    ore_value lhs = *op != '=' ?
      ore_expr0(ore, ore_eval(ore, t->children[0]), op, rhs) : rhs;
    ore_set(ore, t->children[0]->contents, lhs);
    return lhs;
  }
  if (is_a(t, "let_a")) {
    ore_value lhs = ore_eval(ore, t->children[0]->children[0]);
    const char* op = t->children[1]->contents;
    ore_value* r = NULL;
    for (i = 2; i < t->children[0]->children_num - 1; i += 3) {
      ore_value key = ore_eval(ore, t->children[0]->children[i]);
      r = ore_index_ref(ore, lhs, key, 1);
      lhs = r == NULL ? ore_value_nil() : *r;
    }
    if (r == NULL) {
      return ore_value_nil();
    }
    ore_value rhs = ore_eval(ore, t->children[2]);
    if (*op != '=') lhs = ore_expr0(ore, lhs, op, rhs);
    ore_value_ref(rhs);
    *r = rhs;
    return rhs;
  }
  if (is_a(t, "let_p")) {
    ore_value lhs = ore_eval(ore, t->children[0]->children[0]);
    const char* op = t->children[1]->contents;
    ore_value* r = NULL;
    for (i = 2; i < t->children[0]->children_num; i += 2) {
      ore_value key = ore_value_str_from_ptr(ore, t->children[0]->children[i]->contents, -1);
      r = ore_index_ref(ore, lhs, key, 0);
      lhs = r == NULL ? ore_value_nil() : *r;
    }
    if (r == NULL) {
      return ore_value_nil();
    }
    ore_value rhs = ore_eval(ore, t->children[2]);
    if (*op != '=') lhs = ore_expr0(ore, lhs, op, rhs);
    ore_value_ref(rhs);
    *r = rhs;
    return rhs;
  }
  if (is_a(t, "var")) {
    ore_value v = ore_eval(ore, t->children[3]);
    ore_define(ore, t->children[1]->contents, v);
    return v;
  }
  if (is_a(t, "cmp")) {
    ore_value lhs = ore_eval(ore, t->children[0]);
    ore_value rhs = ore_eval(ore, t->children[2]);
    return ore_cmp(ore, lhs, t->children[1]->contents, rhs);
  }
  if (is_a(t, "func")) {
    ore_value v = { ORE_TYPE_FUNC };
    v.v.f.ore = ore;
    v.v.f.num_in = -1;
    v.v.f.max_in = -1;
    v.v.f.x.o = t;
    v.v.f.u = NULL;
    ore_define(ore, t->children[1]->contents, v);
    return v;
  }
  if (is_a(t, "lambda")) {
    ore_value v = { ORE_TYPE_FUNC };
    v.v.f.ore = ore;
    v.v.f.num_in = -1;
    v.v.f.max_in = -1;
    v.v.f.x.o = t;
    v.v.f.u = NULL;
    return v;
  }
  if (is_a(t, "class_ext")) {
    return ore_define_class(ore, t->children[1], t->children[5], t->children[3]->contents);
  }
  if (is_a(t, "class")) {
    return ore_define_class(ore, t->children[1], t->children[3], NULL);
  }
  if (is_a(t, "new")) {
    return ore_object_new(ore, t);
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
  if (is_a(t, "break")) {
    ore->err = ORE_ERROR_BREAK;
    return ore_value_nil();
  }
  if (is_a(t, "continue")) {
    ore->err = ORE_ERROR_CONTINUE;
    return ore_value_nil();
  }
  if (is_a(t, "if_stmt")) {
    if (ore_is_true(ore_eval(ore, t->children[2]))) {
      return ore_eval(ore, ore_find_statements(t));
    }
    return ore_value_nil();
  }
  if (is_a(t, "if")) {
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
  if (is_a(t, "while")) {
    ore_context* env = ore_new(ore);
    while (ore_is_true(ore_eval(env, t->children[2]))) {
      ore_eval(env, ore_find_statements(t));
      if (env->err != ORE_ERROR_NONE) {
        if (env->err == ORE_ERROR_CONTINUE) {
          env->err = ORE_ERROR_NONE;
          continue;
        }
        break;
      }
    }
    if (env->err == ORE_ERROR_RETURN)
      ore->err = ORE_ERROR_RETURN;
    ore_destroy(env);
    return ore_value_nil();
  }
  if (is_a(t, "for_in")) {
    ore_value l = ore_eval(ore, t->children[4]);
    if (l.t != ORE_TYPE_ARRAY) {
      fprintf(stderr, "expected array for argument\n");
      ore->err = ORE_ERROR_EXCEPTION;
    }
    ore_array_t* a = (ore_array_t*) l.v.a->p;
    ore_array_iter_t *k;
    ore_context* env = ore_new(ore);
    for (k = kl_begin(a); k != kl_end(a); k = kl_next(k)) {
      ore_define(env, t->children[2]->contents, kl_val(k));
      ore_eval(env, ore_find_statements(t));
      if (env->err != ORE_ERROR_NONE) {
        if (env->err == ORE_ERROR_CONTINUE) {
          env->err = ORE_ERROR_NONE;
          continue;
        }
        break;
      }
    }
    if (env->err == ORE_ERROR_RETURN)
      ore->err = ORE_ERROR_RETURN;
    ore_destroy(env);
    return ore_value_nil();
  }
  if (is_a(t, "stmts") || is_a(t, "template") || t->tag[0] == '>') {
    ore_value v;
    for (i = 0; i < t->children_num; i++) {
      v = ore_eval(ore, t->children[i]);
      if (ore->err != ORE_ERROR_NONE)
        return v;
    }
    return ore_value_nil();
  }
  if (is_a(t, "stmt")) {
    return ore_eval(ore, t->children[0]);
  }
  if (is_a(t, "char") && !strcmp(t->contents, ";")) {
    return ore_value_nil();
  }
  fprintf(stderr, "unknown operation '%s'\n", t->contents);
  ore->err = ORE_ERROR_EXCEPTION;
  return ore_value_nil();
}

ore_context*
ore_new(ore_context* parent) {
  ore_context* ore = (ore_context*) malloc(sizeof(ore_context));
  if (!ore) {
    fprintf(stderr, "failed to allocate memory\n");
    return NULL;
  }
  ore->env = kh_init(value);
  ore->ct = kh_init(value);
  ore->err = ORE_ERROR_NONE;
  ore->parent = parent;
  ore->ast = kh_init(ast);
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
  kh_destroy(ast, ore->ast);
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
  mpc_parser_t* m_cmp        = mpc_new("cmp");
  mpc_parser_t* m_term       = mpc_new("term");
  mpc_parser_t* m_value      = mpc_new("value");
  mpc_parser_t* m_item       = mpc_new("item");
  mpc_parser_t* m_prop       = mpc_new("prop");
  mpc_parser_t* m_lexp       = mpc_new("lexp");
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
m_cmp,\
m_term,\
m_lexp,\
m_value,\
m_item,\
m_prop,\
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
  ore_define_cfunc(ore, "typeof", 1, 1, ore_cfunc_typeof, NULL);
  ore_define_cfunc(ore, "load", 1, 1, ore_cfunc_load, &pc);
  ore_define_cfunc(ore, "environ", 0, 1, ore_cfunc_environ, &pc);
  ore_define_cfunc(ore, "exit", 1, 1, ore_cfunc_exit, NULL);
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
  mpc_cleanup(44, NODES);
  return 0;
}

// vim:set et:
