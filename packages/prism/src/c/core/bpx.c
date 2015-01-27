#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "core/bpx.h"
#include "core/vector.h"

/*--------------------------------------------------------------------*/

#define REQUIRE_HEAP(n) \
	( heap_top + (n) <= local_top ? \
	    (void)(0) : myquit(STACK_OVERFLOW, "stack + heap") )

/*--------------------------------------------------------------------*/
/*                      Functions from B-Prolog                       */

int bpx_mount_query_term(TERM t);

/* cpred.c */
int         bp_string_2_term(const char *, TERM, TERM);
char*	    bp_term_2_string(TERM);
int         bp_call_term(TERM);
int         bp_mount_query_term(TERM);
int         bp_next_solution(void);

/* file.c */
void        write_term(TERM);

/* float1.c */

#ifdef __YAP_PROLOG__
static inline
#endif
double      floatval(TERM);
#ifdef __YAP_PROLOG__
static inline
#endif
TERM        encodefloat1(double USES_REGS);

/* loader.c */
SYM_REC_PTR insert(const char *, int, int);

/* mic.c */
NORET       quit(const char *);
NORET       myquit(int, const char *);

/* unify.c */
int          unify(TERM, TERM);
int          is_UNIFIABLE(TERM, TERM);
int          is_IDENTICAL(TERM, TERM);

/* prism.c */
NORET       bp4p_quit(int);

/*--------------------------------------------------------------------*/

static NORET bpx_raise(const char *fmt, ...)
{
    va_list ap;

    fprintf(curr_out, "***  {PRISM BPX ERROR: ");
    va_start(ap, fmt);
    vfprintf(curr_out, fmt, ap);
    va_end(ap);
    fprintf(curr_out, "}\n");

    bp4p_quit(1);
}

/*--------------------------------------------------------------------*/

bool bpx_is_var(TERM t)
{
    XDEREF(t);
    return ISREF(t);
}

bool bpx_is_atom(TERM t)
{
    XDEREF(t);
    return ISATOM(t);
}

bool bpx_is_integer(TERM t)
{
    XDEREF(t);
    return ISINT(t);
}

bool bpx_is_float(TERM t)
{
    XDEREF(t);
    return ISNUM(t);
}

bool bpx_is_nil(TERM t)
{
    XDEREF(t);
    return ISNIL(t);
}

bool bpx_is_list(TERM t)
{
    XDEREF(t);
    return ISLIST(t);
}

bool bpx_is_structure(TERM t)
{
    XDEREF(t);
    return ISSTRUCT(t);
}

bool bpx_is_compound(TERM t)
{
    XDEREF(t);
    return ISCOMPOUND(t);
}

bool bpx_is_unifiable(TERM t1, TERM t2)
{
    XDEREF(t1);
    XDEREF(t2);
    return (bool)(is_UNIFIABLE(t1, t2));
}

bool bpx_is_identical(TERM t1, TERM t2)
{
    XDEREF(t1);
    XDEREF(t2);
    return (bool)(is_IDENTICAL(t1, t2));
}

/*--------------------------------------------------------------------*/

TERM bpx_get_call_arg(BPLONG i, BPLONG arity)
{
  CACHE_REGS

  if (i < 1 || i > arity) {
    bpx_raise("index out of range");
  }
  return ARG(i, arity);
}

BPLONG bpx_get_integer(TERM t)
{
    XDEREF(t);

    if (ISINT(t)) {
        return INTVAL(t);
    }
    else {
        bpx_raise("integer expected");
    }
}

double bpx_get_float(TERM t)
{
    XDEREF(t);

    if (ISINT(t)) {
        return (double)(INTVAL(t));
    }
    else if (ISFLOAT(t)) {
        return floatval(t);
    }
    else {
        bpx_raise("integer or floating number expected");
    }
}

const char * bpx_get_name(TERM t)
{
    XDEREF(t);

    switch (XTAG(t)) {
    case STR:
        return GET_NAME_STR(GET_STR_SYM_REC(t));
    case ATM:
        return GET_NAME_ATOM(GET_ATM_SYM_REC(t));
    case LST:
        return ".";
    default:
        bpx_raise("callable expected");
    }
}

int bpx_get_arity(TERM t)
{
    XDEREF(t);

    switch (XTAG(t)) {
    case STR:
        return GET_ARITY_STR(GET_STR_SYM_REC(t));
    case ATM:
        return GET_ARITY_ATOM(GET_ATM_SYM_REC(t));
    case LST:
        return 2;
    default:
        bpx_raise("callable expected");
    }
}

TERM bpx_get_arg(BPLONG i, TERM t)
{
    BPLONG n, j;

    XDEREF(t);

    switch (XTAG(t)) {
    case STR:
        n = GET_ARITY_STR(GET_STR_SYM_REC(t));
        j = 0;
        break;
    case LST:
        n = 2;
        j = 1;
        break;
    default:
        bpx_raise("compound expected");
    }

    if (i < 1 || i > n) {
        bpx_raise("bad argument index");
    }
    return GET_ARG(t, i - j);
}

TERM bpx_get_car(TERM t)
{
    XDEREF(t);

    if (ISLIST(t)) {
        return GET_CAR(t);
    }
    else {
        bpx_raise("list expected");
    }
}

TERM bpx_get_cdr(TERM t)
{
    XDEREF(t);

    if (ISLIST(t)) {
        return GET_CDR(t);
    }
    else {
        bpx_raise("list expected");
    }
}

/*--------------------------------------------------------------------*/

TERM bpx_build_var(void)
{
  CACHE_REGS
    TERM term;

    REQUIRE_HEAP(1);
    term = (TERM)(heap_top);
    NEW_HEAP_FREE;
    return term;
}

TERM bpx_build_integer(BPLONG n)
{
    return MAKEINT(n);
}

TERM bpx_build_float(double x)
{
  CACHE_REGS
    REQUIRE_HEAP(4);
    return encodefloat1(x PASS_REGS);
}

TERM bpx_build_atom(const char *name)
{
    SYM_REC_PTR sym;

    sym = insert(name, strlen(name), 0);
    return ADDTAG(sym, ATM);
}

TERM bpx_build_list(void)
{
  CACHE_REGS
    TERM term;

    REQUIRE_HEAP(2);
    term = ADDTAG(heap_top, LST);
    NEW_HEAP_FREE;
    NEW_HEAP_FREE;
    return term;
}

TERM bpx_build_nil(void)
{
    return nil_sym;
}

TERM bpx_build_structure(const char *name, BPLONG arity)
{
  CACHE_REGS
    SYM_REC_PTR sym;
    TERM term;

    REQUIRE_HEAP(arity + 1);
    term = ADDTAG(heap_top, STR);
    sym = insert(name, strlen(name), arity);
    NEW_HEAP_NODE((TERM)(sym));
    while (--arity >= 0) {
        NEW_HEAP_FREE;
    }
    return term;
}

/*--------------------------------------------------------------------*/

bool bpx_unify(TERM t1, TERM t2)
{
    return (bool)(unify(t1, t2));
}

/*--------------------------------------------------------------------*/

TERM bpx_string_2_term(const char *s)
{
  CACHE_REGS
    TERM term, vars;
    int  result;

    REQUIRE_HEAP(2);
    term = (TERM)(heap_top);
    NEW_HEAP_FREE;
    vars = (TERM)(heap_top);
    NEW_HEAP_FREE;

    result = bp_string_2_term(s, term, vars);
    if (result != BP_TRUE) {
        bpx_raise("parsing failed -- %s", s);
    }
    return term;
}

const char * bpx_term_2_string(TERM t)
{
    XDEREF(t);
    return bp_term_2_string(t);
}

/*--------------------------------------------------------------------*/

int bpx_call_term(TERM t)
{
    XDEREF(t);
    return bp_call_term(t);
}

int bpx_call_string(const char *s)
{
    return bp_call_term(bpx_string_2_term(s));
}

int bpx_mount_query_term(TERM t)
{
    XDEREF(t);
    return bp_mount_query_term(t);
}

int bpx_mount_query_string(const char *s)
{
    return bp_mount_query_term(bpx_string_2_term(s));
}

int bpx_next_solution(void)
{
    if (curr_toam_status == TOAM_NOTSET) {
        bpx_raise("no goal mounted");
    }
    return bp_next_solution();
}

/*--------------------------------------------------------------------*/

void bpx_write(TERM t)
{
    XDEREF(t);
    write_term(t);
}

/*--------------------------------------------------------------------*/

int bpx_printf(const char *fmt, ...)
{
    va_list ap;
    int r;

    va_start(ap, fmt);
    r = vfprintf(curr_out, fmt, ap);
    va_end(ap);

    return r;
}

/*--------------------------------------------------------------------*/

#ifdef __YAP_PROLOG__
BPLONG toam_signal_vec;

BPLONG illegal_arguments;
BPLONG failure_atom;
BPLONG number_var_exception;
#endif
