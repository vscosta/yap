#include <string.h>
#include "core/idtable.h"

/*--------------------------------------------------------------------*/

static ID_TABLE *g_table = NULL;  /* goals */
static ID_TABLE *s_table = NULL;  /* switches */
static ID_TABLE *i_table = NULL;  /* switch instances */

/*--------------------------------------------------------------------*/

/* cpreds.c */
char * bp_term_2_string(TERM);

/* unify.c */
int    unify(TERM, TERM);

int prism_goal_id_register(TERM term);
int prism_sw_id_register(TERM term);
int prism_sw_ins_id_register(TERM term);
int prism_goal_id_get(TERM term);
int prism_sw_id_get(TERM term);
int prism_sw_ins_id_get(TERM term);
int prism_goal_count(void);
int prism_sw_count(void);
int prism_sw_ins_count(void);
TERM prism_goal_term(IDNUM i);
TERM prism_sw_term(IDNUM i);
TERM prism_sw_ins_term(IDNUM i);
char * prism_goal_string(IDNUM i);
char * prism_sw_string(IDNUM i);
char * prism_sw_ins_string(IDNUM i);
char * copy_prism_goal_string(IDNUM i);
char * copy_prism_sw_string(IDNUM i);
char * copy_prism_sw_ins_string(IDNUM i);
int pc_prism_id_table_init_0(void);
int pc_prism_goal_id_register_2(void);
int pc_prism_sw_id_register_2(void);
int pc_prism_sw_ins_id_register_2(void);
int pc_prism_goal_id_get_2(void);
int pc_prism_sw_id_get_2(void);
int pc_prism_sw_ins_id_get_2(void);
int pc_prism_goal_count_1(void);
int pc_prism_sw_count_1(void);
int pc_prism_sw_ins_count_1(void);
int pc_prism_goal_term_2(void);
int pc_prism_sw_term_2(void);
int pc_prism_sw_ins_term_2(void);

/*--------------------------------------------------------------------*/

int prism_goal_id_register(TERM term)
{
    return id_table_register(g_table, term);
}

int prism_sw_id_register(TERM term)
{
    return id_table_register(s_table, term);
}

int prism_sw_ins_id_register(TERM term)
{
    return id_table_register(i_table, term);
}

int prism_goal_id_get(TERM term)
{
    return id_table_retrieve(g_table, term);
}

int prism_sw_id_get(TERM term)
{
    return id_table_retrieve(s_table, term);
}

int prism_sw_ins_id_get(TERM term)
{
    return id_table_retrieve(i_table, term);
}

int prism_goal_count(void)
{
    return id_table_count(g_table);
}

int prism_sw_count(void)
{
    return id_table_count(s_table);
}

int prism_sw_ins_count(void)
{
    return id_table_count(i_table);
}

TERM prism_goal_term(IDNUM i)
{
    return id_table_id2term(g_table, i);
}

TERM prism_sw_term(IDNUM i)
{
    return id_table_id2term(s_table, i);
}

TERM prism_sw_ins_term(IDNUM i)
{
    return id_table_id2term(i_table, i);
}

char * prism_goal_string(IDNUM i)
{
    return bp_term_2_string(prism_goal_term(i));
}

char * prism_sw_string(IDNUM i)
{
    return bp_term_2_string(prism_sw_term(i));
}

char * prism_sw_ins_string(IDNUM i)
{
    return bp_term_2_string(prism_sw_ins_term(i));
}

/* Note: the strings returned by strdup() should be released by the caller. */
char * copy_prism_goal_string(IDNUM i)
{
    return strdup(prism_goal_string(i));
}

char * copy_prism_sw_string(IDNUM i)
{
    return strdup(prism_sw_string(i));
}

char * copy_prism_sw_ins_string(IDNUM i)
{
    return strdup(prism_sw_ins_string(i));
}

/*--------------------------------------------------------------------*/

int pc_prism_id_table_init_0(void)
{
    if (g_table != NULL) id_table_delete(g_table);
    if (s_table != NULL) id_table_delete(s_table);
    if (i_table != NULL) id_table_delete(i_table);

    g_table = id_table_create();
    s_table = id_table_create();
    i_table = id_table_create();

    return BP_TRUE;
}

int pc_prism_goal_id_register_2(void)
{
  CACHE_REGS
    TERM term;
    IDNUM id;

    term = ARG(1,2);
    XDEREF(term);
    id = prism_goal_id_register(term);

    return unify(MAKEINT(id), ARG(2,2));
}

int pc_prism_sw_id_register_2(void)
{
  CACHE_REGS
    TERM term;
    IDNUM id;

    term = ARG(1,2);
    XDEREF(term);
    id = prism_sw_id_register(term);

    return unify(MAKEINT(id), ARG(2,2));
}

int pc_prism_sw_ins_id_register_2(void)
{
  CACHE_REGS
    TERM term;
    IDNUM id;

    term = ARG(1,2);
    XDEREF(term);
    id = prism_sw_ins_id_register(term);

    return unify(MAKEINT(id), ARG(2,2));
}

int pc_prism_goal_id_get_2(void)
{
  CACHE_REGS
    TERM term;
    IDNUM id;

    term = ARG(1,2);
    XDEREF(term);

    id = prism_goal_id_get(term);
    if (id == ID_NONE) return BP_FALSE;

    return unify(MAKEINT(id), ARG(2,2));
}

int pc_prism_sw_id_get_2(void)
{
  CACHE_REGS
    TERM term;
    IDNUM id;

    term = ARG(1,2);
    XDEREF(term);
    id = prism_sw_id_get(term);
    if (id == ID_NONE) return BP_FALSE;

    return unify(MAKEINT(id), ARG(2,2));
}

int pc_prism_sw_ins_id_get_2(void)
{
  CACHE_REGS
    TERM term;
    IDNUM id;

    term = ARG(1,2);
    XDEREF(term);
    id = prism_sw_ins_id_get(term);
    if (id == ID_NONE) return BP_FALSE;

    return unify(MAKEINT(id), ARG(2,2));
}

int pc_prism_goal_count_1(void)
{
  CACHE_REGS
    return unify(MAKEINT(prism_goal_count()), ARG(1,1));
}

int pc_prism_sw_count_1(void)
{
  CACHE_REGS
    return unify(MAKEINT(prism_sw_count()), ARG(1,1));
}

int pc_prism_sw_ins_count_1(void)
{
  CACHE_REGS
    return unify(MAKEINT(prism_sw_ins_count()), ARG(1,1));
}

int pc_prism_goal_term_2(void)
{
  CACHE_REGS
    TERM id, term;

    id = ARG(1,2);
    XDEREF(id);
    term = unnumber_var_term(prism_goal_term((IDNUM)INTVAL(id)));

    return unify(term, ARG(2,2));
}

int pc_prism_sw_term_2(void)
{
  CACHE_REGS
    TERM id, term;

    id = ARG(1,2);
    XDEREF(id);

    term = unnumber_var_term(prism_sw_term((IDNUM)INTVAL(id)));

    return unify(term, ARG(2,2));
}

int pc_prism_sw_ins_term_2(void)
{
  CACHE_REGS
    TERM id, term;

    id = ARG(1,2);
    XDEREF(id);
    term = unnumber_var_term(prism_sw_ins_term((IDNUM)INTVAL(id)));

    return unify(term, ARG(2,2));
}
