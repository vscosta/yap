/* yap2swi.h  */
/*
 * Project: jpl for Yap Prolog
 * Author: Steve Moyle and Vitor Santos Costa
 * Email:  steve.moyle@comlab.ox.ac.uk
 * Date:   21 January 2002

 * Copyright (c) 2002 Steve Moyle and Vitor Santos Costa.  All rights reserved.
 

*/


//=== includes ===============================================================
#include "config.h"
#include	<YapInterface.h>
#include	<stdarg.h>
#if HAVE_TIME_H
#include <time.h>
#endif
#if USE_GMP
#include <gmp.h>
#endif

#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif

typedef	unsigned long    fid_t;
typedef	unsigned long    term_t;
typedef	void *module_t;
typedef unsigned long	atom_t;
typedef	YAP_Term    *predicate_t;
typedef struct  open_query_struct *qid_t;
typedef long    functor_t;
typedef int     (*PL_agc_hook_t)(atom_t);
typedef unsigned long	foreign_t;	/* return type of foreign functions */
#ifdef WIN32
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#else
#include <inttypes.h>			/* more portable than stdint.h */
#endif

typedef void *function_t;

typedef struct _PL_extension
{ char 		*predicate_name;	/* Name of the predicate */
  short		arity;			/* Arity of the predicate */
  function_t	function;		/* Implementing functions */
  short		flags;			/* Or of PL_FA_... */
} PL_extension;

typedef struct
{ unsigned long	    local_size;		/* Stack sizes */
  unsigned long	    global_size;
  unsigned long	    trail_size;
  unsigned long	    argument_size;
  char *	    alias;		/* alias name */
  int		  (*cancel)(int id);	/* cancel function */
  void *	    reserved[5];	/* reserved for extensions */
} PL_thread_attr_t;

typedef void *PL_engine_t;


#define PL_FA_NOTRACE		(0x01)	/* foreign cannot be traced */
#define PL_FA_TRANSPARENT	(0x02)	/* foreign is module transparent */
#define PL_FA_NONDETERMINISTIC	(0x04)	/* foreign is non-deterministic */
#define PL_FA_VARARGS		(0x08)	/* call using t0, ac, ctx */

/* begin from pl-itf.h */
#define PL_VARIABLE      (1)            /* nothing */
#define PL_ATOM          (2)            /* const char * */
#define PL_INTEGER       (3)            /* int */
#define PL_FLOAT         (4)            /* double */
#define PL_STRING        (5)            /* const char * */
#define PL_TERM          (6)

                                        /* PL_unify_term() */
#define PL_FUNCTOR       (10)           /* functor_t, arg ... */
#define PL_LIST          (11)           /* length, arg ... */
#define PL_CHARS         (12)           /* const char * */
#define PL_POINTER       (13)           /* void * */
                                        /* PlArg::PlArg(text, type) */
#define PL_CODE_LIST     (14)           /* [ascii...] */
#define PL_CHAR_LIST     (15)           /* [h,e,l,l,o] */
#define PL_BOOL          (16)           /* PL_set_feature() */
#define PL_FUNCTOR_CHARS (17)           /* PL_unify_term() */
#define PL_PREDICATE_INDICATOR (18)    /* predicate_t (Procedure) */
#define PL_SHORT         (19)           /* short */
#define PL_INT           (20)           /* int */
#define PL_LONG          (21)           /* long */
#define PL_DOUBLE        (22)           /* double */

#define CVT_ATOM	0x0001
#define CVT_STRING	0x0002
#define CVT_LIST	0x0004
#define CVT_INTEGER	0x0008
#define CVT_FLOAT	0x0010
#define CVT_VARIABLE	0x0020
#define CVT_NUMBER	(CVT_INTEGER|CVT_FLOAT)
#define CVT_ATOMIC	(CVT_NUMBER|CVT_ATOM|CVT_STRING)
#define CVT_WRITE	0x0040		/* as of version 3.2.10 */
#define CVT_ALL		(CVT_ATOMIC|CVT_LIST)
#define CVT_MASK	0x00ff

#define BUF_DISCARDABLE	0x0000
#define BUF_RING	0x0100
#define BUF_MALLOC	0x0200

#define PL_ENGINE_CURRENT   ((PL_engine_t)-1)

#define PL_ENGINE_SET   0               /* engine set successfully */
#define PL_ENGINE_INVAL 2               /* engine doesn't exist */
#define PL_ENGINE_INUSE 3               /* engine is in use */

#define	PL_ACTION_TRACE		1	/* switch to trace mode */
#define PL_ACTION_DEBUG		2	/* switch to debug mode */
#define PL_ACTION_BACKTRACE	3	/* show a backtrace (stack dump) */
#define PL_ACTION_BREAK		4	/* create a break environment */
#define PL_ACTION_HALT		5	/* halt Prolog execution */
#define PL_ACTION_ABORT		6	/* generate a Prolog abort */
					/* 7: Obsolete PL_ACTION_SYMBOLFILE */
#define PL_ACTION_WRITE		8	/* write via Prolog i/o buffer */
#define PL_ACTION_FLUSH		9	/* Flush Prolog i/o buffer */
#define PL_ACTION_GUIAPP	10	/* Win32: set when this is a gui */
#define PL_ACTION_ATTACH_CONSOLE 11	/* MT: Attach a console */

/* end from pl-itf.h */

/* copied from old SICStus/SWI interface */
typedef void install_t;

extern X_API PL_agc_hook_t PL_agc_hook(PL_agc_hook_t);
extern X_API void   PL_free(void *);
extern X_API char*  PL_atom_chars(atom_t);
extern X_API term_t PL_copy_term_ref(term_t);
extern X_API term_t PL_new_term_ref(void);
extern X_API term_t PL_new_term_refs(int);
extern X_API void PL_reset_term_refs(term_t);
/* begin PL_get_* functions =============================*/
extern X_API int PL_get_arg(int, term_t, term_t);
extern X_API int PL_get_atom(term_t, atom_t *);
extern X_API int PL_get_atom_chars(term_t, char **);
extern X_API int PL_get_chars(term_t, char **, unsigned);
extern X_API int PL_get_nchars(term_t, size_t *, char **, unsigned);
extern X_API int PL_get_wchars(term_t, size_t *, wchar_t **, unsigned);
extern X_API int PL_get_functor(term_t, functor_t *);
extern X_API int PL_get_float(term_t, double *);
extern X_API int PL_get_head(term_t, term_t);
extern X_API int PL_get_int64(term_t, int64_t *);
extern X_API int PL_get_integer(term_t, int *);
extern X_API int PL_get_list(term_t, term_t, term_t);
extern X_API int PL_get_long(term_t, long *);
extern X_API int PL_get_list_chars(term_t, char **, unsigned);
extern X_API int PL_get_module(term_t, module_t *);
extern X_API atom_t PL_module_name(module_t);
extern X_API module_t PL_new_module(atom_t);
extern X_API int PL_get_name_arity(term_t, atom_t *, int *);
extern X_API int PL_get_nil(term_t);
extern X_API int PL_get_pointer(term_t, void **);
extern X_API int PL_get_string(term_t, char **, int *);
extern X_API int PL_get_tail(term_t, term_t);
/* end PL_get_* functions  =============================*/
/* begin PL_new_* functions =============================*/
extern X_API atom_t PL_new_atom(const char *);
extern X_API atom_t PL_new_atom_wchars(int, const wchar_t *);
extern X_API char *PL_atom_nchars(atom_t, size_t *);
extern X_API wchar_t *PL_atom_wchars(atom_t, size_t *);
extern X_API functor_t PL_new_functor(atom_t, int);
extern X_API atom_t PL_functor_name(functor_t);
extern X_API int PL_functor_arity(functor_t);
/* end PL_new_* functions =============================*/
/* begin PL_put_* functions =============================*/
extern X_API void PL_cons_functor(term_t, functor_t,...);
extern X_API void PL_cons_functor_v(term_t, functor_t,term_t);
extern X_API void PL_cons_list(term_t, term_t, term_t);
extern X_API void PL_put_atom(term_t, atom_t);
extern X_API void PL_put_atom_chars(term_t, const char *);
extern X_API void PL_put_float(term_t, double);
extern X_API void PL_put_functor(term_t, functor_t t);
extern X_API void PL_put_int64(term_t, int64_t);
extern X_API void PL_put_integer(term_t, long);
extern X_API void PL_put_list(term_t);
extern X_API void PL_put_list_chars(term_t, const char *);
extern X_API void PL_put_nil(term_t);
extern X_API void PL_put_pointer(term_t, void *);
extern X_API void PL_put_string_chars(term_t, const char *);
extern X_API void PL_put_term(term_t, term_t);
extern X_API void PL_put_variable(term_t);
extern X_API int PL_compare(term_t, term_t);
/* end PL_put_* functions =============================*/
/* begin PL_unify_* functions =============================*/
extern X_API int PL_unify(term_t, term_t);
extern X_API int PL_unify_atom(term_t, atom_t);
extern X_API int PL_unify_atom_chars(term_t, const char *);
extern X_API int PL_unify_float(term_t, double);
extern X_API int PL_unify_int64(term_t, int64_t);
extern X_API int PL_unify_integer(term_t, long);
extern X_API int PL_unify_list(term_t, term_t, term_t);
extern X_API int PL_unify_list_chars(term_t, const char *);
extern X_API int PL_unify_nil(term_t);
extern X_API int PL_unify_pointer(term_t, void *);
extern X_API int PL_unify_string_chars(term_t, const char *);
extern X_API int PL_unify_term(term_t,...);
/* end PL_unify_* functions =============================*/
/* begin PL_is_* functions =============================*/
extern X_API int PL_is_atom(term_t);
extern X_API int PL_is_atomic(term_t);
extern X_API int PL_is_compound(term_t);
extern X_API int PL_is_float(term_t);
extern X_API int PL_is_functor(term_t, functor_t);
extern X_API int PL_is_integer(term_t);
extern X_API int PL_is_list(term_t);
extern X_API int PL_is_number(term_t);
extern X_API int PL_is_string(term_t);
extern X_API int PL_is_variable(term_t);
extern X_API int PL_term_type(term_t);
/* end PL_is_* functions =============================*/
extern X_API void PL_halt(int);
extern X_API int  PL_initialise(int, char **);
extern X_API int  PL_is_initialised(int *, char ***);
extern X_API void PL_close_foreign_frame(fid_t);
extern X_API void PL_discard_foreign_frame(fid_t);
extern X_API fid_t PL_open_foreign_frame(void);
extern X_API int PL_raise_exception(term_t);
extern X_API void PL_register_atom(atom_t);
extern X_API void PL_unregister_atom(atom_t);
extern X_API predicate_t PL_pred(functor_t, module_t);
extern X_API predicate_t PL_predicate(const char *, int, const char *);
extern X_API void PL_predicate_info(predicate_t, atom_t *, int *, module_t *);
extern X_API qid_t PL_open_query(module_t, int, predicate_t, term_t);
extern X_API int PL_next_solution(qid_t);
extern X_API void PL_cut_query(qid_t);
extern X_API void PL_close_query(qid_t);
extern X_API term_t PL_exception(qid_t);
extern X_API int PL_call_predicate(module_t, int, predicate_t, term_t);
extern X_API int PL_call(term_t, module_t);
extern X_API void PL_register_extensions(PL_extension *);
extern X_API void PL_load_extensions(PL_extension *);
extern X_API int  PL_thread_self(void);
extern X_API int PL_thread_attach_engine(const PL_thread_attr_t *);
extern X_API int PL_thread_destroy_engine(void);
extern X_API int PL_thread_at_exit(void (*)(void *), void *, int);
extern X_API PL_engine_t PL_create_engine(const PL_thread_attr_t *);
extern X_API int PL_destroy_engine(PL_engine_t);
extern X_API int PL_set_engine(PL_engine_t,PL_engine_t *);
extern X_API int PL_get_string_chars(term_t, char **, int *);
extern X_API int PL_action(int,...);

extern X_API int Sprintf(char *,...);
extern X_API int Sdprintf(char *,...);

void swi_install(void);

