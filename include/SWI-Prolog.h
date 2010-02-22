/* yap2swi.h  */
/*
 * Project: SWI emulation for Yap Prolog
 * Author: Steve Moyle and Vitor Santos Costa
 * Email:  steve.moyle@comlab.ox.ac.uk
 * Date:   21 January 2002

 * Copyright (c) 2002 Steve Moyle and Vitor Santos Costa.  All rights reserved.
 

*/

#ifndef _FLI_H_INCLUDED
#define _FLI_H_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

//=== includes ===============================================================
#ifdef          _YAP_NOT_INSTALLED_
#include	"config.h"
#include	"YapInterface.h"
#else
#include	<Yap/config.h>
#include	<Yap/YapInterface.h>
#endif
#include	<stdarg.h>
#include	<stdlib.h>
#include        <stddef.h>
#include        <wchar.h>
#if HAVE_TIME_H
#include <time.h>
#endif

#ifndef X_API
#if (defined(_MSC_VER) || defined(__MINGW32__)) && defined(PL_KERNEL)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif
#endif


		 /*******************************
		 *	       EXPORT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See SWI-Prolog.h, containing the same code   for  an explanation on this
stuff.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef _PL_EXPORT_DONE
#define _PL_EXPORT_DONE

#if (defined(__WINDOWS__) || defined(__CYGWIN__) || defined(__MINGW32__)) && !defined(__LCC__)
#define HAVE_DECLSPEC
#endif

#ifdef HAVE_DECLSPEC
# ifdef PL_KERNEL
#define PL_EXPORT(type)		__declspec(dllexport) type
#define PL_EXPORT_DATA(type)	__declspec(dllexport) type
#define install_t	 	void
# else
#  ifdef __BORLANDC__
#define PL_EXPORT(type)	 	type _stdcall
#define PL_EXPORT_DATA(type)	extern type
#  else
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	__declspec(dllimport) type
#  endif
#define install_t	 	__declspec(dllexport) void
# endif
#else /*HAVE_DECLSPEC*/
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	extern type
#define install_t	 	void
#endif /*HAVE_DECLSPEC*/
#endif /*_PL_EXPORT_DONE*/

typedef	unsigned long    term_t;
typedef	void *module_t;
typedef	void *record_t;
typedef unsigned long	atom_t;
typedef	YAP_Term    *predicate_t;
typedef struct  open_query_struct *qid_t;
typedef long    functor_t;
typedef int     (*PL_agc_hook_t)(atom_t);
typedef unsigned long	foreign_t;	/* return type of foreign functions */
typedef wchar_t pl_wchar_t;             /* wide character support */
#ifdef WIN32
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#else
#include <inttypes.h>			/* more portable than stdint.h */
#endif
#if  !defined(_MSC_VER) 
typedef uintptr_t	PL_fid_t;	/* opaque foreign context handle */
#endif

typedef void *function_t;

#define fid_t PL_fid_t			/* avoid AIX name-clash */

typedef struct _PL_extension
{ const char 		*predicate_name;	/* Name of the predicate */
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
#define PL_FA_CREF		(0x10)	/* Internal: has clause-reference */
#define PL_FA_ISO		(0x20)	/* Internal: ISO core predicate */

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
#define PL_NCHARS	 (23)		/* unsigned, const char * */
#define PL_UTF8_CHARS	 (24)		/* const char * */
#define PL_UTF8_STRING	 (25)		/* const char * */
#define PL_INT64	 (26)		/* int64_t */
#define PL_NUTF8_CHARS	 (27)		/* unsigned, const char * */
#define PL_NUTF8_CODES	 (29)		/* unsigned, const char * */
#define PL_NUTF8_STRING	 (30)		/* unsigned, const char * */
#define PL_NWCHARS	 (31)		/* unsigned, const wchar_t * */
#define PL_NWCODES	 (32)		/* unsigned, const wchar_t * */
#define PL_NWSTRING	 (33)		/* unsigned, const wchar_t * */
#define PL_MBCHARS	 (34)		/* const char * */
#define PL_MBCODES	 (35)		/* const char * */
#define PL_MBSTRING	 (36)		/* const char * */
#define PL_INTPTR        (37)           /* intptr_t */
#define PL_CHAR          (38)           /* int */
#define PL_CODE          (39)           /* int */
#define PL_BYTE          (40)           /* int */

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

#define CVT_EXCEPTION	0x10000
#define CVT_VARNOFAIL	0x20000		/* return 2 if argument is unbound */

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Foreign language interface definitions.  Note that these macros MUST  be
consistent  with  the  definitions  in  pl-itf.h, which is included with
users foreign language code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define NOTRACE PL_FA_NOTRACE
#define META    PL_FA_TRANSPARENT
#define NDET	PL_FA_NONDETERMINISTIC
#define VA	PL_FA_VARARGS
#define CREF	PL_FA_CREF
#define ISO	PL_FA_ISO

typedef enum
{ FRG_FIRST_CALL = 0,		/* Initial call */
  FRG_CUTTED     = 1,		/* Context was cutted */
  FRG_REDO	 = 2		/* Normal redo */
} frg_code;

struct foreign_context
{ uintptr_t		context;	/* context value */
  frg_code		control;	/* FRG_* action */
  struct PL_local_data *engine;		/* invoking engine */
};

typedef struct foreign_context *control_t;

#define PRED_IMPL(name, arity, fname, flags) \
        foreign_t \
        pl_ ## fname ## _va(term_t PL__t0, int PL__ac, control_t PL__ctx)

#define Arg(N)  (PL__t0+((n)-1))
#define A1      (PL__t0)
#define A2      (PL__t0+1)
#define A3      (PL__t0+2)
#define A3      (PL__t0+2)
#define A4      (PL__t0+3)
#define A5      (PL__t0+4)
#define A6      (PL__t0+5)
#define A7      (PL__t0+6)
#define A8      (PL__t0+7)
#define A9      (PL__t0+8)
#define A10     (PL__t0+9)

#define CTX_CNTRL ForeignControl(PL__ctx)
#define CTX_PTR   ForeignContextPtr(PL__ctx)
#define CTX_INT   ForeignContextInt(PL__ctx)
#define CTX_ARITY PL__ac

#define BeginPredDefs(id) \
        PL_extension PL_predicates_from_ ## id[] = {
#define PRED_DEF(name, arity, fname, flags) \
        { name, arity, pl_ ## fname ## _va, (flags)|PL_FA_VARARGS },
#define EndPredDefs \
        { NULL, 0, NULL, 0 } \
        };

#define FRG_REDO_MASK	0x00000003L
#define FRG_REDO_BITS	2
#define REDO_INT	0x02		/* Returned an integer */
#define REDO_PTR	0x03		/* returned a pointer */

#define ForeignRedoIntVal(v)	(((uintptr_t)(v)<<FRG_REDO_BITS)|REDO_INT)
#define ForeignRedoPtrVal(v)	(((uintptr_t)(v))|REDO_PTR)

#define ForeignRedoInt(v)	return ForeignRedoIntVal(v)
#define ForeignRedoPtr(v)	return ForeignRedoPtrVal(v)

#define ForeignControl(h)	((h)->control)
#define ForeignContextInt(h)	((intptr_t)(h)->context)
#define ForeignContextPtr(h)	((void *)(h)->context)
#define ForeignEngine(h)	((h)->engine)

#define FRG(n, a, f, flags) { n, a, f, flags }

/* end from pl-itf.h */

		 /*******************************
		 *	     CALL-BACK		*
		 *******************************/

#ifdef PL_KERNEL
#define PL_Q_DEBUG		0x01	/* = TRUE for backward compatibility */
#endif
#define PL_Q_NORMAL		0x02	/* normal usage */
#define PL_Q_NODEBUG		0x04	/* use this one */
#define PL_Q_CATCH_EXCEPTION	0x08	/* handle exceptions in C */
#define PL_Q_PASS_EXCEPTION	0x10	/* pass to parent environment */
#ifdef PL_KERNEL
#define PL_Q_DETERMINISTIC	0x20	/* call was deterministic */
#endif

#define PL_fail		return FALSE	/* fail */
#define PL_succeed	return TRUE	/* success */

extern X_API PL_agc_hook_t PL_agc_hook(PL_agc_hook_t);
extern X_API char* PL_atom_chars(atom_t);
extern X_API term_t PL_copy_term_ref(term_t);
extern X_API term_t PL_new_term_ref(void);
extern X_API term_t PL_new_term_refs(int);
extern X_API void PL_reset_term_refs(term_t);
/* begin PL_get_* functions =============================*/
extern X_API int PL_get_arg(int, term_t, term_t);
extern X_API int PL_get_atom(term_t, atom_t *);
extern X_API int PL_get_atom_chars(term_t, char **);
extern X_API int PL_get_bool(term_t, int *);
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
extern X_API module_t PL_context(void);
extern X_API int PL_strip_module(term_t, module_t *, term_t);
extern X_API atom_t PL_module_name(module_t);
extern X_API module_t PL_new_module(atom_t);
extern X_API int PL_get_name_arity(term_t, atom_t *, int *);
extern X_API int PL_get_nil(term_t);
extern X_API int PL_get_pointer(term_t, void **);
extern X_API int PL_get_intptr(term_t, intptr_t *);
extern X_API int PL_get_string(term_t, char **, int *);
extern X_API int PL_get_tail(term_t, term_t);
/* end PL_get_* functions  =============================*/
/* begin PL_new_* functions =============================*/
extern X_API atom_t PL_new_atom(const char *);
extern X_API atom_t PL_new_atom_wchars(int, const pl_wchar_t *);
extern X_API char *PL_atom_nchars(atom_t, size_t *);
extern X_API pl_wchar_t *PL_atom_wchars(atom_t, size_t *);
extern X_API functor_t PL_new_functor(atom_t, int);
extern X_API atom_t PL_functor_name(functor_t);
extern X_API int PL_functor_arity(functor_t);
/* end PL_new_* functions =============================*/
/* begin PL_put_* functions =============================*/
extern X_API int PL_cons_functor(term_t, functor_t,...);
extern X_API int PL_cons_functor_v(term_t, functor_t,term_t);
extern X_API int PL_cons_list(term_t, term_t, term_t);
extern X_API int PL_put_atom(term_t, atom_t);
extern X_API int PL_put_atom_chars(term_t, const char *);
extern X_API int PL_put_float(term_t, double);
extern X_API int PL_put_functor(term_t, functor_t t);
extern X_API int PL_put_int64(term_t, int64_t);
extern X_API int PL_put_integer(term_t, long);
extern X_API int PL_put_list(term_t);
extern X_API int PL_put_list_chars(term_t, const char *);
extern X_API void PL_put_nil(term_t);
extern X_API int PL_put_pointer(term_t, void *);
extern X_API int PL_put_string_chars(term_t, const char *);
extern X_API int PL_put_term(term_t, term_t);
extern X_API int PL_put_variable(term_t);
extern X_API  int PL_compare(term_t, term_t);
/* end PL_put_* functions =============================*/
/* begin PL_unify_* functions =============================*/
extern X_API  int PL_unify(term_t, term_t);
extern X_API  int PL_unify_atom(term_t, atom_t);
extern X_API  int PL_unify_arg(int, term_t, atom_t);
extern X_API  int PL_unify_atom_chars(term_t, const char *);
extern X_API  int PL_unify_atom_nchars(term_t, size_t len, const char *);
extern X_API  int PL_unify_float(term_t, double);
extern X_API  int PL_unify_functor(term_t, functor_t);
extern X_API  int PL_unify_int64(term_t, int64_t);
extern X_API  int PL_unify_integer(term_t, long);
extern X_API  int PL_unify_list(term_t, term_t, term_t);
extern X_API  int PL_unify_list_chars(term_t, const char *);
extern X_API  int PL_unify_nil(term_t);
extern X_API  int PL_unify_pointer(term_t, void *);
extern X_API  int PL_unify_string_chars(term_t, const char *);
extern X_API  int PL_unify_term(term_t,...);
extern X_API  int PL_unify_chars(term_t, int, size_t, const char *);
extern X_API  int PL_unify_chars_diff(term_t, term_t, int, size_t, const char *);
extern X_API  int PL_unify_wchars(term_t, int, size_t, const pl_wchar_t *);
extern X_API  int PL_unify_wchars_diff(term_t, term_t, int, size_t, const pl_wchar_t *);
extern X_API  int PL_chars_to_term(const char *,term_t);
/* begin PL_is_* functions =============================*/
extern X_API  int PL_is_atom(term_t);
extern X_API  int PL_is_atomic(term_t);
extern X_API  int PL_is_compound(term_t);
extern X_API  int PL_is_float(term_t);
extern X_API  int PL_is_functor(term_t, functor_t);
extern X_API  int PL_is_ground(term_t);
extern X_API  int PL_is_integer(term_t);
extern X_API  int PL_is_list(term_t);
extern X_API  int PL_is_number(term_t);
extern X_API  int PL_is_string(term_t);
extern X_API  int PL_is_variable(term_t);
extern X_API  int PL_term_type(term_t);
extern X_API  int PL_is_inf(term_t);
/* end PL_is_* functions =============================*/
extern X_API void PL_halt(int);
extern X_API  int  PL_initialise(int, char **);
extern X_API  int  PL_is_initialised(int *, char ***);
extern X_API void PL_close_foreign_frame(fid_t);
extern X_API void PL_discard_foreign_frame(fid_t);
extern X_API void PL_rewind_foreign_frame(fid_t);
extern X_API fid_t PL_open_foreign_frame(void);
extern X_API int PL_raise_exception(term_t);
extern X_API void PL_clear_exception(void);
extern X_API void PL_register_atom(atom_t);
extern X_API void PL_unregister_atom(atom_t);
extern X_API predicate_t PL_pred(functor_t, module_t);
extern X_API predicate_t PL_predicate(const char *, int, const char *);
extern X_API void PL_predicate_info(predicate_t, atom_t *, int *, module_t *);
extern X_API qid_t PL_open_query(module_t, int, predicate_t, term_t);
extern X_API int PL_next_solution(qid_t);
extern X_API void PL_cut_query(qid_t);
extern X_API void PL_close_query(qid_t);
extern X_API int PL_toplevel(void);
extern X_API term_t PL_exception(qid_t);
extern X_API term_t PL_exception(qid_t);
extern X_API int PL_call_predicate(module_t, int, predicate_t, term_t);
extern X_API int PL_call(term_t, module_t);
extern X_API void PL_register_foreign(const char *, int, foreign_t (*)(void), int);
extern X_API void PL_register_foreign_in_module(const char *, const char *, int, foreign_t (*)(void), int);
extern X_API void PL_register_extensions(const PL_extension *);
extern X_API void PL_load_extensions(const PL_extension *);
extern X_API int PL_handle_signals(void);
extern X_API int  PL_thread_self(void);
extern X_API int PL_thread_attach_engine(const PL_thread_attr_t *);
extern X_API int PL_thread_destroy_engine(void);
extern X_API int PL_thread_at_exit(void (*)(void *), void *, int);
extern X_API PL_engine_t PL_create_engine(const PL_thread_attr_t *);
extern X_API int PL_destroy_engine(PL_engine_t);
extern X_API int PL_set_engine(PL_engine_t,PL_engine_t *);
extern X_API int PL_get_string_chars(term_t, char **, int *);
extern X_API record_t PL_record(term_t);
extern X_API int PL_recorded(record_t, term_t);
extern X_API void PL_erase(record_t);
extern X_API int PL_action(int,...);
extern X_API void *PL_malloc(int);
extern X_API void *PL_realloc(void*,int);
extern X_API void PL_free(void *);
extern X_API int  PL_eval_expression_to_int64_ex(term_t t, int64_t *val);

extern X_API int Sprintf(const char * fm,...);
extern X_API int Sdprintf(const char *,...);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Output   representation   for   PL_get_chars()     and    friends.   The
prepresentation type REP_FN is for   PL_get_file_name()  and friends. On
Windows we use UTF-8 which is translated   by the `XOS' layer to Windows
UNICODE file functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define REP_ISO_LATIN_1 0x0000		/* output representation */
#define REP_UTF8 	0x1000
#define REP_MB		0x2000
#ifdef __WINDOWS__
#define REP_FN		REP_UTF8
#else
#define REP_FN		REP_MB
#endif

#define PL_DIFF_LIST	0x20000		/* PL_unify_chars() */

#ifdef SIO_MAGIC			/* defined from <SWI-Stream.h> */
					/* Make IOSTREAM known to Prolog */
PL_EXPORT(int)  	PL_open_stream(term_t t, IOSTREAM *s);
PL_EXPORT(int)  	PL_unify_stream(term_t t, IOSTREAM *s);
#define PL_open_stream  PL_unify_stream
PL_EXPORT(int)  	PL_get_stream_handle(term_t t, IOSTREAM **s);
PL_EXPORT(int) 		PL_release_stream(IOSTREAM *s);

#endif

#define succeed			return TRUE
#define fail			return FALSE

extern X_API  const char *PL_cwd(void);

void swi_install(void);

X_API int PL_error(const char *pred, int arity, const char *msg, int id, ...);
X_API int PL_warning(const char *msg, ...);


		/********************************
		* NON-DETERMINISTIC CALL/RETURN *
		*********************************/

/*  Note 1: Non-deterministic foreign functions may also use the deterministic
    return methods PL_succeed and PL_fail.

    Note 2: The argument to PL_retry is a sizeof(ptr)-2 bits signed
    integer (use type intptr_t).
*/

#define PL_FIRST_CALL		(0)
#define PL_CUTTED		(1)
#define PL_REDO			(2)

#define PL_retry(n)		return _PL_retry(n)
#define PL_retry_address(a)	return _PL_retry_address(a)

PL_EXPORT(foreign_t)	_PL_retry(intptr_t);
PL_EXPORT(foreign_t)	_PL_retry_address(void *);
PL_EXPORT(int)	 	PL_foreign_control(control_t);
PL_EXPORT(intptr_t)	PL_foreign_context(control_t);
PL_EXPORT(void *)	PL_foreign_context_address(control_t);



#ifdef __cplusplus
}
#endif

#endif /* _FLI_H_INCLUDED */

