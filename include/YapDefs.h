/*************************************************************************
 *									 *
 *	 YAP Prolog 	@(#)c_interface.h	2.2			 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		yap_structs.h *
 * Last rev:	15/5/2000						 *
 * mods: *
 * comments:	Data structures and defines used in the Interface	 *
 *									 *
 *************************************************************************/

#ifndef _YAPDEFS_H

#define _YAPDEFS_H 1

/**
 * X_API macro
 *
 * @brief Linux exports all symbols by default, but WIN32 does
 * not. cmake can enable exports, using CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS
 *
 * @param _WIN32
 *
 * @return
 */
#if _WIN32
#if defined(_EXPORT_KERNEL)
// __declspec(dllexport)
#define X_API
#else
// __declspec(dllimport)
#define X_API
#endif
// __declspec(dllexport)
#define O_API
// __declspec(dllimport)
#define I_API
#else
#define O_API
#define I_API
#define X_API
#endif

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>


#include "YapConfig.h"

typedef void *YAP_PredEntryPtr;

typedef size_t YAP_Arity;

typedef YAP_Term YAP_Module;

typedef YAP_Int YAP_handle_t;

typedef void *YAP_PredEntryPtr;

typedef YAP_Bool (*YAP_UserCPred)(void);

typedef int (*YAP_agc_hook)(void *_Atom);

#include "YapError.h"

#include "YapEncoding.h"

typedef encoding_t YAP_encoding_t;

#if __ANDROID__
#include <android/asset_manager.h>
#include <android/native_activity.h>
#endif

typedef struct YAP_thread_attr_struct {
  size_t ssize;
  size_t tsize;
  size_t sysize;
  int (*cancel)(int thread);
  YAP_Term egoal, alias;
} YAP_thread_attr;

#ifdef YAP_H
#include <threads.h>
#endif

typedef enum {
  YAP_BIN = 0x0001,
  YAP_TEXT = 0x0002,
  YAP_SAVED_STATE = 0x0004,
  YAP_OBJ = 0x0008,
  YAP_PL = 0x0010,
  YAP_SOURCE_PL = 0x0030,
  YAP_QLY = 0x0040,
  YAP_EXE = 0x0080,
  YAP_FOUND_BOOT_ERROR = 0x0100,
  YAP_DIR = 0x0200
} YAP_file_type_t;

#define YAP_ANY_FILE (0x00ff)

typedef enum {
  YAP_TAG_ATT = 0x1,
  YAP_TAG_UNBOUND = 0x2,
  YAP_TAG_REF = 0x4,
  YAP_TAG_PAIR = 0x8,
  YAP_TAG_ATOM = 0x10,
  YAP_TAG_INT = 0x20,
  YAP_TAG_LONG_INT = 0x40,
  YAP_TAG_BIG_INT = 0x80,
  YAP_TAG_RATIONAL = 0x100,
  YAP_TAG_FLOAT = 0x200,
  YAP_TAG_OPAQUE = 0x400,
  YAP_TAG_APPL = 0x800,
  YAP_TAG_DBREF = 0x1000,
  YAP_TAG_STRING = 0x2000,
  YAP_TAG_ARRAY = 0x4000
} YAP_tag_t;

#define YAP_WRITE_QUOTED 1
#define YAP_WRITE_IGNORE_OPS 2
#define YAP_WRITE_HANDLE_VARS 4
#define YAP_WRITE_USE_PORTRAY 8
#define YAP_WRITE_HANDLE_CYCLES 0x20
#define YAP_WRITE_BACKQUOTE_STRING 0x80
#define YAP_WRITE_ATTVAR_NONE 0x100
#define YAP_WRITE_ATTVAR_DOTS 0x200
#define YAP_WRITE_ATTVAR_PORTRAY 0x400
#define YAP_WRITE_BLOB_PORTRAY 0x800

#include "YapInit.h"

/* this should be opaque to the user */
typedef struct {
  unsigned long b0, b_entry, b_exit;      //> choice-point at entry
  YAP_handle_t CurSlot; //> variables at entry
  YAP_handle_t EndSlot; //> variables at successful execution
  struct yami *p;       //> Program Counter at entry
  struct yami *cp;      //> Continuation PC at entry
  int lvl;
  long env0;
  unsigned long tr, h;
} YAP_dogoalinfo;

// query manipulation support

typedef struct open_query_struct {
  int q_open;
  int q_state;
  YAP_handle_t q_g;
  struct pred_entry *q_pe;
  struct yami *q_p, *q_cp;
  jmp_buf q_env;
  int q_flags;
  YAP_dogoalinfo q_h;
  struct open_query_struct *oq;
} YAP_openQuery;

typedef void (*YAP_halt_hook)(int exit_code, void *closure);

/** Interface to opaque variables */

/* each type has a tag */
typedef YAP_Int YAP_opaque_tag_t;

typedef YAP_Bool (*YAP_Opaque_CallOnFail)(YAP_Term);
typedef YAP_Bool (*YAP_Opaque_CallOnCut)(YAP_Term);
typedef YAP_Bool (*YAP_Opaque_CallOnWrite)(FILE *, YAP_opaque_tag_t, void *,
                                           int);
typedef YAP_Int (*YAP_Opaque_CallOnGCMark)(YAP_opaque_tag_t, void *, YAP_Term *,
                                           YAP_Int);
typedef YAP_Bool (*YAP_Opaque_CallOnGCRelocate)(YAP_opaque_tag_t, void *,
                                                YAP_Term *, YAP_Int);
/// opaque variables can interact with the system
typedef struct YAP_opaque_handler_struct {
  YAP_Opaque_CallOnCut cut_handler; //< called at cut, which may be a forward
                                    // cut or an exception.
  YAP_Opaque_CallOnFail
      fail_handler; //< called at exit, it can be used to cleanup resources
  YAP_Opaque_CallOnWrite write_handler; //< text representation
  YAP_Opaque_CallOnGCMark
      mark_handler; //< useful if you include pointers to stack
  YAP_Opaque_CallOnGCRelocate
      relocate_handler; //< useful if you include pointers to stack
} YAP_opaque_handler_t;

extern YAP_Opaque_CallOnWrite Yap_blob_write_handler_from_slot(YAP_Int slot);
extern YAP_Opaque_CallOnGCMark Yap_blob_gc_mark_handler(YAP_Term t);
extern YAP_Opaque_CallOnGCRelocate Yap_blob_gc_relocate_handler(YAP_Term t);
extern YAP_Int Yap_blob_tag_from_slot(YAP_Int slot);
extern void *Yap_blob_info_from_slot(YAP_Int slot);

/********* execution mode ***********************/

typedef enum {
  YAPC_INTERPRETED,     /* interpreted */
  YAPC_MIXED_MODE_USER, /* mixed mode only for user predicates */
  YAPC_MIXED_MODE_ALL,  /* mixed mode for all predicates */
  YAPC_COMPILE_USER,    /* compile all user predicates*/
  YAPC_COMPILE_ALL      /* compile all predicates */
} yapc_exec_mode;

/** Stream Modes: */
typedef enum stream_f {
  Free_Stream_f = 0x000001,   /**< Free YAP Stream */
  Input_Stream_f = 0x000002,  /**< Input Stream */
  Output_Stream_f = 0x000004, /**< Output Stream in Truncate Mode */
  Append_Stream_f = 0x000008, /**< Output Stream in Append Mod */
  Eof_Stream_f = 0x000010,    /**< Stream found an EOF */
  Null_Stream_f = 0x000020,   /**< Stream is /dev/null, or equivant */
  Tty_Stream_f = 0x000040,    /**< Stream is a terminal */
  Socket_Stream_f = 0x000080, /**< Socket Stream */
  Binary_Stream_f = 0x000100, /**< Stream is not eof */
  Eof_Error_Stream_f =
      0x000200, /**< Stream should generate error on trying to read after EOF */
  Reset_Eof_Stream_f =
      0x000400, /**< Stream should be reset on findind an EO (C-D and console.*/
  Past_Eof_Stream_f = 0x000800, /**< Read EOF from stream */
  Push_Eof_Stream_f = 0x001000, /**< keep on sending EOFs */
  Seekable_Stream_f =
      0x002000, /**< we can jump around the stream (std regular files) */
  Promptable_Stream_f = 0x004000,    /**< Interactive line-by-line stream */
  Client_Socket_Stream_f = 0x008000, /**< socket in client mode */
  Server_Socket_Stream_f = 0x010000, /**< socket in server mode */
  InMemory_Stream_f = 0x020000,      /**< buffer */
  Pipe_Stream_f = 0x040000,          /**< FIFO buffer */
  Popen_Stream_f = 0x080000,         /**< popen open, pipes mosylyn */
  User_Stream_f = 0x100000,          /**< usually user_ipiy  */
  HAS_BOM_f = 0x200000,              /**< media for streamhas a BOM mar. */
  RepError_Prolog_f =
      0x400000,              /**< handle representation error as Prolog terms */
  RepError_Xml_f = 0x800000, /**< handle representation error as XML objects */
  DoNotCloseOnAbort_Stream_f =
      0x1000000, /**< do not close the stream after an abort event */
  Readline_Stream_f = 0x2000000, /**< the stream is a readline stream */
  FreeOnClose_Stream_f =
  0x4000000, /**< the stream buffer should be releaed on close */
  CloseOnException_Stream_f =
      0x8000000 /**< the stream closed by Yap_Error and friends */
} estream_f;

typedef uint64_t stream_flags_t;

/********* YAP C-Flags ***********************/

typedef enum {
  YAPC_ENABLE_GC, /* enable or disable garbage collection */
  YAPC_ENABLE_AGC /* enable or disable atom garbage collection */
} yap_flag_gc_t;

typedef enum yap_enum_reset_t {
  YAP_EXEC_ABSMI = 0,
  YAP_FULL_RESET = 1,
  YAP_RESET_FROM_RESTORE = 3
} yap_reset_t;

typedef bool (*YAP_ModInit_t)(void);

typedef struct {
  YAP_ModInit_t f;
  const char *s;
} YAP_delaymodule_t;

#endif /* _YAPDEFS_H */
