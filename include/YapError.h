/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapError.h   						 *
* mods:									 *
* comments:	error header file for YAP				 *
* version:      $Id: Yap.h,v 1.38 2008-06-18 10:02:27 vsc Exp $	 *
*************************************************************************/

#ifndef YAP_ERROR_H
#define YAP_ERROR_H 1

/* Types of Errors */
typedef enum
{
  YAP_NO_ERROR,
  FATAL_ERROR,
  INTERNAL_ERROR,
  INTERNAL_COMPILER_ERROR,
  PURE_ABORT,
  CALL_COUNTER_UNDERFLOW,
  /* ISO_ERRORS */
  CONSISTENCY_ERROR,
  DOMAIN_ERROR_ARRAY_OVERFLOW,
  DOMAIN_ERROR_ARRAY_TYPE,
  DOMAIN_ERROR_IO_MODE,
  DOMAIN_ERROR_MUTABLE,
  DOMAIN_ERROR_NON_EMPTY_LIST,
  DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
  DOMAIN_ERROR_NOT_NL,
  DOMAIN_ERROR_NOT_ZERO,
  DOMAIN_ERROR_OUT_OF_RANGE,
  DOMAIN_ERROR_OPERATOR_PRIORITY,
  DOMAIN_ERROR_OPERATOR_SPECIFIER,
  DOMAIN_ERROR_RADIX,
  DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW,
  DOMAIN_ERROR_SOURCE_SINK,
  DOMAIN_ERROR_STREAM,
  DOMAIN_ERROR_STREAM_ENCODING,
  DOMAIN_ERROR_STREAM_OR_ALIAS,
  DOMAIN_ERROR_STREAM_POSITION,
  DOMAIN_ERROR_TIMEOUT_SPEC,
  DOMAIN_ERROR_SYNTAX_ERROR_HANDLER,
  EVALUATION_ERROR_FLOAT_OVERFLOW,
  EVALUATION_ERROR_FLOAT_UNDERFLOW,
  EVALUATION_ERROR_INT_OVERFLOW,
  EVALUATION_ERROR_UNDEFINED,
  EVALUATION_ERROR_UNDERFLOW,
  EVALUATION_ERROR_ZERO_DIVISOR,
  EXISTENCE_ERROR_ARRAY,
  EXISTENCE_ERROR_KEY,
  EXISTENCE_ERROR_SOURCE_SINK,
  EXISTENCE_ERROR_STREAM,
  EXISTENCE_ERROR_VARIABLE,
  INSTANTIATION_ERROR,
  INTERRUPT_ERROR,
  OPERATING_SYSTEM_ERROR,
  OUT_OF_HEAP_ERROR,
  OUT_OF_STACK_ERROR,
  OUT_OF_TRAIL_ERROR,
  OUT_OF_ATTVARS_ERROR,
  OUT_OF_AUXSPACE_ERROR,
  PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE,
  PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM,
  PERMISSION_ERROR_CREATE_ARRAY,
  PERMISSION_ERROR_CREATE_OPERATOR,
  PERMISSION_ERROR_INPUT_BINARY_STREAM,
  PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,
  PERMISSION_ERROR_INPUT_STREAM,
  PERMISSION_ERROR_INPUT_TEXT_STREAM,
  PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE,
  PERMISSION_ERROR_OPEN_SOURCE_SINK,
  PERMISSION_ERROR_OUTPUT_BINARY_STREAM,
  PERMISSION_ERROR_OUTPUT_STREAM,
  PERMISSION_ERROR_OUTPUT_TEXT_STREAM,
  PERMISSION_ERROR_RESIZE_ARRAY,
  PERMISSION_ERROR_REPOSITION_STREAM,
  PRED_ENTRY_COUNTER_UNDERFLOW,
  REPRESENTATION_ERROR_CHARACTER,
  REPRESENTATION_ERROR_CHARACTER_CODE,
  REPRESENTATION_ERROR_INT,
  REPRESENTATION_ERROR_MAX_ARITY,
  REPRESENTATION_ERROR_VARIABLE,
  RESOURCE_ERROR_HUGE_INT,
  RESOURCE_ERROR_MAX_STREAMS,
  RESOURCE_ERROR_MAX_THREADS,
  RESOURCE_ERROR_MEMORY,
  RESOURCE_ERROR_STACK,
  RETRY_COUNTER_UNDERFLOW,
  SAVED_STATE_ERROR,
  SYNTAX_ERROR,
  SYSTEM_ERROR,
  TYPE_ERROR_ARRAY,
  TYPE_ERROR_ATOM,
  TYPE_ERROR_ATOMIC,
  TYPE_ERROR_BIGNUM,
  TYPE_ERROR_BYTE,
  TYPE_ERROR_CALLABLE,
  TYPE_ERROR_CHAR,
  TYPE_ERROR_CHARACTER,
  TYPE_ERROR_COMPOUND,
  TYPE_ERROR_DBREF,
  TYPE_ERROR_DBTERM,
  TYPE_ERROR_EVALUABLE,
  TYPE_ERROR_FLOAT,
  TYPE_ERROR_INTEGER,
  TYPE_ERROR_KEY,
  TYPE_ERROR_LIST,
  TYPE_ERROR_NUMBER,
  TYPE_ERROR_PREDICATE_INDICATOR,
  TYPE_ERROR_PTR,
  TYPE_ERROR_REFERENCE,
  TYPE_ERROR_STRING,
  TYPE_ERROR_TEXT,
  TYPE_ERROR_UBYTE,
  TYPE_ERROR_UCHAR,
  TYPE_ERROR_VARIABLE,
  UNKNOWN_ERROR
} yap_error_number;

#define JMP_LOCAL_ERROR(v, LAB)   \
  if (H + 2*(v) > ASP-1024) { \
    LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = 2*(v)*sizeof(CELL);\
    goto LAB;				  \
  }

#define LOCAL_ERROR(v)   \
  if (HR + (v) > ASP-1024) { \
    LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = 2*(v)*sizeof(CELL);\
    return NULL; \
  }

#define LOCAL_TERM_ERROR(v)   \
  if (HR + (v) > ASP-1024) { \
    LOCAL_Error_TYPE = OUT_OF_STACK_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = 2*(v)*sizeof(CELL);\
    return 0L; \
  }

#define AUX_ERROR(t, n, s, TYPE)      \
    if (s + (n+1) > (TYPE *)AuxSp) {  \
    LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = n*sizeof(TYPE);\
    return NULL; \
    }

#define AUX_TERM_ERROR(t, n, s, TYPE)      \
    if (s + (n+1) > (TYPE *)AuxSp) {  \
    LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = n*sizeof(TYPE);\
    return 0L; \
    }

#define JMP_AUX_ERROR(n, s, t, TYPE, LAB)	\
    if (s + (n+1) > (TYPE *)AuxSp) {  \
    LOCAL_Error_TYPE = OUT_OF_AUXSPACE_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = n*sizeof(TYPE);\
    goto LAB; \
    }

#define HEAP_ERROR(a,TYPE) if( a == NIL) {	\
    LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = n*sizeof(TYPE);\
    return NULL;\
    }

#define HEAP_TERM_ERROR(a,TYPE) if( a == NIL) {	\
    LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = n*sizeof(TYPE);\
    return 0L;\
    }

#define JMP_HEAP_ERROR(a,n,t,TYPE, LAB) if( a == NIL) {	\
    LOCAL_Error_TYPE = OUT_OF_HEAP_ERROR;\
    LOCAL_Error_Term = t;\
    LOCAL_Error_Size = n*sizeof(TYPE);\
    goto LAB;\
    }



#endif