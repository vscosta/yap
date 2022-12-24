/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 2015-		 *
*									 *
**************************************************************************
*									 *
* File:		YapFlags.h						 *
* Last rev:								 *
* mods:									 *
* comments:	flag system interface.					 *
*									 *
*************************************************************************/

/**
 @file YapFlags.h

@{
@defgroup YAPFlags Prolog Flags
@ingroup Builtins

@brief Inquuiring and setting YAP state.
*/

#include "inline-only.h"
#ifndef YAP_FLAGS_H
#define YAP_FLAGS_H 1

// INLINE_ONLY  bool nat( Term inp );

#define SYSTEM_OPTION_0 "attributed_variables,rational_trees]"
#if THREADS
#define SYSTEM_OPTION_1 "threads,"
#endif
#define SYSTEM_OPTION_3 "big_numbers,"
#if DEPTH_LIMIT
#define SYSTEM_OPTION_4 "depth_limit,"
#endif
#if LOW_LEVEL_TRACE
#define SYSTEM_OPTION_5 "low_level_tracer,"
#endif
#if YAPOR
#define SYSTEM_OPTION_6 "or_parallelism,"
#endif
#if USE_READLINE
#define SYSTEM_OPTION_7 "readline,"
#endif
#if TABLING
#define SYSTEM_OPTION_8 "tabling,"
#endif

static inline Term nat(Term inp) {
  if (IsVarTerm(inp)) {
    Yap_Error(INSTANTIATION_ERROR, inp, "set_prolog_flag: value must be %s",
              "bound");
    return TermZERO;
  }
  if (IsIntTerm(inp)) {
    Int i = IntOfTerm(inp);
    if (i >= 0)
      return inp;
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, inp,
              "set_prolog_flag: value must be %s", ">= 0");
    return TermZERO;
  }
  Yap_Error(TYPE_ERROR_INTEGER, inp, "set_prolog_flag: value must be %s",
            "integer");
  return TermZERO;
}

static inline Term at2n(Term inp) {
  Yap_Error(PERMISSION_ERROR_READ_ONLY_FLAG, inp, "set_prolog_flag %s",
            "flag is read-only");
  return TermZERO;
}

static inline Term isfloat(Term inp) {
  if (IsVarTerm(inp)) {
    Yap_Error(INSTANTIATION_ERROR, inp, "set_prolog_flag: value must be %s",
              "integer");

    return TermZERO;
  }
  if (IsFloatTerm(inp)) {
    return inp;
  }
  Yap_Error(TYPE_ERROR_FLOAT, inp, "set_prolog_flag: value must be %s",
            "floating-point");
  return TermZERO;
}

extern Term ro(Term inp);
extern Term aro(Term inp);
extern Term booleanFlag( Term inp );
extern Term febooleanFlag( Term inp );
extern Term synerr(Term inp);


static inline Term filler(Term inp) { return inp; }

static inline Term list_filler(Term inp) {
  if (IsVarTerm(inp) || IsPairTerm(inp) || inp == TermNil)
    return inp;

  Yap_Error(TYPE_ERROR_LIST, inp, "set_prolog_flag in {codes,string}");

  return TermZERO;
}

// INLINE_ONLY  Term isatom( Term inp );

static inline Term isatom(Term inp) {
  CACHE_REGS
  if (IsVarTerm(inp)) {
    Yap_Error(INSTANTIATION_ERROR, inp, "set_prolog_flag %s",
              "value must be bound");
    return TermZERO;
  }
  if (IsStringTerm(inp)) {
    inp = MkStringTerm(RepAtom(AtomOfTerm(inp))->StrOfAE);
  }
  if (IsAtomTerm(inp))
    return inp;
  Yap_Error(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
  return TermZERO;
}

static inline Term isadress(Term inp) {
  if (IsVarTerm(inp)) {
    Yap_Error(INSTANTIATION_ERROR, inp, "set_prolog_flag %s",
              "value must be bound");
    return TermZERO;
  }
  if (IsAddressTerm(inp))
    return inp;
  Yap_Error(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
  return TermZERO;
}

static inline Term options(Term inp) {
  return Yap_IsGroundTerm(inp) ? inp : TermZERO;
}

static inline const char *                                                                                    rootdir(Term inp) {
  return Yap_ROOTDIR;
}

// INLINE_ONLY  Term ok( Term inp );

static inline Term ok(Term inp) { return inp; }

// a pair, obtained from x(y) -> 1,2,y)
typedef struct x_el {
  bool used;
  Term source;
  Term tvalue;
} xarg;

INLINE_ONLY void badBoolean(yap_error_number err, xarg *entry)
{
  if (IsVarTerm(entry->tvalue)) {
      Yap_ThrowError(INSTANTIATION_ERROR, entry->tvalue, "on parameter %s", NameOfFunctor(FunctorOfTerm(entry->source)));
  }
  if (!IsAtomTerm(entry->tvalue)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, entry->tvalue, "on parameter %s",  "on parameter %s", NameOfFunctor(FunctorOfTerm(entry->source)));
  }
  Yap_ThrowError(err, entry->source, NULL);
}


typedef struct struct_param {
  const char *name;
  flag_func type;
  int id;
} param_t;

typedef struct struct_param2 {
  char *name;
  flag_func type;
  int id;
  const char *scope;
} param2_t;

/// @brief prolog_flag/2 support, notice flag is initialized as text.
/// 
/// 
typedef struct {
  char *name;                 //< user visible name
  bool writable;              //< read-write or read-only
  flag_func def;              //< call on definition
  const char *init;           //< initial value as string
  flag_helper_func helper;    //< operations triggered by writing the flag.
} flag_info;

typedef struct {
  char *name;
  flag_func def;
  const char *init;
} arg_info;

/// @brief
///  a flag is represented as a Prolog term.
typedef union flagTerm {
  Term at;
  struct DB_TERM *DBT;
} flag_term;

extern void Yap_InitFlags(bool);
extern bool Yap_InitSIGSEGV(Term enable);

/**
 @pred  yap_flag( ?Param, ?Value)


Set or read system properties for  _Param_:
*/


#undef YAP_FLAG
#undef END_FLAG
#if DOXYGEN
#define  YAP_FLAG(ID,NAME,WRITABLE,DEF,INIT,HELPER)   **NAME** = INIT
#define  END_FLAG() 

#elif !defined(YAP_FLAG)
#define YAP_FLAG(ID,NAME,WRITABLE,DEF,INIT, HELPER)  ID
#define  END_FLAG() 


#endif

enum local_flag_t {
#include "YapLFlagInfo.h"
} ;
 
enum global_flag_t {
#include "YapGFlagInfo.h"
} ;
 
 #undef YAP_FLAG
#undef END_FLAG

bool Yap_set_flag(Term tflag, Term t2);
Term getYapFlag(Term tflag);

int Yap_ArgKey(Atom key, const param_t *def, int n);

static inline bool check_refs_to_ltable(void) { return true; }

static inline void setAtomicGlobalPrologFlag(int id, Term v) {
  GLOBAL_Flags[id].at = v;
}

static inline Term getAtomicGlobalPrologFlag(int id) {
  return GLOBAL_Flags[id].at;
}

static inline Term getAtomicLocalPrologFlag(int id) {
  CACHE_REGS
  return LOCAL_Flags[id].at;
}

static inline void setAtomicLocalPrologFlag(int id, Term v) {
  CACHE_REGS
  check_refs_to_ltable();
  LOCAL_Flags[id].at = v;
}

static inline void setBooleanLocalPrologFlag(int id, bool v) {
  CACHE_REGS
  check_refs_to_ltable();
  if (v) {
    LOCAL_Flags[id].at = TermTrue;
  } else {
    LOCAL_Flags[id].at = TermFalse;
  }
}

static inline void setBooleanGlobalPrologFlag(int id, bool v) {
  if (v) {
    GLOBAL_Flags[id].at = TermTrue;
  } else {
    GLOBAL_Flags[id].at = TermFalse;
  }
}

static inline bool trueGlobalPrologFlag(int id) {
  return GLOBAL_Flags[id].at == TermTrue;
}

static inline bool falseGlobalPrologFlag(int id) {
  return GLOBAL_Flags[id].at == TermFalse;
}

static inline bool trueLocalPrologFlag(int id) {
  CACHE_REGS
  return LOCAL_Flags[id].at == TermTrue;
}

static inline bool falseLocalPrologFlag(int id) {
  CACHE_REGS
  return LOCAL_Flags[id].at == TermFalse;
}

static inline bool isoLanguageFlag(void) {
    return GLOBAL_Flags[ISO_FLAG].at == TermTrue;
}

static inline bool strictISOFlag(void) {
  return GLOBAL_Flags[STRICT_ISO_FLAG].at == TermTrue;
}

static inline bool silentMode(void) {
  return GLOBAL_Flags[VERBOSE_FLAG].at == TermSilent;
}

static inline bool verboseMode(void) {
  return GLOBAL_Flags[VERBOSE_FLAG].at != TermSilent;
}

static inline bool FileErrors(USES_REGS1) {return LOCAL_Flags[FILE_ERRORS_FLAG].at    == TermError; }


static inline void setVerbosity(Term val) {
    GLOBAL_Flags[VERBOSE_FLAG].at = val;
}

static inline bool setSyntaxErrorsFlag(Term val) {
  if ((val = synerr(val)) == TermZERO)
    return false;
  CACHE_REGS
  LOCAL_Flags[SYNTAX_ERRORS_FLAG].at = val;
  return true;
}

static inline Term getSyntaxErrorsFlag(void) {
  CACHE_REGS
  return LOCAL_Flags[SYNTAX_ERRORS_FLAG].at;
}

static inline Term getBackQuotesFlag(Term mod) {
  Term val;
  unsigned int flags = Yap_GetModuleEntry(mod)->flags;
  if (flags & BCKQ_ATOM) {
    val = TermAtom;
  } else if (flags & BCKQ_STRING) {
    val = TermString;
  } else if (flags & BCKQ_CHARS) {
    val = TermChars;
  } else {
    val = TermCodes;
  }
return val;
}

static inline Term getSingleQuotesFlag(Term mod) {
    Term val;
    unsigned int flags = Yap_GetModuleEntry(mod)->flags;
    if (flags & SNGQ_ATOM) {
        val = TermAtom;
    } else if (flags & SNGQ_STRING) {
        val = TermString;
    } else if (flags & SNGQ_CHARS) {
        val = TermChars;
    } else {
        val = TermCodes;
    }
    return  val;
}

static inline Term getDoubleQuotesFlag(Term mod) {
    Term val;
    unsigned int flags = Yap_GetModuleEntry(mod)->flags;
    if (flags & DBLQ_ATOM) {
        val = TermAtom;
    } else if (flags & DBLQ_STRING) {
        val = TermString;
    } else if (flags & DBLQ_CHARS) {
        val = TermChars;
    } else {
        val = TermCodes;
    }
    return val;
}

static inline Term indexingMode(void) { return GLOBAL_Flags[INDEX_FLAG].at; }

static inline const char *floatFormat(void) {
  return RepAtom(AtomOfTerm(GLOBAL_Flags[FLOAT_FORMAT_FLAG].at))->rep.uStrOfAE;
}


static inline size_t indexingDepth(void) {
  return IntOfTerm(GLOBAL_Flags[INDEX_SUB_TERM_SEARCH_DEPTH_FLAG].at);
}

static inline Term gcTrace(void) { return GLOBAL_Flags[GC_TRACE_FLAG].at; }

Term Yap_UnknownFlag(Term mod);

bool rmdot(Term inp);

#define Yap_ArgListToVector(l, def, n, a, e)				\
  Yap_ArgListToVector__(__FILE__, __FUNCTION__, __LINE__, l, def, n, a, e)

extern  xarg *Yap_ArgListToVector__(const char *file, const char *function, int lineno,Term listl, const param_t *def, int n, xarg *a,
                          yap_error_number e);

extern xarg *Yap_ArgList2ToVector__(const char *file, const char *function, int lineno, Term listl, const param2_t *def, int n, yap_error_number e);

#define Yap_ArgList2ToVector(l, def, n, e)           \
  Yap_ArgList2ToVector__(__FILE__, __FUNCTION__, __LINE__, l, def, n, e)

#endif // YAP_FLAGS_H

/// @}
