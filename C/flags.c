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
 * File:		flags.c *
 * Last rev:								 *
 * mods: *
 * comments:	abstract machine definitions				 *
 *									 *
 *************************************************************************/

/** @file C/flags.c

    @brief  Prolog parameter browsing and setting,
*/

/*
 * @namespace prolog
 */

/**

    @defgroup YAPFlagsC C-code to handle Prolog flags.
    @ingroup YAPFlags

    @{

@brief Low-level code to support flags.

*/

// this is where  we define flags
#define INIT_FLAGS 1

#include "Yap.h"
#include "iopreds.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

static Term ro(Term inp);
static Term nat(Term inp);
static Term isatom(Term inp);
static Term booleanFlag(Term inp);
// static bool string( Term inp );
// static bool  list_atom( Term inp );
static Term list_option(Term inp);
static Term argv(Term inp);
static Term os_argv(Term inp);
static bool agc_threshold(Term inp);
static bool gc_margin(Term inp);
static Term executable(Term inp);
static Term sys_thread_id(Term inp);
static Term sys_pid(Term inp);
static bool mkprompt(Term inp);
static Term synerr(Term inp);
static Term indexer(Term inp);
static Term stream(Term inp);
static bool getenc(Term inp);
static bool typein(Term inp);
static bool dqs(Term t2);
static bool bqs(Term t2);
static bool sqf(Term t2);
static bool set_error_stream(Term inp);
static bool set_input_stream(Term inp);
static bool set_output_stream(Term inp);
static bool dollar_to_lc(Term inp);
static bool setSignals(Term inp);

static void newFlag(Term fl, Term val);
static Int current_prolog_flag(USES_REGS1);
static Int set_prolog_flag(USES_REGS1);

#include "YapEval.h"
#include "Yatom.h"
#include "yapio.h"

#define YAP_FLAG(ID, NAME, WRITABLE, DEF, INIT, HELPER)                        \
  { NAME, WRITABLE, DEF, INIT, HELPER }

#define START_LOCAL_FLAGS static flag_info local_flags_setup[] = {
#define END_LOCAL_FLAGS                                                        \
  LZERO_FLAG                                                                   \
  }                                                                            \
  ;

#define START_GLOBAL_FLAGS static flag_info global_flags_setup[] = {
#define END_GLOBAL_FLAGS                                                       \
  GZERO_FLAG                                                                   \
  }                                                                            \
  ;

#define GZERO_FLAG                                                             \
  { NULL, false, NULL, NULL, NULL }
#define LZERO_FLAG                                                             \
  { NULL, false, NULL, NULL, NULL }

#include "YapGFlagInfo.h"

#include "YapLFlagInfo.h"

static Term indexer(Term inp) {
  if (IsStringTerm(inp)) {
    inp = MkStringTerm(RepAtom(AtomOfTerm(inp))->StrOfAE);
  }
  if (inp == TermOff || inp == TermSingle || inp == TermCompact ||
      inp == TermMulti || inp == TermOn || inp == TermMax)
    return inp;

  if (IsAtomTerm(inp)) {
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, inp,
              "set_prolog_flag index in {off,single,compact,multi,on,max}");
    return TermZERO;
  }
  Yap_ThrowError(TYPE_ERROR_ATOM, inp, "set_prolog_flag index to an atom");
  return TermZERO;
}

/**
 * set or reset signal handlers. The act is only performed if the flag changed values.
 *
 * @param inp Whether to enable or disable
 * @return always true
 *
 */
static bool setSignals(Term inp) {
      bool handle = (inp == TermTrue || inp == TermOn);
      if (handle !=  GLOBAL_PrologShouldHandleInterrupts) {
          Yap_InitSignals(0);
      }
      GLOBAL_PrologShouldHandleInterrupts = handle;
      return true;
  }

static bool dqf1(ModEntry *new, Term t2 USES_REGS) {
  new->flags &= ~(DBLQ_CHARS | DBLQ_CODES | DBLQ_ATOM | DBLQ_STRING);
  if (IsStringTerm(t2)) {
    t2 = MkStringTerm(RepAtom(AtomOfTerm(t2))->StrOfAE);
  }
  if (IsAtomTerm(t2)) {
    if (t2 == TermString) {
      new->flags |= DBLQ_STRING;
      return true;
    } else if (t2 == TermAtom) {
      new->flags |= DBLQ_ATOM;
      return true;
    } else if (t2 == TermCodes) {
      new->flags |= DBLQ_CODES;
      return true;
    } else if (t2 == TermChars) {
      new->flags |= DBLQ_CHARS;
      return true;
    }
    /* bad argument, but still an atom */
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, t2,
              "bad option %s for backquoted "
              "string flag, use one string, "
              "atom, codes or chars",
              RepAtom(AtomOfTerm(t2))->StrOfAE);
    return false;
  } else {
    Yap_ThrowError(TYPE_ERROR_ATOM, t2,
              "set_prolog_flag(double_quotes, %s), should "
              "be {string,atom,codes,chars}",
              RepAtom(AtomOfTerm(t2))->StrOfAE);
    return false;
  }
}

static bool dqs(Term t2) {
  CACHE_REGS
  ModEntry *new = Yap_GetModuleEntry(CurrentModule);
  return dqf1(new, t2 PASS_REGS);
}

static bool bqf1(ModEntry *new, Term t2 USES_REGS) {
  new->flags &= ~(BCKQ_CHARS | BCKQ_CODES | BCKQ_ATOM | BCKQ_STRING);
  if (IsStringTerm(t2)) {
    t2 = MkStringTerm(RepAtom(AtomOfTerm(t2))->StrOfAE);
  }
  if (IsAtomTerm(t2)) {
    if (t2 == TermString) {
      new->flags |= BCKQ_STRING;
      return true;
    } else if (t2 == TermAtom) {
      new->flags |= BCKQ_ATOM;
      return true;
    } else if (t2 == TermCodes) {
      new->flags |= BCKQ_CODES;
      return true;
    } else if (t2 == TermChars) {
      new->flags |= BCKQ_CHARS;
      return true;
    }
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, t2,
              "bad option %s for backquoted "
              "string flag, use one string, "
              "atom, codes or chars",
              RepAtom(AtomOfTerm(t2))->StrOfAE);
    return false;
  } else {
    Yap_ThrowError(TYPE_ERROR_ATOM, t2, "flag  %s is not module-scoped",
              RepAtom(AtomOfTerm(t2))->StrOfAE);
    return false;
  }
}

static bool bqs(Term t2) {
  CACHE_REGS
  ModEntry *new = Yap_GetModuleEntry(CurrentModule);
  return bqf1(new, t2 PASS_REGS);
}

static bool sqf1(ModEntry *new, Term t2 USES_REGS) {
  new->flags &= ~(SNGQ_CHARS | SNGQ_CODES | SNGQ_ATOM | SNGQ_STRING);
  if (IsStringTerm(t2)) {
    t2 = MkStringTerm(RepAtom(AtomOfTerm(t2))->StrOfAE);
  }
  if (IsAtomTerm(t2)) {
    if (t2 == TermString) {
      new->flags |= SNGQ_STRING;
      return true;
    } else if (t2 == TermAtom) {
      new->flags |= SNGQ_ATOM;
      return true;
    } else if (t2 == TermCodes) {
      new->flags |= SNGQ_CODES;
      return true;
    } else if (t2 == TermChars) {
      new->flags |= SNGQ_CHARS;
      return true;
    }
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, t2,
              "bad option %s for backquoted "
              "string flag, use one string, "
              "atom, codes or chars",
              RepAtom(AtomOfTerm(t2))->StrOfAE);
    return false;
  } else {
    Yap_ThrowError(TYPE_ERROR_ATOM, t2, "flag  %s is not module-scoped",
              RepAtom(AtomOfTerm(t2))->StrOfAE);
    return false;
  }
}

static bool sqf(Term t2) {
  CACHE_REGS
  ModEntry *new = Yap_GetModuleEntry(CurrentModule);
  return sqf1(new, t2 PASS_REGS);
}

static bool dollar_to_lc(Term inp) {
  if (inp == TermTrue || inp == TermOn) {
    Yap_chtype0['$'+1] = LC;
    return true;
  }
  if (inp == TermFalse || inp == TermOff) {
    Yap_chtype0['$'+1] = CC;
    return false;
  }
    Yap_ThrowError(TYPE_ERROR_BOOLEAN, inp,
              "dollar_to_lower_case is a boolean flag");
    return TermZERO;
  }

static Term isaccess(Term inp) {
  if (inp == TermReadWrite || inp == TermReadOnly)
    return inp;

  if (IsStringTerm(inp)) {
    inp = MkStringTerm(RepAtom(AtomOfTerm(inp))->StrOfAE);
  }
  if (IsAtomTerm(inp)) {
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, inp,
              "set_prolog_flag access in {read_write,read_only}");
    return TermZERO;
  }
  Yap_ThrowError(TYPE_ERROR_ATOM, inp,
            "set_prolog_flag access in {read_write,read_only}");
  return TermZERO;
}

static Term stream(Term inp) {
  if (IsVarTerm(inp))
    return inp;
  if (Yap_CheckStream(inp,
                      Input_Stream_f | Output_Stream_f | Append_Stream_f |
                          Socket_Stream_f,
                      "yap_flag/3") >= 0)
    return inp;
  return 0;
}

static bool set_error_stream(Term inp) {
  if (IsVarTerm(inp))
    return Yap_unify(inp, Yap_StreamUserName(LOCAL_c_error_stream));
  return Yap_SetErrorStream(inp);
}

static bool set_input_stream(Term inp) {
  if (IsVarTerm(inp))
    return Yap_unify(inp, Yap_StreamUserName(LOCAL_c_input_stream));
  return Yap_SetInputStream(inp);
}

static bool set_output_stream(Term inp) {
  if (IsVarTerm(inp))
    return Yap_unify(inp, Yap_StreamUserName(LOCAL_c_output_stream));
  return Yap_SetOutputStream(inp);
}

static Term isground(Term inp) {
  return Yap_IsGroundTerm(inp) ? inp : TermZERO;
}

static Term flagscope(Term inp) {
  if (inp == TermGlobal || inp == TermThread || inp == TermModule)
    return inp;

  if (IsStringTerm(inp)) {
    inp = MkStringTerm(RepAtom(AtomOfTerm(inp))->StrOfAE);
  }
  if (IsAtomTerm(inp)) {
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, inp,
              "set_prolog_flag access in {global,module,thread}");
    return TermZERO;
  }
  Yap_ThrowError(TYPE_ERROR_ATOM, inp,
            "set_prolog_flag access in {global,module,thread}");
  return TermZERO;
}

static bool mkprompt(Term inp) {
  CACHE_REGS
  if (IsVarTerm(inp)) {
    return Yap_unify(inp, MkAtomTerm(Yap_LookupAtom(LOCAL_Prompt)));
  }
  if (IsStringTerm(inp)) {
    inp = MkStringTerm(RepAtom(AtomOfTerm(inp))->StrOfAE);
  }
 if (!IsAtomTerm(inp)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
    return false;
  }
  strncpy(LOCAL_Prompt, (const char *)RepAtom(AtomOfTerm(inp))->StrOfAE,
          MAX_PROMPT);
  return true;
}

static bool getenc(Term inp) {
  CACHE_REGS
  if (IsStringTerm(inp)) {
    inp = MkStringTerm(RepAtom(AtomOfTerm(inp))->StrOfAE);
  }
  if (!IsVarTerm(inp) && !IsAtomTerm(inp)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, inp, "get_encoding");
    return false;
  }
  return Yap_unify(inp, MkAtomTerm(Yap_LookupAtom(enc_name(LOCAL_encoding))));
}

/*
static bool enablerl( Term inp ) {
CACHE_REGS
if (IsVarTerm(inp)) {
return Yap_unify( inp, MkAtomTerm( Yap_LookupAtom( enc_name(LOCAL_encoding)
)) );
}
if (!IsAtomTerm(inp) ) {
Yap_ThrowError(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
return false;
}
enc_id( RepAtom( AtomOfTerm( inp ) )->StrOfAE, ENC_OCTET );
return true;
}
*/

static bool typein(Term inp) {
  CACHE_REGS
  if (IsVarTerm(inp)) {
    Term tin = CurrentModule;
    if (tin == PROLOG_MODULE)
      tin = TermProlog;
    return Yap_unify(inp, tin);
  }
  if (IsStringTerm(inp)) {
    inp = MkAtomTerm(Yap_LookupAtom(StringOfTerm(inp)));
  }
  if (!IsAtomTerm(inp)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, inp, "set_prolog_flag");
    return false;
  }
  CurrentModule = inp;
  if (inp == TermProlog)
    CurrentModule = PROLOG_MODULE;
  return true;
}

#if 0

                    static Int p_has_yap_or(USES_REGS1) {
#ifdef YAPOR
                        return (TRUE);
#else
                        return (FALSE);
#endif
                    }

                    static Int p_has_eam(USES_REGS1) {

#ifdef BEAM
                        return (TRUE);
#else
                        return (FALSE);
#endif
                    }

                    static Int p_has_jit(USES_REGS1) {
#ifdef HAS_JIT
                        return (TRUE);
#else
                        return (FALSE);
#endif
                    }

                    static bool tabling( Term inp ) {
                        if (value == 0) { /* default */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                TabEnt_mode(tab_ent) = TabEnt_flags(tab_ent);
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            yap_flags[TA BLING_MODE_FLAG] = 0;
                        } else if (value == 1) { /* batched */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                SetMode_Batched(TabEnt_mode(tab_ent));
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            SetMode_Batched(yap_flags[TABLING_MODE_FLAG]);
                        } else if (value == 2) { /* local */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                SetMode_Local(TabEnt_mode(tab_ent));
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            SetMode_Local(yap_flags[TABLING_MODE_FLAG]);
                        } else if (value == 3) { /* exec_answers */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                SetMode_ExecAnswers(TabEnt_mode(tab_ent));
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            SetMode_ExecAnswers(yap_flags[TABLING_MODE_FLAG]);
                        } else if (value == 4) { /* load_answers */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                SetMode_LoadAnswers(TabEnt_mode(tab_ent));
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            SetMode_LoadAnswers(yap_flags[TABLING_MODE_FLAG]);
                        } else if (value == 5) { /* local_trie */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                SetMode_LocalTrie(TabEnt_mode(tab_ent));
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            SetMode_LocalTrie(yap_flags[TABLING_MODE_FLAG]);
                        } else if (value == 6) { /* global_trie */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                SetMode_GlobalTrie(TabEnt_mode(tab_ent));
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            SetMode_GlobalTrie(yap_flags[TABLING_MODE_FLAG]);
                        } else if (value == 7) { /* CoInductive */
                            tab_ent_ptr tab_ent = GLOBAL_root_tab_ent;
                            while (tab_ent) {
                                SetMode_CoInductive(TabEnt_mode(tab_ent));
                                tab_ent = TabEnt_next(tab_ent);
                            }
                            SetMode_CoInductive(yap_flags[TABLING_MODE_FLAG]);
                        }
                    }

                    static bool string( Term inp ) {
                        if (IsVarTerm(inp)) {
                            Yap_ThrowError(INSTANTIATION_ERROR, inp, "set_prolog_flag in \"...\"");
                            return false;
                        }
                        if (IsStringTerm( inp ))
                        return true;
                        Term inp0  = inp;
                        if (IsPairTerm(inp)) {
                            Term hd = HeadOfTerm(inp);
                            if (IsAtomTerm(hd)) {
                                do {
                                    Term hd = HeadOfTerm(inp);
  if (IsStringTerm(hd)) {
    hd = MkStringTerm(RepAtom(AtomOfTerm(hd))->StrOfAE);
  }
                                    if (!IsAtomTerm(hd)) {
                                        Yap_ThrowError(TYPE_ERROR_TEXT, inp0, "set_prolog_flag in \"...\"");
                                        return false;
                                    }
                                } while (IsPairTerm( inp ) );
                            } else if (IsIntTerm(hd)) {
                                do {
                                    Term hd = HeadOfTerm(inp);
                                    if (!IsIntTerm(hd)) {
                                        Yap_ThrowError(TYPE_ERROR_TEXT, inp0, "set_prolog_flag in \"...\"");
                                        return false;
                                    }
                                    if (IntOfTerm(hd) < 0) {
                                        Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, inp0, "set_prolog_flag in 0...");
                                        return false;
                                    }
                                } while (IsPairTerm( inp ) );
                            } else {
                                Yap_ThrowError(TYPE_ERROR_TEXT, inp0, "set_prolog_flag in \"...\"");
                                return false;
                            }
                        }
                        if ( inp != TermNil ) {
                            Yap_ThrowError(TYPE_ERROR_TEXT, inp0, "set_prolog_flag in \"...\"");
                            return false;
                        }
                        return true;
                    }

x                    static bool list_atom( Term inp ) {
                        if (IsVarTerm(inp)) {
                            Yap_ThrowError(INSTANTIATION_ERROR, inp, "set_prolog_flag in \"...\"");
                            return false;
                        }
                        Term inp0  = inp;
                        if (IsPairTerm(inp)) {
                            Term hd = HeadOfTerm(inp);
                            do {
			        if (IsStringTerm(hd)) {
				  hd = MkStringTerm(RepAtom(AtomOfTerm(hd))->StrOfAE);
  }

                                if (!IsAtomTerm(hd)) {
                                    Yap_ThrowError(TYPE_ERROR_ATOM, inp0, "set_prolog_flag in \"...\"");
                                    return false;
                                }
                            } while (IsPairTerm( inp ) );
                        }
                        if ( inp != TermNil ) {
                            Yap_ThrowError(TYPE_ERROR_LIST, inp0, "set_prolog_flag in [...]");
                            return false;
                        }
                        return true;
                    }
#endif

static Term list_option(Term inp) {
  if (IsVarTerm(inp)) {
    Yap_ThrowError(INSTANTIATION_ERROR, inp, "set_prolog_flag in \"...\"");
    return inp;
  }
  Term inp0 = inp;
  if (IsPairTerm(inp)) {
    do {
      Term hd = HeadOfTerm(inp);
      inp = TailOfTerm(inp);
  if (IsStringTerm(hd)) {
    hd = MkStringTerm(RepAtom(AtomOfTerm(hd))->StrOfAE);
  }
      if (IsAtomTerm(hd)) {
        continue;
      }
      if (IsApplTerm(hd)) {
        Functor f = FunctorOfTerm(hd);
        if (!IsExtensionFunctor(f) && ArityOfFunctor(f) == 1 &&
            Yap_IsGroundTerm(hd)) {
          continue;
        }
        if (!Yap_IsGroundTerm(hd))
          Yap_ThrowError(INSTANTIATION_ERROR, hd, "set_prolog_flag in \"...\"");
        return TermZERO;
      }
    } while (IsPairTerm(inp));
    if (inp == TermNil) {
      return inp0;
    }
    Yap_ThrowError(TYPE_ERROR_LIST, inp0, "set_prolog_flag in [...]");
    return TermZERO;
  } else /* lone option */ {
  if (IsStringTerm(inp)) {
    inp = MkStringTerm(RepAtom(AtomOfTerm(inp))->StrOfAE);
  }
    if (IsAtomTerm(inp)) {
      return inp;
    } else if (IsApplTerm(inp)) {
      Functor f = FunctorOfTerm(inp);
      if (!IsExtensionFunctor(f) && ArityOfFunctor(f) == 1 &&
          Yap_IsGroundTerm(ArgOfTerm(1, inp))) {
        return inp;
      }
    }
  }
  return TermZERO;
}

static bool agc_threshold(Term t) {
  t = Deref(t);
  if (IsVarTerm(t)) {
    CACHE_REGS
    return Yap_unify(t, MkIntegerTerm(GLOBAL_AGcThreshold));
  } else if (!IsIntegerTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t, "prolog_flag/2 agc_margin");
    return FALSE;
  } else {
    Int i = IntegerOfTerm(t);
    if (i < 0) {
      Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "prolog_flag/2 agc_margin");
      return FALSE;
    } else {
      GLOBAL_AGcThreshold = i;
      return TRUE;
    }
  }
}

static bool gc_margin(Term t) {
  t = Deref(t);
  if (IsVarTerm(t)) {
    return Yap_unify(t, Yap_GetValue(AtomGcMargin));
  } else if (!IsIntegerTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t, "prolog_flag/2 agc_margin");
    return FALSE;
  } else {
    Int i = IntegerOfTerm(t);
    if (i < 0) {
      Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "prolog_flag/2 gc_margin");
      return FALSE;
    } else {
      CACHE_REGS
      Yap_PutValue(AtomGcMargin, MkIntegerTerm(i));
      return true;
    }
  }
}

static Term mk_argc_list(USES_REGS1) {
  int i = 1;
  Term t = TermNil;
  while (i < GLOBAL_argc) {
    char *arg = GLOBAL_argv[i];
    /* check for -L -- */
    if (arg[0] == '-' && arg[1] == 'L') {
      arg += 2;
      while (*arg != '\0' && (*arg == ' ' || *arg == '\t'))
        arg++;
      if (*arg == '-' && arg[1] == '-' && arg[2] == '\0') {
        /* we found the separator */
        int j;
        for (j = GLOBAL_argc - 1; j > i + 1; --j) {
          t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(GLOBAL_argv[j])), t);
        }
        return t;
      } else if (GLOBAL_argv[i + 1] && GLOBAL_argv[i + 1][0] == '-' &&
                 GLOBAL_argv[i + 1][1] == '-' &&
                 GLOBAL_argv[i + 1][2] == '\0') {
        /* we found the separator */
        int j;
        for (j = GLOBAL_argc - 1; j > i + 2; --j) {
          t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(GLOBAL_argv[j])), t);
        }
        return t;
      }
    }
    if (arg[0] == '-' && arg[1] == '-' && arg[2] == '\0') {
      /* we found the separator */
      int j;
      for (j = GLOBAL_argc - 1; j > i; --j) {
        t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(GLOBAL_argv[j])), t);
      }
      return (t);
    }
    i++;
  }
  return (t);
}

static Term mk_os_argc_list(USES_REGS1) {
  int i = 0;
  Term t = TermNil;
  for (i = GLOBAL_argc; i >0; ) {
    i--;
    const char *arg = GLOBAL_argv[i];
    t = MkPairTerm(MkAtomTerm(Yap_LookupAtom(arg)), t);
  }
  return (t);
}

static Term argv(Term inp) {
  CACHE_REGS
  return mk_argc_list(PASS_REGS1);
}

static Term os_argv(Term inp) {
  CACHE_REGS
  return mk_os_argc_list(PASS_REGS1);
}

static FlagEntry *
GetFlagProp(Atom a) { /* look property list of atom a for kind  */
  AtomEntry *ae = RepAtom(a);
  FlagEntry *pp;

  READ_LOCK(ae->ARWLock);

  pp = RepFlagProp(ae->PropsOfAE);
  while (!EndOfPAEntr(pp) && pp->KindOfPE != FlagProperty)
    pp = RepFlagProp(pp->NextOfPE);
  READ_UNLOCK(ae->ARWLock);

  return pp;
}

/**
 * @}
 * @defgroup YAPFlagsPs  Predicates to access Prolog flags.
 * @ingroup YAPFlags
 *
 * @{
 *
 *  @brief the following builins provide read-write access the Prolog flags. We advise you to use
 *  the ISO buitins on ISO flags.
 */



static void initFlag(flag_info *f, int fnum, bool global) {

  Atom name = Yap_LookupAtom(f->name);
  AtomEntry *ae = RepAtom(name);
  WRITE_LOCK(ae->ARWLock);
  FlagEntry *fprop = RepFlagProp(Yap_GetAPropHavingLock(name, FlagProperty));
  if (fprop == NULL) {
    fprop = (FlagEntry *)Yap_AllocAtomSpace(sizeof(FlagEntry));
    if (fprop == NULL) {
      WRITE_UNLOCK(ae->ARWLock);
      Yap_ThrowError(RESOURCE_ERROR_HEAP, TermNil,
                "not enough space for new Flag %s", ae->StrOfAE);
      return;
    }
    fprop->KindOfPE = FlagProperty;
    fprop->FlagOfVE = fnum;
    fprop->rw = f->writable;
    fprop->global = global;
    fprop->type = f->def;
    fprop->helper = f->helper;
    AddPropToAtom(ae, AbsFlagProp(fprop));
  }
  WRITE_UNLOCK(ae->ARWLock);
}

static Term executable(Term inp) {

  return MkAtomTerm(Yap_LookupAtom(Yap_FindExecutable()));
}

static Term sys_thread_id(Term inp) {
  CACHE_REGS
  int pid;
#ifdef HAVE_GETTID_SYSCALL
  pid = syscall(__NR_gettid);
#elif defined(HAVE_GETTID_MACRO)
  pid = gettid();
#elif defined(__WINDOWS__)
  pid = GetCurrentThreadId();
#else
  pid = 0;
#endif

  return MkIntegerTerm(pid);
}

static Term sys_pid(Term inp) {
  CACHE_REGS
  int pid;
#if defined(__MINGW32__) || _MSC_VER
  pid = _getpid();
#else
  pid = getpid();
#endif

  return MkIntegerTerm(pid);
}

static bool setYapFlagInModule(Term tflag, Term t2, Term mod) {
  CACHE_REGS
  FlagEntry *fv;
  ModEntry *me = Yap_GetModuleEntry(mod);
  if (!me)
    return false;
  fv = GetFlagProp(AtomOfTerm(tflag));
  if (!fv && !fv->global) {
    Yap_ThrowError(DOMAIN_ERROR_PROLOG_FLAG, tflag,
              "trying to set unknown module flag");
    return false;
  }

  if (mod == USER_MODULE) {
    flag_term *tarr = GLOBAL_Flags;
    if (!(fv->type(t2)))
      return false;

    if (fv->helper && !(fv->helper(t2)))
      return false;
    Term tout = tarr[fv->FlagOfVE].at;
    if (IsVarTerm(tout)) {
      Term t;
      while ((t = Yap_PopTermFromDB(tarr[fv->FlagOfVE].DBT)) == 0) {
	if (!Yap_gcl(0, 2, ENV, gc_P(P, CP))) {
          Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return false;
        }
      }
    } else if (IsAtomOrIntTerm(t2))
      tarr[fv->FlagOfVE].at = t2;
    else {
      tarr[fv->FlagOfVE].DBT = Yap_StoreTermInDB(t2, 2);
    }
  }
  // module specific stuff now

  if (fv->FlagOfVE == UNKNOWN_FLAG) {
    me->flags &= ~(UNKNOWN_MASK);
    if (t2 == TermError) {
      me->flags |= (UNKNOWN_ERROR);
      return true;
    } else if (t2 == TermFail) {
      me->flags |= (UNKNOWN_FAIL);
      return true;
    } else if (t2 == TermWarning) {
      me->flags |= (UNKNOWN_WARNING);
      return true;
    } else if (t2 == TermFastFail) {
      me->flags |= (UNKNOWN_FAST_FAIL);
      return true;
    }
    Yap_ThrowError(
        DOMAIN_ERROR_OUT_OF_RANGE, t2,
        "bad option  %s  for unknown flag, use one of error, fail or warning.",
        RepAtom(AtomOfTerm(tflag))->StrOfAE);
    return false;
  } else if (fv->FlagOfVE == DOUBLE_QUOTES_FLAG) {
    return dqf1(me, t2 PASS_REGS);
  } else if (fv->FlagOfVE == CHARACTER_ESCAPES_FLAG) {
    if (t2 == TermTrue) {
      me->flags |= M_CHARESCAPE;
      return true;
    } else if (t2 == TermFalse) {
      me->flags &= ~(M_CHARESCAPE);
      return true;
    }
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, t2,
              "bad option %s for character_escapes flag, use true or false",
              RepAtom(AtomOfTerm(tflag))->StrOfAE);
    return false;
  } else if (fv->FlagOfVE == BACK_QUOTES_FLAG) {
    return bqf1(me, t2 PASS_REGS);
  } else if (fv->FlagOfVE == SINGLE_QUOTES_FLAG) {
    return sqf1(me, t2 PASS_REGS);
  }
  // bad key?
  return false;
}

static Term getYapFlagInModule(Term tflag, Term mod) {
  FlagEntry *fv;
  ModEntry *me = Yap_GetModuleEntry(mod);
  if (!mod)
    return false;
  fv = GetFlagProp(AtomOfTerm(tflag));
  if (!fv && !fv->global) {
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, tflag, "trying to set unknown flag");
    return 0L;
  }
  // module specific stuff now

  if (fv->FlagOfVE == UNKNOWN_FLAG) {
    if (me->flags & UNKNOWN_ERROR)
      return TermError;
    if (me->flags & UNKNOWN_WARNING)
      return TermWarning;
    return TermFail;
  } else if (fv->FlagOfVE == CHARACTER_ESCAPES_FLAG) {
    if (me->flags & M_CHARESCAPE)
      return TermTrue;
  } else if (fv->FlagOfVE == BACK_QUOTES_FLAG) {
    if (me->flags & BCKQ_CHARS)
      return TermChars;
    if (me->flags & BCKQ_CODES)
      return TermCodes;
    if (me->flags & BCKQ_ATOM)
      return TermAtom;
    return TermString;
  } else if (fv->FlagOfVE == SINGLE_QUOTES_FLAG) {
    if (me->flags & SNGQ_CHARS)
      return TermChars;
    if (me->flags & SNGQ_CODES)
      return TermCodes;
    if (me->flags & SNGQ_ATOM)
      return TermAtom;
    return TermString;
  } else if (fv->FlagOfVE == DOUBLE_QUOTES_FLAG) {
    if (me->flags & DBLQ_CHARS)
      return TermChars;
    if (me->flags & DBLQ_CODES)
      return TermCodes;
    if (me->flags & DBLQ_ATOM)
      return TermAtom;
    return TermString;
  }
  Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, tflag, "flag  %s is not module-scoped",
            RepAtom(AtomOfTerm(tflag))->StrOfAE);
  return 0L;
}

static Int cont_yap_flag(USES_REGS1) {
  int i = IntOfTerm(EXTRA_CBACK_ARG(2, 1));
  int gmax = GLOBAL_flagCount;
  int lmax = LOCAL_flagCount;
  Term tflag = Deref(ARG1);
  EXTRA_CBACK_ARG(2, 1) = MkIntTerm(i + 1);

  if (IsApplTerm(tflag) && FunctorOfTerm(tflag) == FunctorModule) {
    Term modt = CurrentModule;
    tflag = Yap_StripModule(tflag, &modt);
    while (i != gmax && i != UNKNOWN_FLAG && i != CHARACTER_ESCAPES_FLAG &&
           i != BACK_QUOTES_FLAG && i != SINGLE_QUOTES_FLAG &&
           i != DOUBLE_QUOTES_FLAG)
      i++;
    if (i == gmax)
      cut_fail();
    EXTRA_CBACK_ARG(2, 1) = MkIntTerm(i + 1);
    {
      Term lab = MkAtomTerm(Yap_LookupAtom(global_flags_setup[i].name));
      Term val = Deref(ARG2);

      if (!Yap_unify(tflag, lab))
        return false;
      if (IsVarTerm(val)) {
        Term oval = getYapFlagInModule(lab, modt);
        if (oval == 0)
          return false;
        return Yap_unify(oval, val);
      } else {
        return setYapFlagInModule(tflag, val, modt);
      }
    }
    return false;
  }
  if (i >= gmax) {
    Yap_unify(ARG1,
              MkAtomTerm(Yap_LookupAtom(local_flags_setup[i - gmax].name)));
    if (i == gmax + lmax - 1)
      do_cut(0);
  } else {
    Yap_unify(ARG1, MkAtomTerm(Yap_LookupAtom(global_flags_setup[i].name)));
  }
  Term flag = getYapFlag(Deref(ARG1));
  return Yap_unify(flag, ARG2);
}

static Int yap_flag(USES_REGS1) {
  Term tflag = Deref(ARG1);
  if (IsVarTerm(tflag)) {
    EXTRA_CBACK_ARG(2, 1) = MkIntTerm(0);
    return cont_yap_flag(PASS_REGS1);
  }
  if (IsApplTerm(tflag) && FunctorOfTerm(tflag) == FunctorModule) {
    Term modt;
    tflag = Yap_StripModule(tflag, &modt);
    if (IsVarTerm(tflag)) {
      EXTRA_CBACK_ARG(2, 1) = MkIntTerm(0);
      return cont_yap_flag(PASS_REGS1);
    }
    do_cut(0);

    if (!isatom(tflag))
      return false;
    if (!isatom(modt))
      return false;
    if (IsVarTerm(Deref(ARG2))) {
      Term flag = getYapFlagInModule(tflag, modt);
      if (flag == 0)
        return false;
      return Yap_unify(flag, ARG2);
    } else {
      return setYapFlagInModule(tflag, Deref(ARG2), modt);
    }
  }

  do_cut(0);

  if (IsVarTerm(Deref(ARG2))) {
    Term flag = getYapFlag(Deref(ARG1));
    if (flag == 0)
      return false;
    return Yap_unify(flag, ARG2);
  }
  return set_prolog_flag(PASS_REGS1);
}

static Int cont_prolog_flag(USES_REGS1) {
  int i = IntOfTerm(EXTRA_CBACK_ARG(3, 1));
  while (i < GLOBAL_flagCount + LOCAL_flagCount) {
    int gmax = GLOBAL_flagCount;
    int lmax = LOCAL_flagCount;
    Term flag, f;

    if (i >= gmax + lmax) {
      cut_fail();
    } else if (i >= gmax) {
      Yap_unify(ARG1, (f = MkAtomTerm(
                           Yap_LookupAtom(local_flags_setup[i - gmax].name))));
    } else {
      Yap_unify(ARG1,
                (f = MkAtomTerm(Yap_LookupAtom(global_flags_setup[i].name))));
    }
    EXTRA_CBACK_ARG(3, 1) = MkIntTerm(++i);
    flag = getYapFlag(f);
    if (!Yap_unify(flag, ARG2))
      return false;
    return Yap_set_flag(f, Deref(ARG3));
  }
  cut_fail();
}

/** @pred prolog_flag(? _Flag_,- _OldValue_,+ _NewValue_)

Obtain the value for a YAP Prolog flag and then set it to a new
value. Equivalent to first calling current_prolog_flag/2 with the
second argument  _OldValue_ unbound and then calling
set_prolog_flag/2 with the third argument  _NewValue_.


*/
static Int prolog_flag(USES_REGS1) {
  if (IsVarTerm(Deref(ARG1))) {
    EXTRA_CBACK_ARG(3, 1) = MkIntTerm(0);
    return cont_prolog_flag(PASS_REGS1);
  }
  do_cut(0);
  if (IsVarTerm(Deref(ARG3))) {
    Term flag = getYapFlag(Deref(ARG1));
    if (flag == 0)
      return false;
    return Yap_unify(flag, ARG2);
  }
  return Yap_set_flag(Deref(ARG1), Deref(ARG3));
}

static Int cont_current_prolog_flag(USES_REGS1) {
  int i = IntOfTerm(EXTRA_CBACK_ARG(2, 1));
  while (i < GLOBAL_flagCount + LOCAL_flagCount) {
    int gmax = GLOBAL_flagCount;
    int lmax = LOCAL_flagCount;
    Term flag, f;

    if (i >= gmax + lmax) {
      cut_fail();
    } else if (i >= gmax) {
      Yap_unify(ARG1, (f = MkAtomTerm(
                           Yap_LookupAtom(local_flags_setup[i - gmax].name))));
    } else {
      Yap_unify(ARG1,
                (f = MkAtomTerm(Yap_LookupAtom(global_flags_setup[i].name))));
    }
    EXTRA_CBACK_ARG(2, 1) = MkIntTerm(++i);
    flag = getYapFlag(f);
    return Yap_unify(flag, ARG2);
  }
  cut_fail();
}

/** @pred current_prolog_flag(? _Flag_,- _Value_, +_New_) is iso

Obtain the value for a YAP Prolog flag, and then set the flag to the _New_ value.

*/
static Int current_prolog_flag(USES_REGS1) {
  if (IsVarTerm(Deref(ARG1))) {
    EXTRA_CBACK_ARG(3, 1) = MkIntTerm(0);
    return cont_current_prolog_flag(PASS_REGS1);
  }
  do_cut(0);
  Term flag = getYapFlag(Deref(ARG1));
  if (flag == 0)
    return false;
  return Yap_unify(flag, ARG2);
}

/** @pred current_prolog_flag(? _Flag_,- _Value_) is iso

Obtain the value for a YAP Prolog flag. Equivalent to calling
yap_flag/2 with the second argument unbound, and unifying the
returned second argument with  _Value_.

*/
static Int current_prolog_flag2(USES_REGS1) {
  Term tflag = Deref(ARG1);
  Term tout = 0;
  FlagEntry *fv;
  flag_term *tarr;

  if (IsVarTerm(tflag)) {
    EXTRA_CBACK_ARG(2, 1) = MkIntTerm(0);
    return cont_yap_flag(PASS_REGS1);
  }
  do_cut(0);
  if (IsStringTerm(tflag)) {
    tflag = MkStringTerm(RepAtom(AtomOfTerm(tflag))->StrOfAE);
  }
  if (!IsAtomTerm(tflag)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, tflag, "current_prolog_flag/3");
    return (FALSE);
  }
  fv = GetFlagProp(AtomOfTerm(tflag));
  if (!fv) {
    // should itself depend on a flag
    return FALSE;
  }
  if (fv->global)
    tarr = GLOBAL_Flags;
  else
    tarr = LOCAL_Flags;
  tout = tarr[fv->FlagOfVE].at;
  if (tout == TermZERO) {
    //    Yap_DebugPlWriteln(tflag);
    return false;
  }
  if (!IsAtomicTerm(tout)) {
      while ((tout = Yap_FetchTermFromDB(tarr[fv->FlagOfVE].DBT)) == 0) {
          /* oops, we are in trouble, not enough stack space */
          if (LOCAL_Error_TYPE == RESOURCE_ERROR_ATTRIBUTED_VARIABLES) {
              LOCAL_Error_TYPE = YAP_NO_ERROR;
              if (!Yap_growglobal(NULL)) {
                  Yap_Error(RESOURCE_ERROR_ATTRIBUTED_VARIABLES, TermNil,
                            LOCAL_ErrorMessage);
                  UNLOCK(ap->PELock);
                  return false;
              }
          } else {
              LOCAL_Error_TYPE = YAP_NO_ERROR;
              if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, gc_P(P, CP))) {
                  Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
                  UNLOCK(ap->PELock);
                  return false;
              }
          }
      }
  }
  return (Yap_unify(ARG2, tout));
}

void Yap_setModuleFlags(ModEntry *new, ModEntry *cme) {
  CACHE_REGS

  Atom at = new->AtomOfME;
  if (at == AtomProlog || CurrentModule == PROLOG_MODULE) {
    new->flags = M_SYSTEM | UNKNOWN_ERROR | M_CHARESCAPE | DBLQ_CODES |
                 BCKQ_STRING | SNGQ_ATOM;
    if (at == AtomUser)
      new->flags =
          UNKNOWN_ERROR | M_CHARESCAPE | DBLQ_CODES | BCKQ_STRING | SNGQ_ATOM;
  } else if (cme && cme->flags && cme != new) {
    new->flags = cme->flags;
  } else {
    new->flags =
        (UNKNOWN_ERROR | M_CHARESCAPE | DBLQ_CODES | BCKQ_STRING | SNGQ_ATOM);
  }
  // printf("cme=%s new=%s flags=%x\n",cme,at->StrOfAE,new->flags);
}

bool Yap_set_flag(Term tflag, Term t2) {
  FlagEntry *fv;
  flag_term *tarr;
  if (IsVarTerm(tflag)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tflag, "yap_flag/2");
    return (FALSE);
  }
    if (IsStringTerm(tflag)) {
    tflag = MkStringTerm(RepAtom(AtomOfTerm(tflag))->StrOfAE);
  }

  if (IsApplTerm(tflag) && FunctorOfTerm(tflag) == FunctorModule) {
    Term modt;
    tflag = Yap_StripModule(tflag, &modt);
    if (!isatom(tflag))
      return false;
    if (!isatom(modt))
      return false;
    return setYapFlagInModule(tflag, t2, modt);
  }
  if (!IsAtomTerm(tflag)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, tflag, "yap_flag/2");
    return (FALSE);
  }
  fv = GetFlagProp(AtomOfTerm(tflag));
  if (!fv) {
    Term fl = GLOBAL_Flags[USER_FLAGS_FLAG].at;
    if (fl == TermSilent) {
      CACHE_REGS
      Term t2 = Deref(ARG2);
      newFlag(tflag, t2);
    } else if (fl == TermWarning) {
      Yap_Warning("Flag %s does not exist", RepAtom(AtomOfTerm(fl))->StrOfAE);
    } else {
      Yap_ThrowError(DOMAIN_ERROR_PROLOG_FLAG, tflag,
                "trying to set unknown flag \"%s\"",
                AtomName(AtomOfTerm(tflag)));
    }
    return false;
  }
  if (fv->global) {
    CACHE_REGS
    switch (fv->FlagOfVE) {
    case UNKNOWN_FLAG:
    case CHARACTER_ESCAPES_FLAG:
    case BACK_QUOTES_FLAG:
    case DOUBLE_QUOTES_FLAG:
    case SINGLE_QUOTES_FLAG:
      return setYapFlagInModule(tflag, t2, CurrentModule);
    default:
      tarr = GLOBAL_Flags;
    }
  } else {
    CACHE_REGS
    tarr = LOCAL_Flags;
  }
  if (!(t2 = fv->type(t2)))
    return false;
  if (fv->helper && !(fv->helper(t2)))
    return false;
  Term tout = tarr[fv->FlagOfVE].at;
  if (IsVarTerm(tout))
    Yap_PopTermFromDB(tarr[fv->FlagOfVE].DBT);
  if (IsAtomOrIntTerm(t2))
    tarr[fv->FlagOfVE].at = t2;
  else {
    tarr[fv->FlagOfVE].DBT = Yap_StoreTermInDB(t2, 2);
  }
  return true;
}

Term Yap_UnknownFlag(Term mod) {
  if (mod == PROLOG_MODULE)
    mod = TermProlog;

  ModEntry *fv = Yap_GetModuleEntry(mod);
  if (fv == NULL)
    fv = Yap_GetModuleEntry(TermUser);
  if (fv->flags & UNKNOWN_ERROR)
    return TermError;
  if (fv->flags & UNKNOWN_WARNING)
    return TermWarning;
  return TermFail;
}

Term getYapFlag(Term tflag) {
  FlagEntry *fv;
   flag_term *tarr;
   tflag = Deref(tflag);
   if (IsVarTerm(tflag)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tflag, "yap_flag/2");
    return (FALSE);
  }
  if (IsStringTerm(tflag)) {
    tflag = MkStringTerm(RepAtom(AtomOfTerm(tflag))->StrOfAE);
  }
  if (IsApplTerm(tflag) && FunctorOfTerm(tflag) == FunctorModule) {
    Term modt;
    tflag = Yap_StripModule(tflag, &modt);
  if (IsStringTerm(tflag)) {
    tflag = MkStringTerm(RepAtom(AtomOfTerm(tflag))->StrOfAE);
  }
    if (!isatom(tflag))
      return false;
  if (IsStringTerm(modt)) {
    modt = MkStringTerm(RepAtom(AtomOfTerm(modt))->StrOfAE);
  }
    if (!isatom(modt))
      return false;
    return getYapFlagInModule(tflag, modt);
  }
  if (!IsAtomTerm(tflag)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, tflag, "yap_flag/2");
    return (FALSE);
  }
  if (tflag == TermSilent)
    {
      Yap_DebugPlWriteln(TermSilent);
    }
  fv = GetFlagProp(AtomOfTerm(tflag));
  if (!fv) {
    Term fl = GLOBAL_Flags[USER_FLAGS_FLAG].at;
    if (fl == TermSilent) {
      return false;
    } else if (fl == TermWarning) {
      Yap_Warning("Flag ~s does not exist",
                  RepAtom(AtomOfTerm(tflag))->StrOfAE);
    } else {
      Yap_ThrowError(DOMAIN_ERROR_PROLOG_FLAG, tflag,
                "trying to use  unknown flag %s",
                RepAtom(AtomOfTerm(tflag))->StrOfAE);
    }
    return false;
  }
  if (fv->global)
    tarr = GLOBAL_Flags;
  else {
    CACHE_REGS
    tarr = LOCAL_Flags;
  }
  Term tout = tarr[fv->FlagOfVE].at;
  if (IsVarTerm(tout))
    return Yap_FetchTermFromDB(tarr[fv->FlagOfVE].DBT);
  else
    return tout;
}

/** @pred set_prolog_flag(+ _Flag_,+ _Value_) is iso

Set the value for YAP Prolog flag `Flag`. Equivalent to
calling yap_flag/2 with both arguments bound.

*/
static Int set_prolog_flag(USES_REGS1) {
  Term tflag = Deref(ARG1), t2 = Deref(ARG2);
  return Yap_set_flag(tflag, t2);
}

/**   @pred source

After executing this goal, YAP keeps information on the source
of the predicates that will be consulted. This enables the use of
listing/0, listing/1 and clause/2 for those
clauses.

The same as `source_mode(_,on)` or as declaring all newly defined
static procedures as `public`.
*/
static Int source(USES_REGS1) {
  setBooleanGlobalPrologFlag(SOURCE_FLAG, true);
  return true;
}

/** @pred no_source
The opposite to `source`.

The same as `source_mode(_,off)`.

*/
static Int no_source(USES_REGS1) {
  setBooleanGlobalPrologFlag(SOURCE_FLAG, false);
  return true;
}

/**
@pred source_mode(- _O_,+ _N_)

The state of source mode can either be on or off. When the source mode
is on, all clauses are kept both as compiled code and in a "hidden"
database.  _O_ is unified with the previous state and the mode is set
according to  _N_.

*/
static Int source_mode(USES_REGS1) {
  Term targ;
  bool current = trueGlobalPrologFlag(SOURCE_FLAG);
  if (current && !Yap_unify_constant(ARG1, TermTrue))
    return false;
  if (!current && !Yap_unify_constant(ARG1, TermFalse))
    return false;
  targ = Deref(ARG2);
    Yap_set_flag(TermSource, targ);
  return true;
}

static bool setInitialValue(bool bootstrap, flag_func f, const char *s,
                            flag_term *tarr) {
  errno = 0;
  const char *ss = (const char *)s;

  if (f == booleanFlag) {
    if (!bootstrap) {
      return 0;
    }
    const char *ss = (const char *)s;
    if (!strcmp(ss, "true")) {
      tarr->at = TermTrue;
      return true;
    }
    if (!strcmp(ss, "false")) {
      tarr->at = TermFalse;
      return true;
    }
    if (!strcmp(ss, "on")) {
      tarr->at = TermTrue;
      return true;
    }
    if (!strcmp(ss, "off")) {
      tarr->at = TermFalse;
      return true;
    }
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, TermNil,
              "~s should be either true (on) or false (off)", s);
    return false;
  } else if (f == nat) {
    if (!bootstrap) {
      return 0;
    }
    UInt r = strtoul(ss, NULL, 10);
    Term t;
    if (errno) {
      Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, TermNil,
                "~s should be a positive integer)", s);
      return false;
    }
    CACHE_REGS
    t = MkIntegerTerm(r);
    if (IsIntTerm(t))
      tarr->at = t;
    else {
      tarr->DBT = Yap_StoreTermInDB(t, 2);
    }
    return true;
  } else if (f == at2n) {
    if (!bootstrap) {
      return false;
    }
    if (!strcmp(ss, "INT_MAX")) {
      tarr->at = MkIntTerm(Int_MAX);
      return true;
    }
    if (!strcmp(ss, "MAX_THREADS")) {
      tarr->at = MkIntTerm(MAX_THREADS);
      return true;
    }
    if (!strcmp(ss, "MAX_WORKERS")) {
      tarr->at = MkIntTerm(MAX_WORKERS);
      return true;
    }
    if (!strcmp(ss, "INT_MIN")) {
      tarr->at = MkIntTerm(Int_MIN);
      return true;
    }
    if (!strcmp(ss, "YAP_NUMERIC_VERSION")) {
      tarr->at = MkIntTerm(atol(YAP_NUMERIC_VERSION));
      return true;
    }
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, TermNil,
              "~s should be either true (on) or false (off)", s);
    return false;
  } else if (f == isatom) {
    if (!bootstrap) {
      return false;
    }
    Atom r = Yap_LookupAtom(s);
    if (errno) {
      Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, TermNil,
                "~s should be a positive integer)", s);
      tarr->at = TermNil;
    }
    tarr->at = MkAtomTerm(r);
    return true;
  } else if (f == options) {
    CACHE_REGS
    char tmp[512];
    Term t0;
    if (bootstrap) {
      return true;
    }
    t0 = AbsPair(HR);
    while (true) {
      int i = 0, ch = s[0];
      while (ch != '\0' && ch != ';') {
        if (ch != ' ')
          tmp[i++] = ch;
        s++;
        ch = *s;
      }
      tmp[i] = '\0';
      HR += 2;
      HR[-2] = MkAtomTerm(Yap_LookupAtom(tmp));
      if (ch) {
        HR[-1] = AbsPair(HR);
        s++;
        continue;
      } else {
        HR[-1] = TermNil;
        tarr->DBT = Yap_StoreTermInDB(t0, 2);
        return true;
      }
    }
  } else if (strcmp(ss, "@boot") == 0) {
    if (bootstrap) {
      return true;
    }

    Term t = f(TermZERO);
    if (t == TermZERO)
      return false;
    if (IsAtomOrIntTerm(t)) {
      tarr->at = t;
    } else {
      tarr->DBT = Yap_StoreTermInDB(t, 2);
    }

  } else {
    Term t0;
    if (bootstrap) {
      return false;
    }
    CACHE_REGS
    const char *us = (const char *)s;
    t0 = Yap_BufferToTermWithPrioBindings(us, TermNil, 0L, strlen(s) + 1,
                                          GLOBAL_MaxPriority);
    if (!t0)
      return false;
  if (IsStringTerm(t0)) {
   t0 = MkStringTerm(RepAtom(AtomOfTerm(t0))->StrOfAE);
  }
    if (IsAtomTerm(t0) || IsIntTerm(t0)) {
      // do yourself flags
      if (t0 == MkAtomTerm(AtomQuery)) {
        f(TermNil);
      } else {
        tarr->at = t0;
      }
    } else {
      tarr->DBT = Yap_StoreTermInDB(t0, 2);
    }
    return true;
  }
  return false;
}

#undef PAR

#define PROLOG_FLAG_PROPERTY_DEFS()                                            \
  PAR("access", isaccess, PROLOG_FLAG_PROPERTY_ACCESS, "read_write")           \
  , PAR("type", isground, PROLOG_FLAG_PROPERTY_TYPE, "term"),                  \
      PAR("scope", flagscope, PROLOG_FLAG_PROPERTY_SCOPE, "global"),           \
      PAR("keep", booleanFlag, PROLOG_FLAG_PROPERTY_KEEP, "false"),            \
      PAR(NULL, ok, PROLOG_FLAG_PROPERTY_END, 0)

#define PAR(x, y, z, w) z

typedef enum prolog_flag_property_enum_choices {
  PROLOG_FLAG_PROPERTY_DEFS()
} prolog_flag_property_choices_t;

#undef PAR

#define PAR(x, y, z, w)                                                        \
  { x, y, z, w }

static const param2_t prolog_flag_property_defs[] = {
    PROLOG_FLAG_PROPERTY_DEFS()};
#undef PAR

static Int
do_prolog_flag_property(Term tflag,
                        Term opts USES_REGS) { /* Init current_prolog_flag */
  FlagEntry *fv;
  xarg *args;
  prolog_flag_property_choices_t i;
  bool rc = true;
  args =
      Yap_ArgList2ToVector(opts, prolog_flag_property_defs,
                           PROLOG_FLAG_PROPERTY_END, DOMAIN_ERROR_PROLOG_FLAG);
  if (args == NULL) {
    Yap_ThrowError(LOCAL_Error_TYPE, opts, NULL);
    return false;
  }
  if (IsStringTerm(tflag)) {
   tflag = MkStringTerm(RepAtom(AtomOfTerm(tflag))->StrOfAE);
  }
  if (!IsAtomTerm(tflag)) {
    if (IsApplTerm(tflag) && FunctorOfTerm(tflag) == FunctorModule) {
      Term modt = CurrentModule;
      tflag = Yap_YapStripModule(tflag, &modt);
    } else {
      free(args);
      Yap_ThrowError(TYPE_ERROR_ATOM, tflag, "yap_flag/2");
      return (FALSE);
    }
  }
  fv = GetFlagProp(AtomOfTerm(tflag));
  for (i = 0; i < PROLOG_FLAG_PROPERTY_END; i++) {
    if (args[i].used) {
      switch (i) {
      case PROLOG_FLAG_PROPERTY_ACCESS:
        if (fv->rw)
          rc = rc && Yap_unify(TermReadWrite,
                               args[PROLOG_FLAG_PROPERTY_ACCESS].tvalue);
        else
          rc = rc && Yap_unify(TermReadOnly,
                               args[PROLOG_FLAG_PROPERTY_ACCESS].tvalue);
        break;
      case PROLOG_FLAG_PROPERTY_TYPE:
        if (fv->type == booleanFlag)
          rc = rc &&
               Yap_unify(TermBoolean, args[PROLOG_FLAG_PROPERTY_TYPE].tvalue);
        else if (fv->type == isatom)
          rc =
              rc && Yap_unify(TermAtom, args[PROLOG_FLAG_PROPERTY_TYPE].tvalue);
        else if (fv->type == nat)
          rc = rc &&

               Yap_unify(TermInteger, args[PROLOG_FLAG_PROPERTY_TYPE].tvalue);
        else if (fv->type == isfloat)
          rc = rc &&
               Yap_unify(TermFloat, args[PROLOG_FLAG_PROPERTY_TYPE].tvalue);
        else
          rc =
              rc && Yap_unify(TermTerm, args[PROLOG_FLAG_PROPERTY_TYPE].tvalue);
        break;
      case PROLOG_FLAG_PROPERTY_KEEP:
        rc = rc && false;
        break;
      case PROLOG_FLAG_PROPERTY_SCOPE:
        if (fv->global) {
          if (fv->FlagOfVE == UNKNOWN_FLAG ||
              fv->FlagOfVE == CHARACTER_ESCAPES_FLAG ||
              fv->FlagOfVE == SINGLE_QUOTES_FLAG ||
              fv->FlagOfVE == DOUBLE_QUOTES_FLAG ||
              fv->FlagOfVE == BACK_QUOTES_FLAG)
            Yap_unify(TermModule, args[PROLOG_FLAG_PROPERTY_SCOPE].tvalue);
          rc = rc &&
               Yap_unify(TermGlobal, args[PROLOG_FLAG_PROPERTY_SCOPE].tvalue);
        } else
          rc = rc &&
               Yap_unify(TermThread, args[PROLOG_FLAG_PROPERTY_SCOPE].tvalue);
        break;
      case PROLOG_FLAG_PROPERTY_END:
        /* break; */
        Yap_ThrowError(DOMAIN_ERROR_PROLOG_FLAG, opts, "Flag not supported by YAP");
      }
    }
  }
  // UNLOCK(GLOBAL_Prolog_Flag[sno].prolog_flaglock);
  free(args);
  return rc;
}

static Int cont_prolog_flag_property(USES_REGS1) { /* current_prolog_flag */
  int i = IntOfTerm(EXTRA_CBACK_ARG(2, 1));

  while (i < GLOBAL_flagCount + LOCAL_flagCount) {
    int gmax = GLOBAL_flagCount;
    int lmax = LOCAL_flagCount;
    Term lab;

    if (i >= gmax + lmax) {
      cut_fail();
    } else if (i >= gmax) {
      lab = MkAtomTerm(Yap_LookupAtom(local_flags_setup[i - gmax].name));
    } else {
      if (i == UNKNOWN_FLAG || i == CHARACTER_ESCAPES_FLAG ||
          i == SINGLE_QUOTES_FLAG || i == DOUBLE_QUOTES_FLAG ||
          i == BACK_QUOTES_FLAG) {
        Term labs[2];
        labs[0] = MkVarTerm();
        labs[1] = MkAtomTerm(Yap_LookupAtom(global_flags_setup[i].name));
        lab = Yap_MkApplTerm(FunctorModule, 2, labs);
      } else {
        lab = MkAtomTerm(Yap_LookupAtom(global_flags_setup[i].name));
      }
    }
    EXTRA_CBACK_ARG(2, 1) = MkIntTerm(++i);
    Yap_unify(ARG1, lab);
    return do_prolog_flag_property(lab, Deref(ARG2) PASS_REGS);
  }
  cut_fail();
}

/** @pred prolog_flag_property(+ _Flag_,+ _Prooperties_)

Report a property for a YAP Prolog flag.  _Properties_ include

* `type(+_Type_)` with _Type_ one of `boolean`, `integer`, `float`, `atom`
and `term` (that is, any ground term)

* `access(+_Access_)` with  _Access_ one of `read_only` or `read_write`

* `scope(+_Scope_) the flag aplies to a `thread`, to a `module`, or is
`global` to the system.

*/
static Int prolog_flag_property(USES_REGS1) { /* Init current_prolog_flag */
  Term t1 = Deref(ARG1);
  /* make valgrind happy by always filling in memory */
  EXTRA_CBACK_ARG(2, 1) = MkIntTerm(0);
  if (IsStringTerm(t1)) {
    t1 = MkStringTerm(RepAtom(AtomOfTerm(t1))->StrOfAE);
  }
  if (IsVarTerm(t1)) {
    return (cont_prolog_flag_property(PASS_REGS1));
  } else {
    if (IsApplTerm(t1) && FunctorOfTerm(t1) == FunctorModule) {
      Term modt;
      t1 = Yap_StripModule(t1, &modt);
      if (IsAtomTerm(modt)) {
        Int rc;
        rc = cont_prolog_flag_property(PASS_REGS1);

        return rc;
      }
    } else if (IsAtomTerm(t1)) {
      do_cut(0);
      return do_prolog_flag_property(t1, Deref(ARG2) PASS_REGS);
    } else {
      Yap_ThrowError(TYPE_ERROR_ATOM, t1, "prolog_flag_property/2");
    }
  }
  return false;
}

static void newFlag(Term fl, Term val) {
  flag_info f;
  int i = GLOBAL_flagCount;

  GLOBAL_flagCount++;
  f.name = (char *)RepAtom(AtomOfTerm(fl))->StrOfAE;
  f.writable = true;
  f.helper = NULL;
  f.def = ok;
  initFlag(&f, i, true);
  if (IsAtomOrIntTerm(val)) {
    GLOBAL_Flags[i].at = val;
  } else {
    GLOBAL_Flags[i].DBT = Yap_StoreTermInDB(val, 2);
  }
}

static Int do_create_prolog_flag(USES_REGS1) {
  FlagEntry *fv;
  xarg *args;
  prolog_flag_property_choices_t i;
  Term tflag = Deref(ARG1), tval = Deref(ARG2), opts = Deref(ARG3);

  args =
      Yap_ArgList2ToVector(opts, prolog_flag_property_defs,
                           PROLOG_FLAG_PROPERTY_END, DOMAIN_ERROR_PROLOG_FLAG);
  if (args == NULL) {
    Yap_ThrowError(LOCAL_Error_TYPE, opts, NULL);
    return false;
  }
  fv = GetFlagProp(AtomOfTerm(tflag));
  if (fv) {
    if (args[PROLOG_FLAG_PROPERTY_KEEP].used &&
        args[PROLOG_FLAG_PROPERTY_KEEP].tvalue == TermTrue) {
      free(args);
      return true;
    }
  } else {
    newFlag(tflag, tval);
    fv = GetFlagProp(AtomOfTerm(tflag));
  }
  for (i = 0; i < PROLOG_FLAG_PROPERTY_END; i++) {
    if (args[i].used) {
      switch (i) {
      case PROLOG_FLAG_PROPERTY_KEEP:
        break;
      case PROLOG_FLAG_PROPERTY_ACCESS:
        if (args[PROLOG_FLAG_PROPERTY_ACCESS].tvalue == TermReadWrite)
          fv->rw = true;
        else
          fv->rw = false;
        break;
      case PROLOG_FLAG_PROPERTY_TYPE: {
        Term ttype = args[PROLOG_FLAG_PROPERTY_TYPE].tvalue;
        if (ttype == TermBoolean)
          fv->type = booleanFlag;
        else if (ttype == TermInteger)
          fv->type = isatom;
        else if (ttype == TermFloat)
          fv->type = isfloat;
        else
          fv->type = isground;
      } break;
      case PROLOG_FLAG_PROPERTY_SCOPE:
        free(args);
        return false;
      case PROLOG_FLAG_PROPERTY_END:
        break;
      }
    }
  }
  // UNLOCK(GLOBAL_Prolog_Flag[sno].prolog_flaglock);
  free(args);
  return true;
}

/**
 * Create a new global prolog flag.
 *
 * @arg name
 * @arg whether read-only or writable
 * @arg type: boolean, integer, atom, any as a pprolog term
 *
 */
X_API bool Yap_create_prolog_flag(const char *name, bool writable,  Term ttype, Term v) {

  Atom aname = Yap_LookupAtom (name);
  FlagEntry *fv;
   fv = GetFlagProp(aname);
  if (fv) {
    return false;
  } else {
    newFlag(MkAtomTerm(aname), v);
    fv = GetFlagProp(aname);
  }
 fv->rw = writable;
        if (ttype == TermBoolean)
          fv->type = booleanFlag;
        else if (ttype == TermInteger)
          fv->type = isatom;
        else if (ttype == TermFloat)
          fv->type = isfloat;
        else
          fv->type = isground;
 return true;
      }

/**
 * Init System Prolog flags. This is done in two phases:
 *   early on, it takes care of the atomic flags that are required by other
 *modules;
 * later, it looks at flags that are structured terms
 *
 * @param bootstrap: wether this is done before stack initialization, or
 *afterwards.
 * Complex terms can only be built in the second step.
 */

void Yap_InitFlags(bool bootstrap) {
  CACHE_REGS
  tr_fr_ptr tr0 = TR;
  flag_info *f = global_flags_setup;
  int lvl = push_text_stack();
  char *buf = Malloc(4098);
  GLOBAL_flagCount = 0;
  if (bootstrap) {
    GLOBAL_Flags = (union flagTerm *)Yap_AllocCodeSpace(
        sizeof(union flagTerm) *
        (2 * sizeof(global_flags_setup) / sizeof(flag_info)));
  }
  while (f->name != NULL) {
    bool itf = setInitialValue(bootstrap, f->def, f->init,
                               GLOBAL_Flags + GLOBAL_flagCount);
    if (itf) {
      initFlag(f, GLOBAL_flagCount, true);
    }
    GLOBAL_flagCount++;
    f++;
  }
  LOCAL_flagCount = 0;
  int nflags = sizeof(local_flags_setup) / sizeof(flag_info);
  if (bootstrap)
    LOCAL_Flags =
        (union flagTerm *)Yap_AllocCodeSpace(sizeof(union flagTerm) * nflags);
  f = local_flags_setup;
  while (f->name != NULL) {
     char *s;
    if (f->init == NULL || f->init[0] == '\0') s = NULL;
    else if (strlen(f->init) < 4096) {
      s = buf;
      strcpy(buf, f->init);
    } else {
      s = Malloc(strlen(f->init)+1);
      strcpy(s, f->init);
    }
    bool itf = setInitialValue(bootstrap, f->def, s,
                               LOCAL_Flags + LOCAL_flagCount);
    //    Term itf = Yap_BufferToTermWithPrioBindings(f->init,
    //    strlen(f->init)+1,
    //    LOBAL_MaxPriority, &tp);
    if (itf) {
      initFlag(f, LOCAL_flagCount, false);
    }
    LOCAL_flagCount++;
    f++;
  }
  // fix readline gettong set so early
  if (GLOBAL_Stream[StdInStream].status & Readline_Stream_f) {
    setBooleanGlobalPrologFlag(READLINE_FLAG, true);
  }
  pop_text_stack(lvl);
  if (!bootstrap) {
    Yap_InitCPredBack("current_prolog_flag", 2, 1, current_prolog_flag,
                      cont_yap_flag, 0);
    TR = tr0;
    Yap_InitCPredBack("prolog_flag", 3, 1, prolog_flag, cont_yap_flag,
                      0);
    Yap_InitCPredBack("yap_flag", 3, 1, yap_flag, cont_yap_flag, 0);
    Yap_InitCPredBack("prolog_flag", 2, 1, current_prolog_flag2,
                      cont_current_prolog_flag, 0);
    Yap_InitCPredBack("current_prolog_flag", 2, 1, current_prolog_flag2,
                      cont_current_prolog_flag, 0);
    Yap_InitCPred("set_prolog_flag", 2, set_prolog_flag, SyncPredFlag);
    Yap_InitCPred("$create_prolog_flag", 3, do_create_prolog_flag,
                  SyncPredFlag);
    Yap_InitCPredBack("yap_flag", 2, 1, yap_flag, cont_yap_flag, 0);
    Yap_InitCPredBack("prolog_flag_property", 2, 1, prolog_flag_property,
                      cont_prolog_flag_property, 0);
    Yap_InitCPred("source", 0, source, SyncPredFlag);
    Yap_InitCPred("no_source", 0, no_source, SyncPredFlag);
    Yap_InitCPred("source_mode", 2, source_mode, SyncPredFlag);
  }
}

/* Accessing and changing the flags for a predicate */

/// @}


