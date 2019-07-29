/// @file yapdb.hh
///
/// @brief C++ Interface to generated code.

#ifndef _YAPDB_H
#define _YAPDB_H

#include <YapInterface.h>

#define YAP_CPP_DB_INTERFACE 1

/**
 *
 *   @defgroup yap-cplus-db-interface Data-Base Component of YAP interface.
 *
 *   @ingroup yap-cplus-interface
 * @{
 *    @tableofcontents
 *
 *
 * These classes define the main data-structures stored to represent compiled
 * programs:
 *
 *  + YAPFunctor represents a name/arity combination.
 *
 *  + YAPModule wraps the YAP module implementation.
 *
 *  + YAPPredicate and subclasses store the actual program, Preliminary
 * support covers Prolog and C-defined predicates.
 */

class YAPTerm;
class YAPAtomTerm;

class YAPError;

class YAPModule;

/**
 * @brief YAPModule
 * A YAPModule describes a bare module, which in YAP is just a name.
 *
 * Info about the module is in YAPModuleProp
 *
 */
class X_API YAPModule : protected YAPAtomTerm {
  friend class YAPPredicate;
  friend class YAPModuleProp;
  Term t() { return gt(); }
  Term curModule() { CACHE_REGS return Yap_CurrentModule(); }

public:
  YAPModule(YAP_Term t) : YAPAtomTerm(t){};
  YAPModule() : YAPAtomTerm(curModule()){};
  YAPModule(YAPAtom t) : YAPAtomTerm(t){};
  YAPModule(YAPStringTerm t) : YAPAtomTerm(t.getString()){};
  Term term() { return gt(); };
};

/**
 * @brief YAPModuleProp
 * A YAPModuleProp controls access to a module property.
 *
 */
class X_API YAPModuleProp : public YAPProp {
  friend class YAPPredicate;
  ModEntry *m;

  YAPModuleProp(ModEntry *mod) { m = mod; };
  YAPModuleProp(Term tmod) { m = Yap_GetModuleEntry(tmod); };

public:
  YAPModuleProp(YAPModule tmod) { m = Yap_GetModuleEntry(tmod.gt()); };
  YAPModuleProp() { m = Yap_GetModuleEntry(Yap_CurrentModule()); };
  virtual YAPModule module() { return YAPModule(m->AtomOfME); };
};

/**
 * @brief Predicates
 *
 * This class interfaces with PredEntry in Yatom.
 */
class X_API YAPPredicate : public YAPModuleProp {
  friend class YAPQuery;
  friend class YAPEngine;

protected:
  PredEntry *ap;

  /// auxiliary routine to find a predicate in the current module.

  /// auxiliary routine to find a predicate in the current module.
  PredEntry *getPred(Term &t, Term &tm, CELL *&outp);

  PredEntry *asPred() { return ap; };

  /// Empty constructor for predicates
  ///
  /// Just do nothing.
  inline YAPPredicate() {}
  YAPPredicate(Term &to, Term &tmod, CELL *&ts, const char *pname);

  /// Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(Term t, CELL *&v) {
    if (t) {
      Term tm = Yap_CurrentModule();
      ap = getPred(t, tm, v);
    }
  }

  inline YAPPredicate(Term t) {
    if (t) {
      CELL *v = nullptr;
      Term tm = Yap_CurrentModule();
      ap = getPred(t, tm, v);
    }
  }

  /// Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(YAPTerm t, CELL *&v) {
    Term tp = t.term(), tm = Yap_CurrentModule();
    ap = getPred(tp, tm, v);
  }
  inline YAPPredicate(YAPTerm t) {
    CELL *v = nullptr;
    Term tp = t.term();
    Term tm = Yap_CurrentModule();
    ap = getPred(tp, tm, v);
  }

  /// Cast constructor for predicates,
  /// if we have the implementation data.
  ///
  inline YAPPredicate(PredEntry *pe) { ap = pe; }

  /// Functor constructor for predicates, is given a specific module.
  /// This version avoids manufacturing objects
  inline YAPPredicate(Functor f, Term mod) {
    ap = RepPredProp(PredPropByFunc(f, mod));
  }

public:
  /// String constructor for predicates
  ///
  /// It also communicates the array of arguments t[]
  /// and the array of variables
  /// back to yapquery
  YAPPredicate(const char *s0, Term &tout, YAPPairTerm * &names, CELL *&nts) {
    CACHE_REGS
    const char *s = (const char *)s0;
    Term tnames = MkVarTerm(),
      parameters = TermNil;
    tout =
        Yap_BufferToTermWithPrioBindings(s, parameters, tnames, strlen(s0)+1, 1200);
    
    // fprintf(stderr,"ap=%p arity=%d text=%s", ap, ap->ArityOfPE, s);
    if (tout == 0L) {
      return;
      throw YAPError();
    }
    Term tm = Yap_CurrentModule();
    ap = getPred(tout, tm, nts);
    tout = Yap_SaveTerm(tout);
    
    names = new YAPPairTerm(tnames);
  }

  /// Functor constructor for predicates
  ///
  /// Asssumes that we use the current module.
  YAPPredicate(YAPFunctor f) {
    CACHE_REGS
    ap = RepPredProp(PredPropByFunc(f.f, Yap_CurrentModule()));
  }

  /// Functor constructor for predicates, is given a specific module.
  ///
  inline YAPPredicate(YAPFunctor f, YAPTerm mod) {
    ap = RepPredProp(PredPropByFunc(f.f, mod.term()));
  }

  /// Name/arity constructor for predicates.
  ///
  inline YAPPredicate(YAPAtom at, YAPTerm mod) {
    ap = RepPredProp(PredPropByAtom(at.a, mod.term()));
  }

  /// Name/0 constructor for predicates.
  ///
  YAPPredicate(YAPAtom at);

  /// Mod:Name/Arity constructor for predicates.
  ///
  inline YAPPredicate(YAPAtom at, uintptr_t arity, YAPModule mod) {
    if (arity) {
      Functor f = Yap_MkFunctor(at.a, arity);
      ap = RepPredProp(PredPropByFunc(f, mod.term()));
    } else {
      ap = RepPredProp(PredPropByAtom(at.a, mod.term()));
    }
  }

  /// Atom/Arity constructor for predicates.
  ///
  YAPPredicate(YAPAtom at, uintptr_t arity);

  /// char */module constructor for predicates.
  ///
  inline YAPPredicate(const char *at, uintptr_t arity) {
    ap = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(at), arity),
                                    Yap_CurrentModule()));
  };

  /// char */module constructor for predicates.
  ///
  inline YAPPredicate(const char *at, uintptr_t arity, YAPTerm mod) {
    ap = RepPredProp(
        PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(at), arity), mod.term()));
  };

  /// char */module constructor for predicates.
  ///
  inline YAPPredicate(const char *at, YAPTerm mod) {
    ap = RepPredProp(PredPropByAtom(Yap_LookupAtom(at), mod.term()));
  }

  /// module of a predicate
  ///
  /// notice that modules are currently treated as atoms, this should change.
  YAPModule module() {
    if (ap->ModuleOfPred == PROLOG_MODULE)
      return YAPModule(AtomProlog);
    else
      return YAPModule(AtomOfTerm(ap->ModuleOfPred));
  }

  /// name of predicate
  ///
  /// notice that we return the atom, not a string.
  YAPAtom name() {
    if (ap->ArityOfPE)
      return YAPAtom((Atom)ap->FunctorOfPred);
    else
      return YAPAtom(NameOfFunctor(ap->FunctorOfPred));
  }

  /// functor of predicate
  ///
  /// onlu defined if arity >= 1
  YAPFunctor functor() {
    if (ap->ArityOfPE)
      return YAPFunctor(ap->FunctorOfPred);
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, MkIntTerm(0),
                   "YAPFunctor::functor");
  }

  /// arity of predicate
  ///
  /// we return a positive number.
  uintptr_t getArity() { return ap->ArityOfPE; }
  arity_t arity() { return ap->ArityOfPE; }
  PredEntry *predEntry() { return ap; }
};

/**
 * @brief PrologPredicate
 *
 * This class interfaces with Predicates Implemented in Prolog.
 */
class X_API YAPPrologPredicate : public YAPPredicate {
public:
  YAPPrologPredicate(YAPTerm t) : YAPPredicate(t){};
  YAPPrologPredicate(const char *s, arity_t arity) : YAPPredicate(s, arity){};
  YAPPrologPredicate(YAPAtom s, arity_t arity) : YAPPredicate(s, arity){};
  /// add a new clause
  bool assertClause(YAPTerm clause, bool last = true,
                    YAPTerm source = YAPTerm());
  /// add a new tuple
  bool assertFact(YAPTerm *tuple, bool last = true);
  /// retract at least the first clause matching the predicate.
  void *retractClause(YAPTerm skeleton, bool all = false);
  /// return the Nth clause (if source is available)
  // YAPTerm clause(size_t index, YAPPredicate p) { return YAPTerm(); };
  /// return the Nth clause (if source is available)
  YAPTerm *nextClause() { return nullptr; };
};

/**
 * @brief PrologPredicate
 *
 * This class interfaces with Predicates Implemented in Prolog.
 */
class X_API YAPFLIP : public YAPPredicate {
public:
  YAPFLIP(YAP_UserCPred call, YAPAtom name, YAP_Arity arity,
          YAPModule module = YAPModule(), YAP_UserCPred retry = 0,
          YAP_UserCPred cut = 0, YAP_Arity extra = 0, bool test = false)
      : YAPPredicate(name, arity, module) {
    if (retry) {
      YAP_UserBackCutCPredicate(name.getName(), call, retry, cut, arity, extra);
    } else {
      if (test) {
        YAP_UserCPredicate(name.getName(), call, arity);
      } else {
        YAP_UserCPredicate(name.getName(), call, arity);
      }
    }
  };
  YAPFLIP(const char *name, uintptr_t arity, YAPModule module = YAPModule(),
          bool backtrackable = false)
      : YAPPredicate(YAPAtom(name), arity, module) {
    if (backtrackable) {
      Yap_InitCPredBackCut(name, arity, 0, 0, 0, 0, UserCPredFlag);
    } else {
      YAP_UserCPredicate(name, 0, arity);
    }
  };
  bool addCall(CPredicate call) { return Yap_AddCallToFli(ap, call); }
  bool addRetry(CPredicate call) { return Yap_AddRetryToFli(ap, call); }
  bool addCut(CPredicate call) { return Yap_AddCutToFli(ap, call); }
};

#endif

/// @}
