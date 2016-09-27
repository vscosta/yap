
#ifndef _YAPDB_H
#define _YAPDB_H

#define YAP_CPP_DB_INTERFACE 1

//! @{

/**
 *
 *   @defgroup yap-cplus-db-interface Data-Base Component of YAP interface.
 *
 *   @ingroup yap-cplus-interface
 *    @tableofcontents
 *
 *
 * Data-base component of C++ interface to YAP. It manipulates sets of
 * atoms, each one containing a number of props.
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
class YAPModule : protected YAPAtomTerm {
  friend class YAPPredicate;
  YAPModule(Term t) : YAPAtomTerm(t){};
  Term t() { return gt(); }
  Term curModule() { CACHE_REGS return Yap_CurrentModule(); }

public:
  ~YAPModule(){};
  YAPModule() : YAPAtomTerm(curModule()){};
  YAPModule(YAPAtom t) : YAPAtomTerm(t){};
};

/**
 * @brief YAPModuleProp
 * A YAPModuleProp controls access to a module property.
 *
 */
class YAPModuleProp : public YAPProp {
  friend class YAPPredicate;
  ModEntry *m;

  YAPModuleProp(ModEntry *mod) { m = mod; };
  YAPModuleProp(Term tmod) { m = Yap_GetModuleEntry(tmod); };

public:
  YAPModuleProp() { CACHE_REGS m = Yap_GetModuleEntry(Yap_CurrentModule()); };
  YAPModuleProp(YAPModule tmod);
  virtual YAPModule module() { return YAPModule(m->AtomOfME); };
};

/**
 * @brief YAPFunctor represents Prolog functors Name/Arity
 */
class YAPFunctor : public YAPProp {
  friend class YAPApplTerm;
  friend class YAPTerm;
  friend class YAPPredicate;
  friend class YAPQuery;
  Functor f;
  /// Constructor: receives Prolog functor and casts it to YAPFunctor
  ///
  /// Notice that this is designed for internal use only.
  inline YAPFunctor(Functor ff) { f = ff; }

public:
  /// Constructor: receives name as an atom, plus arity
  ///
  /// This is the default method, and the most popular
  YAPFunctor(YAPAtom at, uintptr_t arity) { f = Yap_MkFunctor(at.a, arity); }

  /// Constructor: receives name as a string plus arity
  ///
  /// Notice that this is designed for ISO-LATIN-1 right now
  /// Note: Python confuses the 3 constructors,
  /// use YAPFunctorFromString
  inline YAPFunctor(const char *s, uintptr_t arity, bool isutf8 = true) {
    f = Yap_MkFunctor(Yap_LookupAtom(s), arity);
  }
  /// Constructor: receives name as a  wide string plus arity
  ///
  /// Notice that this is designed for UNICODE right now
  ///
  /// Note: Python confuses the 3 constructors,
  /// use YAPFunctorFromWideString
  inline YAPFunctor(const wchar_t *s, uintptr_t arity) {
    f = Yap_MkFunctor(Yap_LookupWideAtom(s), arity);
  }
  ~YAPFunctor(){};
  /// Getter: extract name of functor as an atom
  ///
  /// this is for external usage.
  YAPAtom name(void) { return YAPAtom(NameOfFunctor(f)); }

  /// Getter: extract arity of functor as an unsigned integer
  ///
  /// this is for external usage.
  uintptr_t arity(void) { return ArityOfFunctor(f); }
};

/**
 * @brief Predicates
 *
 * This class interfaces with PredEntry in Yatom.
 */
class YAPPredicate : public YAPModuleProp {
  friend class YAPQuery;
  friend class YAPEngine;

protected:
  PredEntry *ap;

  /// auxiliary routine to find a predicate in the current module.
  PredEntry *getPred(Term &t, Term *&outp);

  PredEntry *asPred() { return ap; };

  /// String constructor for predicates
  ///
  /// It also communicates the array of arguments t[]
  /// and the array of variables
  /// back to yapquery
  YAPPredicate(const char *s0, Term &out, Term &names) {
    CACHE_REGS
    BACKUP_MACHINE_REGS();
    Term *modp = NULL;

    out = Yap_StringToTerm(s0, strlen(s0) + 1, &LOCAL_encoding, 1200, &names);
    // extern char *s0;
    // fprintf(stderr,"ap=%p arity=%d text=%s", ap, ap->ArityOfPE, s);
    //  Yap_DebugPlWrite(out);
    //  delete [] ns;
    if (out == 0L)
      ap = nullptr;
    else
      ap = getPred(out, modp);
    RECOVER_MACHINE_REGS();
  }

  /// Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(Term t) {
    CELL *v = NULL;
    ap = getPred(t, v);
  }

  /// Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(YAPTerm t) {
    Term *v = nullptr;
    Term tt = t.term();
    ap = getPred(tt, v);
  }

  /// Cast constructor for predicates,
  /// if we have the implementation data.
  ///
  inline YAPPredicate(PredEntry *pe) { ap = pe; }

public:
  ~YAPPredicate(){};

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
    ap = RepPredProp(PredPropByFunc(f.f, mod.t));
  }

  /// Name/arity constructor for predicates.
  ///
  inline YAPPredicate(YAPAtom at, YAPTerm mod) {
    ap = RepPredProp(PredPropByAtom(at.a, mod.t));
  }

  /// Name/0 constructor for predicates.
  ///
  YAPPredicate(YAPAtom at);

  /// Mod:Name/Arity constructor for predicates.
  ///
  inline YAPPredicate(YAPAtom at, uintptr_t arity, YAPModule mod) {
    if (arity) {
      Functor f = Yap_MkFunctor(at.a, arity);
      ap = RepPredProp(PredPropByFunc(f, mod.t()));
    } else {
      ap = RepPredProp(PredPropByAtom(at.a, mod.t()));
    }
  }

  /// Atom/Arity constructor for predicates.
  ///
  YAPPredicate(YAPAtom at, uintptr_t arity);

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

  /// arity of predicate
  ///
  /// we return a positive number.
  uintptr_t getArity() { return ap->ArityOfPE; }
  arity_t arity() { return ap->ArityOfPE; }
};

/**
 * @brief PrologPredicate
 *
 * This class interfaces with Predicates Implemented in Prolog.
 */
class YAPPrologPredicate : public YAPPredicate {
public:
  YAPPrologPredicate(YAPTerm t);
  /// add a new clause
  void *assertClause(YAPTerm clause, bool last = true,
                     YAPTerm source = YAPTerm());
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
class YAPFLIP : public YAPPredicate {
public:
  YAPFLIP(CPredicate call, YAPAtom name, uintptr_t arity,
          YAPModule module = YAPModule(), CPredicate retry = 0,
          CPredicate cut = 0, size_t extra = 0, bool test = false)
      : YAPPredicate(name, arity, module) {
    if (retry) {
      Yap_InitCPredBackCut(name.getName(), arity, extra, call, retry, cut,
                           UserCPredFlag);
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
