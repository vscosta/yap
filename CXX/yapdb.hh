/// @file yapdb.hh
///
/// @brief C++ Interface to generated code.

#ifndef _YAPDB_H
#define _YAPDB_H

#include "yapa.hh"
#include <YapInterface.h>

#define YAP_CPP_DB_INTERFACE 1

/**
 *
 *   @defgroup FLI_YAP-cplus-db-interface Data-Base Component of YAP interface.
 *
 *   @ingroup FLI_YAP-cplus-interface
 * @{
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
 *    @tableofcontents
 */

class YAPTerm;
class YAPAtomTerm;

class YAPError;

class YAPModule;

/**
 * @class YAPModule
 *
 * A YAPModule describes a module, which in YAP
 * can just be a name, but may also be a module property.
 *
 *
 */
class X_API YAPModule :  public YAPAtomTerm {
  friend class YAPPredicate;
  //  ModEntry m;
public:
  ///< create or fetch the YAPModule whose name is contained in a `C`term;
  YAPModule(YAP_Term t) : YAPAtomTerm(t){};
  ///< create or fetch the YAPModule whose name is contained in a `C`term;
  YAPModule() : YAPAtomTerm(YAP_CurrentModule()){};
  YAPModule(std::string t) : YAPAtomTerm(t) {};
};

/**
 * @class Predicates
 *
 * This class interfaces with PredEntry in Yatom.
 */
class X_API YAPPredicate {
  friend class YAPQuery;
  friend class YAPEngine;
  friend class YAPModule;

protected:
  PredEntry *ap;

  PredEntry *getPred(Term &t, Term &tm, CELL *&outp);  ///< auxiliary routine to find a predicate in the current module.

  PredEntry *asPred() { return ap; };

  ///< Empty constructor for predicates
  ///
  /// Just do nothing.
  inline YAPPredicate() {ap = NULL;};
  YAPPredicate(Term &to, Term &tmod, CELL *&ts, const char *pname);
  
  ///< Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(Term t, CELL *&v)  {
    CACHE_REGS
      
    if (t) {
      Term tm = Yap_CurrentModule();
      ap = getPred(t, tm, v);
    }
  }

  inline YAPPredicate(Term t) {
          CACHE_REGS

    if (t) {
      CELL *v = nullptr;
      Term tm = Yap_CurrentModule();
      ap = getPred(t, tm, v);
    }
  }

  ///< Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(YAPTerm t, CELL *&v) {
          CACHE_REGS

    Term tp = t.term(), tm = Yap_CurrentModule();
    ap = getPred(tp, tm, v);
  }
  inline YAPPredicate(YAPTerm t) {
          CACHE_REGS

    CELL *v = nullptr;
    Term tp = t.term();
    Term tm = Yap_CurrentModule();
    ap = getPred(tp, tm, v);
  }

   inline YAPPredicate(PredEntry *pe) { ap = pe; }   ///< Cast constructor for predicates, from C to C++

  inline YAPPredicate(Functor f, Term mod) {  ///< C Functor plus C Module constructor for predicates.

    ap = RepPredProp(PredPropByFunc(f, mod));
  }

public:
  ///< String constructor for predicates
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

  
  YAPPredicate(YAPFunctor f) {///< Functor constructor for predicates, assumes that we use the current module.
    CACHE_REGS
    ap = RepPredProp(PredPropByFunc(f.f, Yap_CurrentModule()));
  }

  

  inline YAPPredicate(YAPFunctor f, YAPTerm mod) {///< Functor constructor for predicates, is given a specific module.
    ap = RepPredProp(PredPropByFunc(f.f, mod.term()));
  }

  inline YAPPredicate(YAPAtom at, YAPTerm mod) {  ///< Name/arity constructor for predicates.

    ap = RepPredProp(PredPropByAtom(at.a, mod.term()));
  }

    YAPPredicate(YAPAtom at);  ///< Name/0 constructor for predicates.


  ///< Mod:Name/Arity constructor for predicates.
  ///
  inline YAPPredicate(YAPAtom at, uintptr_t arity, YAPModule mod) {
    if (arity) {
      Functor f = Yap_MkFunctor(at.a, arity);
      ap = RepPredProp(PredPropByFunc(f, mod.term()));
    } else {
      ap = RepPredProp(PredPropByAtom(at.a, mod.term()));
    }
  }

 
    YAPPredicate(YAPAtom at, uintptr_t arity);   ///< Atom/Arity constructor for predicates.

    inline YAPPredicate(const std::string at, uintptr_t arity, std::string mod="") {  ///< std::string/Arity constructor for predicates.

          CACHE_REGS

  Term m;
  if (//mod != nullptr &&
      mod[0] != 0)
    m = MkAtomTerm(Yap_LookupAtom(mod.c_str()));
  else
    m =  Yap_CurrentModule();
  ap = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(at.c_str()), arity),
				m));
}

    inline YAPPredicate(const char *at, uintptr_t arity) {   ///< char */arity constructor for predicates.

          CACHE_REGS

    ap = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(at), arity),
                                    Yap_CurrentModule()));
  };

    inline YAPPredicate(const char *at, uintptr_t arity, YAPTerm mod) {   ///< char */module constructor for predicates.

    ap = RepPredProp(
        PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom(at), arity), mod.term()));
  };

    inline YAPPredicate(const char *at, YAPTerm mod) {   ///< char */module constructor for predicates.

    ap = RepPredProp(PredPropByAtom(Yap_LookupAtom(at), mod.term()));
  }

    /// notice that modules are currently treated as atoms, this should change.
  YAPModule module() {   ///< module of a predicate

    if (ap->ModuleOfPred == PROLOG_MODULE)
      return YAPModule(std::string("prolog"));
    else
      return YAPModule(AtomOfTerm(ap->ModuleOfPred)->StrOfAE);
	};

    /// notice that we return the atom, not a string.
  YAPAtom name() {   ///< name of predicate

    if (ap->ArityOfPE)
      return YAPAtom((Atom)ap->FunctorOfPred);
    else
      return YAPAtom(NameOfFunctor(ap->FunctorOfPred));
  };

    /// onlu defined if arity >= 1
  YAPFunctor functor() {   ///< functor of predicate

    if (ap->ArityOfPE)
      return YAPFunctor(ap->FunctorOfPred);
    Yap_ThrowError(DOMAIN_ERROR_OUT_OF_RANGE, MkIntTerm(0),
                   "YAPFunctor::functor");
    return (YAPFunctor)0;
  };


  uintptr_t getArity() { return ap->ArityOfPE; };   ///< arity of predicate, we return a positive number.

  arity_t arity() { return ap->ArityOfPE; }
  PredEntry *predEntry() { return ap; }
};

/**
 * @class YAPPrologPredicate
 *
 * This class interfaces with Predicates Implemented in Prolog.
 */
class X_API YAPPrologPredicate : public YAPPredicate {
public:
  YAPPrologPredicate(YAPTerm t) : YAPPredicate(t){};
  YAPPrologPredicate(const char *s, arity_t arity) : YAPPredicate(s, arity){};
  YAPPrologPredicate(YAPAtom s, arity_t arity) : YAPPredicate(s, arity){};
  bool assertClause(YAPTerm clause, bool last = true,   ///< add a new clause

                    YAPTerm source = YAPTerm());
  bool assertFact(YAPTerm *tuple, bool last = true);   ///< add a new tuple

  void *retractClause(YAPTerm skeleton, bool all = false);   ///< retract at least the first clause matching the predicate.

  // YAPTerm clause(size_t index, YAPPredicate p) { return YAPTerm(); };   ///< return the Nth clause (if source is available)

  YAPTerm *nextClause() { return nullptr; };   ///< return the Nth clause (if source is available)

};

/**
 * @class YAPFLIP
 *
 * This class interfaces with Predicates Implemented in Prolog.
 */
class X_API YAPFLIP : public YAPPredicate {
public:
  YAPFLIP(YAP_UserCPred call, std::string name, YAP_Arity arity,
          const std::string module = std::string(RepAtom(AtomOfTerm(YAP_CurrentModule()))->StrOfAE), YAP_UserCPred retry = 0,
          YAP_UserCPred cut = 0, YAP_Arity extra = 0, bool test = false)
    : YAPPredicate(name.c_str(), arity, MkAtomTerm(Yap_LookupAtom(module.c_str()))) {
    //CACHE_REGS
    YAP_UserCPredicate(name.c_str(), call, arity);
  };
  YAPFLIP(const char *name, uintptr_t arity, YAPModule module = YAPModule())
      : YAPPredicate(YAPAtom(name), arity, module) {
      YAP_UserCPredicate(name, 0, arity);
  };
  bool addCall(CPredicate call) { return Yap_AddCallToFli(ap, call); }
  bool addRetry(CPredicate call) { return Yap_AddRetryToFli(ap, call); }
  bool addCut(CPredicate call) { return Yap_AddCutToFli(ap, call); }

  Term x(int i) {   ///< access to input argument i as a term

          CACHE_REGS

    return XREGS[i];
  }
  YAPTerm X(int i) {   ///<  access to input argument as a YAPTerm

          CACHE_REGS


    return YAPTerm(XREGS[i]); }

  bool output( Term t, int i) {   ///< unify term t with argumentr i

      CACHE_REGS
    return Yap_unify(XREGS[i], t);
  };
  bool output( YAPTerm t, int i) {
          CACHE_REGS

    return t.unify(XREGS[i]); };

  bool ensureStorage( size_t cells) {   ///< ensure at least cells cells are available

      CACHE_REGS

    return Yap_dogcl(cells*sizeof(CELL) PASS_REGS);
  };
  ///<  ts. YAPTerms do no require this.
  bool ensureStorage( size_t cells, std::vector<Term> ts) {      ///<  ensure memory but take care to first save the terms in ts
          CACHE_REGS

    return Yap_dogcl(cells*sizeof(CELL) PASS_REGS); };
};


#endif




/// @}
