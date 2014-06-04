
#define YAP_CPP_INTERFACE 1

/**
 *
 *   @defgroup yap-cplus-interface An object oriented interface for YAP.
 *
 *
 *    @tableofcontents
 *
 *
 * C++ interface to YAP. Designed to be object oriented and to fit naturally
 * with the swig interface language generator. It uses ideas from the old YAP
 * interface and from the SWI foreign language interface.
 *
 */
#include <stdlib.h>

#include <config.h>

#if USE_GMP
#include <gmp.h>
#endif

extern "C" {

#include "Yap.h"

#include "Yatom.h"

#include "YapHeap.h"

#include "pl-shared.h"

#include "clause.h"

#include "yapio.h"

#include "Foreign.h"

#include "attvar.h"

#include "SWI-Stream.h"

#include "YapText.h"

#if HAVE_STDARG_H
#include <stdarg.h>
#endif

#if HAVE_STDINT_H
#include <stdint.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#endif

#if _MSC_VER || defined(__MINGW32__) 
#include <windows.h>
#endif

// taken from yap_structs.h
#include "iopreds.h"

extern Term Yap_StringToTerm(const char *s, size_t len, term_t bindings);

// we cannot consult YapInterface.h, that conflicts with what we declare, though
// it shouldn't
}

//#include <vector>

class YAPEngine;
class YAPTermHandle;
class YAPAtom;
class YAPFunctor;
class YAPApplTerm;
class YAPPairTerm;
class YAPQuery;

/**
 * @brief Generic Prolog Term
 */
class YAPTerm {
	friend class YAPPredicate;
	friend class YAPTermHandle;
	friend class YAPApplTerm;
	friend class YAPPairTerm;
protected:
	term_t t;
	inline void mk(Term t0) { CACHE_REGS t = Yap_InitSlot( t0 PASS_REGS);  }
	inline Term gt(void) { CACHE_REGS return Yap_GetFromSlot( t PASS_REGS); }
public:
	YAPTerm() { mk(TermNil); } // do nothing constructor
	YAPTerm(void *ptr)  { CACHE_REGS mk( MkIntegerTerm( (Int)ptr )  );}
	YAPTerm(Term tn) {  mk( tn ); }
	YAPTerm(char *s) { Term tp ; mk( YAP_ReadBuffer(s,&tp) );  }
	/*~YAPTerm(void) {
		CACHE_REGS
		Yap_RecoverSlots(1, t PASS_REGS);
	}*/
	Term term() { return gt(); }
	YAP_tag_t  tag();
	YAPTerm  deepCopy();
	//const YAPTerm *vars();
	bool exactlyEqual(YAPTerm t1);
	bool unify(YAPTerm t1);
	bool unifiable(YAPTerm t1);
	bool variant(YAPTerm t1);
	intptr_t hash(size_t sz, size_t depth, bool variant);
	bool isVar() { return IsVarTerm( gt() ); }
	bool isAtom() { return IsAtomTerm( gt() ); }
	bool isInteger() { return IsIntegerTerm( gt() ); }
	bool isFloat() { return IsFloatTerm( gt() ); }
	bool isCompound() { return !(IsVarTerm( gt() ) || IsNumTerm( gt() )); }
	bool isAppl() { return IsApplTerm( gt() ); }
	bool isPair() { return IsPairTerm( gt() ); }
	bool isGround() { return Yap_IsGroundTerm( gt() ); }
	bool isList() { return Yap_IsListTerm( gt() ); }
	bool isString() { return IsStringTerm( gt() ); }

	inline YAPTerm getArg(int i) {
	  Term t0 = gt();
	  if (IsApplTerm(t0))
	    return YAPTerm(ArgOfTerm(i, t0));
	  else if (IsPairTerm(t0)) {
	      if (i==1)
		return YAPTerm(HeadOfTerm(t0));
	      if (i==2)
		return YAPTerm(TailOfTerm(t0));
	  }
	  return YAPTerm((Term)0);
	}
	char *text();
};

/**
 * @brief Variable Term
 */
class YAPVarTerm: private YAPTerm {
	YAPVarTerm(Term t) { if (IsVarTerm(t)) mk( t ); }
public:
	YAPVarTerm() { CACHE_REGS mk( MkVarTerm( ) ); }
	CELL *getVar() { return VarOfTerm( gt() ); }
	bool unbound() { return IsUnboundVar(VarOfTerm( gt() )); }
};

/**
 * @brief Compound Term
 */
class YAPApplTerm: private YAPTerm {
  friend class YAPTerm;
  YAPApplTerm(Term t0) { mk(t0); }
public:
  YAPApplTerm(YAPTerm t0) { mk(t0.term()); }
	YAPApplTerm(YAPFunctor f, YAPTerm ts[]);
	YAPApplTerm(YAPFunctor f);
	YAPFunctor getFunctor();
	YAPTerm getArg(int i);
};

/**
 * @brief List Constructor Term
 */
class YAPPairTerm: private YAPTerm {
  friend class YAPTerm;
	YAPPairTerm(Term t0) { if (IsPairTerm(t0)) mk( t0 );  else mk(0); }
public:
	YAPPairTerm(YAPTerm hd, YAPTerm tl);
	YAPPairTerm();
	YAPTerm getHead() { return  YAPTerm(HeadOfTerm( gt() )); }
	YAPTerm getTail() { return  YAPTerm(TailOfTerm( gt() )); }
};

/**
 * @brief Integer Term
 */

class YAPIntegerTerm: private YAPTerm {
public:
	YAPIntegerTerm(intptr_t i) { CACHE_REGS Term tn = MkIntegerTerm( i ); mk( tn ); }
	intptr_t getInteger() { return  IntegerOfTerm( gt() ); }
	bool isTagged() { return IsIntTerm( gt() ); }
};

class YAPListTerm: private YAPTerm {
public:
  /// Create a list term out of a standard term. Check if a valid operation.
   ///
   /// @param[in] the term
    YAPListTerm(Term t0) { mk(t0); /* else type_error */ }
/*  /// Create a list term out of an array of terms.
  ///
  /// @param[in] the array of terms
  /// @param[in] the length of the array
  YAPListTerm(YAPTerm ts[], size_t n);
  /// Create a list term out of an array of Prolog terms.
  ///
  /// @param[in] the array of terms
  /// @param[in] the length of the array
  YAPListTerm(Term ts[], size_t n);
  */
//	  YAPListTerm( vector<YAPTerm> v );
  /// Return the number of elements in a list term.
  size_t length() { Term *tailp; Term t1 = gt(); return Yap_SkipList(&t1, &tailp); }
//	  vector<YAPTerm> toVector();
  /// Create an array of term out of a list.
   ///
   /// @param[in] the list
  YAPTerm car() { if (IsPairTerm(gt())) return YAPTerm(HeadOfTerm(gt())); else return YAPTerm((term_t)0); }
  YAPListTerm cdr() { if (IsPairTerm(gt())) return YAPListTerm(TailOfTerm(gt()));  else return YAPListTerm((term_t)0);  }
  bool nil() { return gt() == TermNil; }
};

/**
 * @brief Atom
 */
class YAPAtom {
	friend class YAPPredicate;
	friend class YAPFunctor;
	friend class YAPAtomTerm;
	Atom a;
public:
	YAPAtom( Atom at ) { a = at; }
	YAPAtom( char * s) { a = Yap_LookupAtom( s ); }
	YAPAtom( wchar_t * s) { a = Yap_LookupMaybeWideAtom( s ); }
	YAPAtom( char * s, size_t len) { a = Yap_LookupAtomWithLength( s, len ); }
	YAPAtom( wchar_t * s, size_t len) { a = Yap_LookupMaybeWideAtomWithLength( s, len ); }
	char *name(void);
};

/**
 * @brief String Term
 */
class YAPStringTerm: private YAPTerm {
public:
	YAPStringTerm(char *s) ;
	YAPStringTerm(char *s, size_t len);
	YAPStringTerm(wchar_t *s) ;
	YAPStringTerm(wchar_t *s, size_t len);
	const char *getString() { return StringOfTerm( gt() ); }
};

/**
 * @brief Atom Term
 */
class YAPAtomTerm: private YAPTerm {
public:
	YAPAtomTerm(YAPAtom a): YAPTerm() { mk( MkAtomTerm(a.a) ); }
	YAPAtomTerm(Atom a): YAPTerm()  { mk( MkAtomTerm(a) ); }
	YAPAtomTerm(char *s) ;
	YAPAtomTerm(char *s, size_t len);
	YAPAtomTerm(wchar_t *s) ;
	YAPAtomTerm(wchar_t *s, size_t len);
	YAPAtom getAtom() { return YAPAtom(AtomOfTerm( gt() )); }
};

/**
 * @brief Functor
 */
class YAPFunctor {
	friend class YAPApplTerm;
	friend class YAPPredicate;
	Functor f;
public:
	YAPFunctor( char * s, arity_t arity) { f = Yap_MkFunctor( Yap_LookupAtom( s ), arity ); }
	YAPFunctor( wchar_t * s, arity_t arity) { f = Yap_MkFunctor( Yap_LookupWideAtom( s ), arity ); }
	YAPFunctor( YAPAtom at, arity_t arity) { f = Yap_MkFunctor( at.a, arity ); }
	YAPFunctor( Functor ff) { f = ff; }

	Atom name(void) {
		return NameOfFunctor( f );
	}

	arity_t arity(void) {
		return ArityOfFunctor( f );
	}
};

/**
 * @brief Term Handle
 */
class YAPTermHandle {
	long int handle;
public:
	YAPTermHandle(Term t) {
		CACHE_REGS
		handle = Yap_InitSlot(t PASS_REGS);
	}

	void set(YAPTerm t) {
		CACHE_REGS
		Yap_PutInSlot(handle, t.t PASS_REGS);
	}
};

/**
 * @brief Predicate
 */
class YAPPredicate {
	friend class YAPQuery;
	PredEntry *ap;
	// trick to communicate t[] back to yapquery
	YAPPredicate(const char *s, Term **outp, term_t& vnames );
	inline YAPPredicate(Term t) {
		CACHE_REGS
		Term m = CurrentModule ;
		t = Yap_StripModule(t, &m);
		if (IsVarTerm(t) || IsNumTerm(t))
			ap = NULL;
		if (IsAtomTerm(t)) {
			ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
		} else if (IsApplTerm(t)) {
			ap = RepPredProp(PredPropByFunc(FunctorOfTerm(t), m));
		} else if (IsPairTerm(t)) {
			ap = RepPredProp(PredPropByFunc(FunctorCsult, PROLOG_MODULE));
		} else {
			ap = NULL;
		}
	}
	inline YAPPredicate(PredEntry *pe) {
		ap = pe;
	}
public:
	inline YAPPredicate(YAPFunctor f) {
		CACHE_REGS
		ap = RepPredProp(PredPropByFunc(f.f,CurrentModule));
	}
	inline YAPPredicate(YAPFunctor f, YAPTerm mod) {
		ap = RepPredProp(PredPropByFunc(f.f,mod.t));
	}
	inline YAPPredicate(YAPAtom at, YAPTerm mod) {
		ap = RepPredProp(PredPropByAtom(at.a,mod.t));
	}
	inline YAPPredicate(YAPAtom at) {
		CACHE_REGS
		ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
	}
	inline YAPPredicate(YAPAtom at, arity_t arity, YAPTerm mod) {
		if (arity) {
			Functor f = Yap_MkFunctor(at.a, arity);
			ap = RepPredProp(PredPropByFunc(f,mod.t));
		} else {
			ap = RepPredProp(PredPropByAtom(at.a,mod.t));
		}
	}
	inline YAPPredicate(YAPAtom at, arity_t arity) {
		CACHE_REGS
		if (arity) {
			Functor f = Yap_MkFunctor(at.a, arity);
			ap = RepPredProp(PredPropByFunc(f,CurrentModule));
		} else {
			ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
		}
	}
	inline YAPPredicate(char *s) {
		CACHE_REGS
		Term t, tp, m = CurrentModule ;
		t = YAP_ReadBuffer(s,&tp);
		t = Yap_StripModule(t, &m);
		if (IsVarTerm(t) || IsNumTerm(t))
			ap = NULL;
		if (IsAtomTerm(t)) {
			ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
		} else {
			ap = RepPredProp(PredPropByFunc(FunctorOfTerm(t), m));
		}
	}
	inline YAPPredicate(char *s, Term **outp) {
		CACHE_REGS
		Term t, tp, m = CurrentModule ;
		t = YAP_ReadBuffer(s,&tp);
		t = Yap_StripModule(t, &m);
		if (IsVarTerm(t) || IsNumTerm(t))
			ap = NULL;
		if (IsAtomTerm(t)) {
			ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
			*outp = NULL;
		} else if (IsApplTerm(t)) {
			ap = RepPredProp(PredPropByFunc(FunctorOfTerm(t), m));
			*outp = RepAppl(t)+1;
		} else if (IsPairTerm(t)) {
			ap = RepPredProp(PredPropByFunc(FunctorOfTerm(t), m));
			*outp = RepPair(t);
		}
	}
	int call(YAPTerm ts[]);
	arity_t arity() { return ap->ArityOfPE; }
	/// module of predicate
	///
	/// notice that modules are currently treated as atoms, this should change.
	YAPAtom module() { if (ap->ModuleOfPred == PROLOG_MODULE)
	  return  YAPAtom(AtomProlog);
	else
	  return  YAPAtom(AtomOfTerm(ap->ModuleOfPred)); }
	/// name of predicate
	///
	/// notice that we return the atom, not a string.
	YAPAtom name() { if (ap->ArityOfPE)
	    return  YAPAtom((Atom)ap->FunctorOfPred);
	  else
	    return  YAPAtom(NameOfFunctor(ap->FunctorOfPred));
	}
};

/**
 * @brief Queries
 *
 * interface to a YAP Query;
 * uses an SWI-like status info internally.
 */
class YAPQuery: private YAPPredicate {
        int q_open;
        int q_state;
        Term *q_g;
        yamop *q_p, *q_cp;
        jmp_buf q_env;
        int q_flags;
        YAP_dogoalinfo q_h;
        YAPQuery *oq;
        term_t vnames;
        void initQuery( Term ts[] );
        void initQuery( YAPTerm t[], arity_t arity  );
public:
        /// main constructor, uses a predicate and an array of terms
        ///
        /// It is given a YAPPredicate _p_ , and an array of terms that must have at least
        /// the same arity as the functor.
        YAPQuery(YAPPredicate p, YAPTerm t[]);
        /// full constructor,
        ///
        /// It is given a functor, module, and an array of terms that must have at least
        /// the same arity as the functor.
        YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm t[]);
        /// functor/term constructor,
        ///
        /// It is given a functor, and an array of terms that must have at least
        /// the same arity as the functor. Works within the current module.
        YAPQuery(YAPFunctor f, YAPTerm t[]);
        /// string constructor with varnames
        ///
        /// It is given a string, calls the parser and obtains a Prolog term that should be a callable
        /// goal and a list of variables. Useful for top-level simulation. Works within the current module.
        inline YAPQuery(char *s): YAPPredicate(s, &this->q_g, vnames)
        {
                Term *ts = this->q_g;

                initQuery( ts );
        }
        ///  first query
        ///
        /// actually implemented by calling the next();
        inline int first() { return next(); }
        /// ask for the next solution of the current query
        /// same call for every solution
        int next();
        /// remove alternatives in the current search space, and finish the current query
        void cut();
        /// finish the current query: undo all bindings.
        void close();
        /// query variables.
	  YAPListTerm namedVars() {  CACHE_REGS Term o = Yap_GetFromSlot( vnames PASS_REGS ); return YAPListTerm( o ); }
  };


class YAPParams;

/**
 * @brief YAP Constructor
 *
 */
class YAPEngine {
public:
  YAPEngine(YAPParams const& params);  /// construct a new engine
  YAPEngine();  /// construct a new engine, no arguments
  YAPQuery *query( char *s ) { return new YAPQuery( s ); } /// build a query on the engine
};

/**
 * @brief Parameters for YAP Constructor
 *
 */
class YAPParams {
	friend class YAPEngine;
	YAP_init_args init_args;
public:
	YAPParams();
	// sets all the default values for each data member
	YAPParams& savedState( char * f);
	YAPParams& stackSize(size_t sz);
	YAPParams& trailSize(size_t sz);
	YAPParams& maxStackSize(size_t sz);
	YAPParams& maxTrailSize(size_t sz);
	YAPParams& libDir(char *p);
	YAPParams& bootFile(char *f);
	YAPParams& goal(char *g);
	YAPParams& topLevel(char *g);
	YAPParams& script(bool v);
	YAPParams& fastBoot(bool v);
};

inline YAPParams::YAPParams()
{ Yap_InitDefaults( &init_args, NULL ); }

inline YAPParams& YAPParams::savedState( char * f)
{ init_args.SavedState = f; return *this; }

inline YAPParams& YAPParams::stackSize(size_t sz)
{ init_args.StackSize = sz; return *this; }

inline YAPParams& YAPParams::trailSize(size_t sz)
{ init_args.TrailSize = sz; return *this; }

inline YAPParams& YAPParams::maxStackSize(size_t sz)
{ init_args.MaxStackSize = sz; return *this; }

inline YAPParams& YAPParams::maxTrailSize(size_t sz)
{ init_args.MaxTrailSize = sz; return *this; }

inline YAPParams& YAPParams::libDir(char *p)
{ init_args.YapLibDir = p; return *this; }

inline YAPParams& YAPParams::bootFile(char *f)
{ init_args.YapPrologBootFile = f; return *this; }

inline YAPParams& YAPParams::goal(char *g)
{ init_args.YapPrologGoal = g; return *this; }

inline YAPParams& YAPParams::topLevel(char *g)
{ init_args.YapPrologTopLevelGoal = g; return *this; }

inline YAPParams& YAPParams::script(bool v)
{ init_args.HaltAfterConsult = v; return *this; }

inline YAPParams& YAPParams::fastBoot(bool v)
{ init_args.FastBoot = v; return *this; }

