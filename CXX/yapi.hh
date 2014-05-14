
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

// we cannot consult YapInterface.h, that conflicts with what we declare, though
// it shouldn't
}

//#include <vector>

class YAPEngine;
class YAPTermHandle;
class YAPAtom;
class YAPFunctor;
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
	Term t;
public:
	YAPTerm() { t = TermNil; } // do nothing constructor
	YAPTerm(void *ptr)  { CACHE_REGS t = MkIntegerTerm( (Int)ptr ); }
	YAPTerm(Term tn) { t = tn; }
	YAPTerm(char *s) { Term tp ; t = YAP_ReadBuffer(s,&tp); }

	YAP_tag_t  tag();
	YAPTerm  deepCopy();
	bool exactlyEqual(YAPTerm t1);
	bool unify(YAPTerm t1);
	bool unifiable(YAPTerm t1);
	bool variant(YAPTerm t1);
	intptr_t hash(size_t sz, size_t depth, bool variant);
	bool isVar() { return IsVarTerm(t); }
	bool isAtom() { return IsAtomTerm(t); }
	bool isInteger() { return IsIntegerTerm(t); }
	bool isFloat() { return IsFloatTerm(t); }
	bool isCompound() { return !(IsVarTerm(t) || IsNumTerm(t)); }
	bool isAppl() { return IsApplTerm(t); }
	bool isPair() { return IsPairTerm(t); }
	bool isGround() { return Yap_IsGroundTerm(t); }
	bool isList() { return Yap_IsListTerm(t); }
	bool isString() { return IsStringTerm(t); }
};

/**
 * @brief Variable Term
 */
class YAPVarTerm: private YAPTerm {
public:
	YAPVarTerm(): YAPTerm() { CACHE_REGS t = MkVarTerm(); }
	CELL *getVar() { return VarOfTerm(t); }
	bool unbound() { return IsUnboundVar(VarOfTerm(t)); }
};

/**
 * @brief Compound Term
 */
class YAPApplTerm: private YAPTerm {
public:
	YAPApplTerm(YAPFunctor f, YAPTerm ts[]);
	YAPApplTerm(YAPFunctor f);
	YAPFunctor getFunctor();
	YAPTerm getArg(unsigned int i);
};

/**
 * @brief List Constructor Term
 */
class YAPPairTerm: private YAPTerm {
public:
	YAPPairTerm(YAPTerm hd, YAPTerm tl);
	YAPPairTerm();
	YAPTerm getHead() { return  YAPTerm(HeadOfTerm(t)); }
	YAPTerm getTail() { return  YAPTerm(TailOfTerm(t)); }
};

/**
 * @brief Integer Term
 */

class YAPIntegerTerm: private YAPTerm {
public:
	YAPIntegerTerm(intptr_t i) { CACHE_REGS t = MkIntegerTerm( i ); }
	intptr_t getInteger(YAPIntegerTerm t) { return  IntegerOfTerm(t.t); }
	bool isTagged(YAPIntegerTerm i) { return IsIntTerm( t ); }
};

/*
class YAPListTerm: private YAPPairTerm {
public:
	  YAPListTerm(YAPTerm ts[], size_t n);
	  YAPListTerm(Term ts[], size_t n);
	  YAPListTerm( vector<YAPTerm> v );
	  size_t length() { Term *tailp; return Yap_SkipList(&t, &tailp); }
	  vector<YAPTerm> toVector();
};
 */

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
	const char *getString() { return StringOfTerm(t); }
};

/**
 * @brief Atom Term
 */
class YAPAtomTerm: private YAPTerm {
public:
	YAPAtomTerm(YAPAtom a): YAPTerm() { t = MkAtomTerm(a.a); }
	YAPAtomTerm(Atom a): YAPTerm()  { t = MkAtomTerm(a); }
	YAPAtomTerm(char *s) ;
	YAPAtomTerm(char *s, size_t len);
	YAPAtomTerm(wchar_t *s) ;
	YAPAtomTerm(wchar_t *s, size_t len);
	YAPAtom getAtom() { return YAPAtom(AtomOfTerm(t)); }
};

/**
 * @brief Functor
 */
class YAPFunctor {
	friend class YAPApplTerm;
	friend class YAPPredicate;
	Functor f;
public:
	YAPFunctor( char * s, unsigned int arity) { f = Yap_MkFunctor( Yap_LookupAtom( s ), arity ); }
	YAPFunctor( wchar_t * s, unsigned int arity) { f = Yap_MkFunctor( Yap_LookupWideAtom( s ), arity ); }
	YAPFunctor( YAPAtom at, unsigned int arity) { f = Yap_MkFunctor( at.a, arity ); }
	YAPFunctor( Functor ff) { f = ff; }

	Atom name(void) {
		return NameOfFunctor( f );
	}

	unsigned int arity(void) {
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
	~YAPTermHandle(void) {
		CACHE_REGS
		Yap_RecoverSlots(1 PASS_REGS);
	}

	YAPTerm get() {
		CACHE_REGS
		return new YAPTerm( Yap_GetFromSlot(handle PASS_REGS) );
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
	YAPPredicate(char *s, Term **th) {
		CACHE_REGS
		Term t, tp, m = CurrentModule ;
		t = YAP_ReadBuffer(s,&tp);
		t = Yap_StripModule(t, &m);
		if (IsVarTerm(t) || IsNumTerm(t))
			ap = NULL;
		if (IsAtomTerm(t)) {
			ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
			*th = NULL;
		} else if (IsApplTerm(t)) {
			ap = RepPredProp(PredPropByFunc(FunctorOfTerm(t), m));
			*th = RepAppl(t)+1;
		} else {
			ap = NULL;
		}
	}
public:
	YAPPredicate(PredEntry *pe) {
		ap = pe;
	}
	YAPPredicate(YAPFunctor f) {
		CACHE_REGS
		ap = RepPredProp(PredPropByFunc(f.f,CurrentModule));
	}
	YAPPredicate(YAPFunctor f, YAPTerm mod) {
		ap = RepPredProp(PredPropByFunc(f.f,mod.t));
	}
	YAPPredicate(YAPAtom at, YAPTerm mod) {
		ap = RepPredProp(PredPropByAtom(at.a,mod.t));
	}
	YAPPredicate(YAPAtom at) {
		CACHE_REGS
		ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
	}
	YAPPredicate(YAPAtom at, unsigned int arity, YAPTerm mod) {
		if (arity) {
			Functor f = Yap_MkFunctor(at.a, arity);
			ap = RepPredProp(PredPropByFunc(f,mod.t));
		} else {
			ap = RepPredProp(PredPropByAtom(at.a,mod.t));
		}
	}
	YAPPredicate(YAPAtom at, unsigned int arity) {
		CACHE_REGS
		if (arity) {
			Functor f = Yap_MkFunctor(at.a, arity);
			ap = RepPredProp(PredPropByFunc(f,CurrentModule));
		} else {
			ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
		}
	}
	YAPPredicate(char *s) {
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
	int call(YAPTerm ts[]);
};

/**
 * @brief Term Handle
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
	void initQuery( Term *t );
public:
	/// full constructor, is given a functor, module, and an array of terms that must hav at least
	/// the same arity as the functor.
	YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm t[]);
	YAPQuery(YAPFunctor f, YAPTerm t[]);
	YAPQuery(YAPPredicate p, YAPTerm t[]);
	YAPQuery(char *s);
	//  YAPQuery(YAPTerm t);
	int next();
	void cut();
	void close();
};

class YAPParams;

/**
 * @brief YAP Constructor
 *
 */
class YAPEngine {
public:
  YAPEngine(YAPParams const& params);  /// construct a new engine
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

