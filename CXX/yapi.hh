
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
 * @{
 *
 */
#include <stdlib.h>

#include <config.h>

extern "C" {

#ifdef __cplusplus
#define old_cplusplus __cplusplus
#undef __cplusplus
#endif
#if USE_GMP
#include <gmp.h>
#endif
#ifdef old_cplusplus
#define __cplusplus old_cplusplus
#undef old_cplusplus
#endif

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
class YAPAtom;
class YAPFunctor;
class YAPApplTerm;
class YAPPairTerm;
class YAPQuery;

class TypeError {};

/**
 * @brief Generic Prolog Term
 */
class YAPTerm {
  friend class YAPPredicate;
  friend class YAPApplTerm;
  friend class YAPPairTerm;
  friend class YAPListTerm;
protected:
  handle_t t;
  void mk(Term t0);
  Term gt();
  YAPTerm(Term tn) {  mk( tn ); }
public:
  YAPTerm() { mk(TermNil); } // do nothing constructor
  YAPTerm(intptr_t i);
  YAPTerm(void *ptr);
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
class YAPVarTerm: public YAPTerm {
  YAPVarTerm(Term t) { if (IsVarTerm(t)) mk( t ); }
public:
  YAPVarTerm();
  CELL *getVar() { return VarOfTerm( gt() ); }
  bool unbound() { return IsUnboundVar(VarOfTerm( gt() )); }
};

/**
 * @brief Compound Term
 */
class YAPApplTerm: public YAPTerm {
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
class YAPPairTerm: public YAPTerm {
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

class YAPIntegerTerm: public YAPTerm {
public:
  YAPIntegerTerm(intptr_t i);
  intptr_t getInteger() { return  IntegerOfTerm( gt() ); }
  bool isTagged() { return IsIntTerm( gt() ); }
};

class YAPListTerm: public YAPTerm {
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
   */
  //	  YAPListTerm( vector<YAPTerm> v );
  /// Return the number of elements in a list term.
  size_t length() { Term *tailp; Term t1 = gt(); return Yap_SkipList(&t1, &tailp); }
  /// Extract the first element of a list.
  ///
  /// @param[in] the list
  YAPTerm car();
  /// Extract the tail elements of a list.
  ///
  /// @param[in] the list
  YAPListTerm cdr()
  {
    Term to = gt();
    if (IsPairTerm( to ))
      return YAPListTerm(TailOfTerm( to ));
    else
      return MkIntTerm(-1);
  }

  /// Check if the list is empty.
  ///
  /// @param[in] the list
  bool nil();
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
class YAPStringTerm: public YAPTerm {
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
class YAPAtomTerm: public YAPTerm {
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
 * @brief YAPFunctor represents Prolog functors Name/Arity
 */
class YAPFunctor {
  friend class YAPApplTerm;
  friend class YAPPredicate;
  Functor f;
  /// Constructor: receives Prolog functor and casts it to YAPFunctor
  ///
  /// Notice that this is designed for internal use only.
  YAPFunctor( Functor ff) { f = ff; }
public:
  /// Constructor: receives name as a string plus arity
  ///
  /// Notice that this is designed for ISO-LATIN-1 right now
  YAPFunctor( char * s, arity_t arity) { f = Yap_MkFunctor( Yap_LookupAtom( s ), arity ); }
  /// Constructor: receives name as a  wide string plus arity
  ///
  /// Notice that this is designed for UNICODE right now
  YAPFunctor( wchar_t * s, arity_t arity) { f = Yap_MkFunctor( Yap_LookupWideAtom( s ), arity ); }
  /// Constructor: receives name as an atom, plus arity
  ///
  /// This is the default method, and the most popi;at
  YAPFunctor( YAPAtom at, arity_t arity) { f = Yap_MkFunctor( at.a, arity ); }

  /// Getter: extract name of functor as an atom
  ///
  /// this is for external usage.
  YAPAtom name(void) {
  return YAPAtom( NameOfFunctor( f ) );
  }

  /// Getter: extract arity of functor as an unsigned integer
  ///
  /// this is for external usage.
  arity_t arity(void) {
    return ArityOfFunctor( f );
  }
};

/**
 * @brief Predicates
 *
 * This class interfaces with PredEntry in Yatom.g
 */
class YAPPredicate {
  friend class YAPQuery;

private:
  PredEntry *ap;

  /// auxiliary routine to find a predicate in the current module.
   PredEntry  *getPred( Term t, Term **outp ) ;

  /// String constructor for predicates
  ///
  /// It also communicates the array of arguments t[]  abd the array of variables
  /// back to yapquery
  YAPPredicate(const char *s, Term **outp, handle_t& vnames );

  /// Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(Term t) {
    ap = getPred( t , NULL );
  }

  /// Cast constructor for predicates,
  /// if we have the implementation data.
  ///
  inline YAPPredicate(PredEntry *pe) {
    ap = pe;
  }

public:

  /// Functor constructor for predicates
  ///
  /// Asssumes that we use the current module.
  YAPPredicate(YAPFunctor f);

  /// Functor constructor for predicates, is given a specific module.
  ///
  inline YAPPredicate(YAPFunctor f, YAPTerm mod) {
    ap = RepPredProp(PredPropByFunc(f.f,mod.t));
  }

  /// Name/arity constructor for predicates.
  ///
  inline YAPPredicate(YAPAtom at, YAPTerm mod) {
    ap = RepPredProp(PredPropByAtom(at.a,mod.t));
  }


  /// Name/0 constructor for predicates.
  ///
  YAPPredicate(YAPAtom at);

  /// Mod:Name/Arity constructor for predicates.
  ///
  inline YAPPredicate(YAPAtom at, arity_t arity, YAPTerm mod) {
    if (arity) {
	Functor f = Yap_MkFunctor(at.a, arity);
	ap = RepPredProp(PredPropByFunc(f,mod.t));
    } else {
	ap = RepPredProp(PredPropByAtom(at.a,mod.t));
    }
  }

  /// Atom/Arity constructor for predicates.
  ///
   YAPPredicate(YAPAtom at, arity_t arity);


  /// String constructor for predicates.
  ///
  /// String is a Prolog term, we extract the main functor after considering the module qualifiers.
  inline YAPPredicate(char *s) {
    Term t, tp;
    t = YAP_ReadBuffer(s,&tp);
    ap = getPred( t, NULL );
  }


  /// String constructor for predicates, also keeps arguments in tp[]
  ///
  /// String is a Prolog term, we extract the main functor after considering the module qualifiers.
 inline YAPPredicate(char *s, Term **outp) {
    Term t, tp;
    t = YAP_ReadBuffer(s,&tp);
    ap = getPred( t, NULL );
  }

 /// meta-call this predicate, with arguments ts[]
 ///
 int call(YAPTerm ts[]);

  /// module of a predicate
  ///
  /// notice that modules are currently treated as atoms, this should change.
  YAPAtom module() {
    if (ap->ModuleOfPred == PROLOG_MODULE)
      return  YAPAtom(AtomProlog);
    else
      return  YAPAtom(AtomOfTerm(ap->ModuleOfPred));
  }

  /// name of predicate
  ///
  /// notice that we return the atom, not a string.
  YAPAtom name() { if (ap->ArityOfPE)
    return  YAPAtom((Atom)ap->FunctorOfPred);
  else
    return  YAPAtom(NameOfFunctor(ap->FunctorOfPred));
  }

  /// arity of predicate
  ///
  /// we return a positive number.
  arity_t getArity() { return ap->ArityOfPE; }

};

/**
 * @brief Queries
 *
 * interface to a YAP Query;
 * uses an SWI-like status info internally.
 */
class YAPQuery: public YAPPredicate {
  int q_open;
  int q_state;
  Term *q_g;
  yamop *q_p, *q_cp;
  jmp_buf q_env;
  int q_flags;
  YAP_dogoalinfo q_h;
  YAPQuery *oq;
  handle_t vnames;
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
  inline bool first() { return next(); }
  /// ask for the next solution of the current query
  /// same call for every solution
  bool next();
  /// remove alternatives in the current search space, and finish the current query
  void cut();
  /// finish the current query: undo all bindings.
  void close();
  /// query variables.
  YAPListTerm namedVars();
};

// Java support

// This class implements 	a callback Prolog-side
class YAPCallback {
public:
      virtual ~YAPCallback() { printf("~YAPCallback\n"); }
      virtual void run() { __android_log_print(ANDROID_LOG_INFO, __FUNCTION__, "callback");  }
      virtual void displayInWindow(char *s) {  }
};

/**
 * @brief YAP Engine: takes care of constructing an execution environment where we can go executing goals
 *
 */
class YAPEngine {
private:
  YAPCallback *_callback;
  char **buf;
  YAP_init_args init_args;
public:
  YAPEngine(char *savedState = NULL,
            size_t stackSize = 0,
            size_t trailSize = 0,
            size_t maxStackSize = 0,
            size_t maxTrailSize = 0,
            char *libDir = NULL,
            char *bootFile = NULL,
            char *goal = NULL,
            char *topLevel = NULL,
            bool script = FALSE,
            bool fastBoot = FALSE,
	YAPCallback *callback=NULL);  /// construct a new engine, including aaccess to callbacks
  ~YAPEngine() { delYAPCallback(); } /// kill engine
  void delYAPCallback() { _callback = 0; } /// remove current callback
  void setYAPCallback(YAPCallback *cb) { delYAPCallback(); _callback = cb; __android_log_print(ANDROID_LOG_INFO, __FILE__, "after loading startup %p",cb); } /// set a new callback
  void call() { if (_callback) _callback->run(); } /// execute the callback.
  void display( char *s) { __android_log_print(ANDROID_LOG_INFO, __FUNCTION__, "bef calling disp %s %p",s, _callback);  if (_callback) _callback->displayInWindow(s); } /// execute the callback.
  YAPQuery *query( char *s ); /// build a query on the engine
};

/*
 * @}
 *
 */
