
#define YAP_CPP_INTERFACE 1

/**
 *
 *   @defgroup yap-cplus-interface An object oriented interface for YAP.
 *
 *   @ingroup ChYInterface
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

// Bad export from Python
#ifdef HAVE_STAT
#undef HAVE_STAT
#endif
#include <config.h>

extern "C" {

#include <stddef.h>
#ifdef __cplusplus
#define old_cplusplus __cplusplus
#undef __cplusplus
#endif
#if USE_GMP
#include <gmp.h>
#endif
#ifdef  old_cplusplus
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

#include "yapie.hh"

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

/**
 * @brief Generic Prolog Term
 */
class YAPTerm {
  friend class YAPPredicate;
  friend class YAPApplTerm;
  friend class YAPPairTerm;
  friend class YAPListTerm;
  friend class YAPQuery;
protected:
  yhandle_t t; /// handle to term, equivalent to term_t
  void mk(Term t0); /// internal method to convert from term to handle
  Term gt(); /// get handle and obtain term
  YAPTerm(Term tn) {  mk( tn ); } /// private method to convert from Term (internal YAP representation) to YAPTerm
  inline Term term() { return gt(); } /// private method to convert from YAPTerm to Term (internal YAP representation)
public:
  // do nothing constructor
  YAPTerm() { mk(TermNil); }
  /// integer to term
  YAPTerm(intptr_t i);
  /// pointer to term
  YAPTerm(void *ptr);
  /// parse string s and construct a term.
  YAPTerm(char *s) { Term tp ; mk( YAP_ReadBuffer(s,&tp) );  }
  /// extract the tag of a term, after dereferencing.
  YAP_tag_t  tag();
  /// copy the term ( term copy )
  YAPTerm  deepCopy();
  //const YAPTerm *vars();
  /// this term is == to t1
  bool exactlyEqual(YAPTerm t1);
  bool unify(YAPTerm t1);     /// t = t1
  bool unifiable(YAPTerm t1);  /// we can unify t and t1
  bool variant(YAPTerm t1);   /// t =@= t1, the two terms are equal up to variable renaming
  intptr_t hashTerm(size_t sz, size_t depth, bool variant); /// term hash,
  bool isVar() { return IsVarTerm( gt() ); }   /// type check for unbound
  bool isAtom() { return IsAtomTerm( gt() ); } ///  type check for atom
  bool isInteger() { return IsIntegerTerm( gt() ); } /// type check for integer
  bool isFloat() { return IsFloatTerm( gt() ); } /// type check for floating-point
  bool isString() { return IsStringTerm( gt() ); } /// type check for a string " ... "
  bool isCompound() { return !(IsVarTerm( gt() ) || IsNumTerm( gt() )); } /// is a primitive term
  bool isAppl() { return IsApplTerm( gt() ); } /// is a structured term
  bool isPair() { return IsPairTerm( gt() ); } /// is a pair term
  bool isGround() { return Yap_IsGroundTerm( gt() ); } /// term is ground
  bool isList() { return Yap_IsListTerm( gt() ); } /// term is a list

  /// extract the argument i of the term, where i in 1...arity
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

  /// return a string with a textual representation of the term
  char *text();
};

/**
 * @brief Variable Term
 */
class YAPVarTerm: public YAPTerm {
  YAPVarTerm(Term t) { if (IsVarTerm(t)) mk( t ); }
public:
  /// constructor
  YAPVarTerm();
  /// get the internal representation
  CELL *getVar() { return VarOfTerm( gt() ); }
  /// is the variable bound to another one
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
  /// construct new YAPAtom from Atom
  YAPAtom( Atom at ) { a = at; }
public:
  /// construct new YAPAtom from string
  YAPAtom( char * s) { a = Yap_LookupAtom( s ); }
  /// construct new YAPAtom from wide string
  YAPAtom( wchar_t * s) { a = Yap_LookupMaybeWideAtom( s ); }
  /// construct new YAPAtom from max-length string
  YAPAtom( char * s, size_t len) { a = Yap_LookupAtomWithLength( s, len ); }
  /// construct new YAPAtom from max-length wide string
  YAPAtom( wchar_t * s, size_t len) { a = Yap_LookupMaybeWideAtomWithLength( s, len ); }
  /// get name of atom
  char *getName(void);
};

/**
 * @brief String Term
 */
class YAPStringTerm: public YAPTerm {
public:
  /// your standard constructor
  YAPStringTerm(char *s) ;
  /// use this one to construct length limited strings
  YAPStringTerm(char *s, size_t len);
  /// construct using wide chars
  YAPStringTerm(wchar_t *s) ;
  /// construct using length-limited wide chars
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
   PredEntry  *getPred( Term t, Term* &outp ) ;

  /// String constructor for predicates
  ///
  /// It also communicates the array of arguments t[]  abd the array of variables
  /// back to yapquery
  YAPPredicate(const char *s, Term* &outp, YAPTerm& vnames ) throw (int);

  /// Term constructor for predicates
  ///
  /// It is just a call to getPred
  inline YAPPredicate(Term t) {
	CELL *  v = NULL;
    ap = getPred( t , v );
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
  inline YAPPredicate(char *s) throw (int) {
    Term t, tp;
    t = YAP_ReadBuffer(s,&tp);
    if (t == 0L)
      throw YAPError::YAP_SYNTAX_ERROR;
	CELL *  v;
    ap = getPred( t, v);
  }


  /// String constructor for predicates, also keeps arguments in tp[]
  ///
  /// String is a Prolog term, we extract the main functor after considering the module qualifiers.
 inline YAPPredicate(char *s, Term* &outp) throw (int) {
    Term t, tp;
    t = YAP_ReadBuffer(s,&tp);
    if (t == 0L)
      throw YAPError::YAP_SYNTAX_ERROR;
    ap = getPred( t, outp );
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
  YAPTerm vnames;
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
  /// string constructor without varnames
  ///
  /// It is given a string, calls the parser and obtains a Prolog term that should be a callable
  /// goal. It does not ask for a list of variables.
  inline YAPQuery(char *s): YAPPredicate(s, q_g)
  {
    initQuery( q_g );
  }
  /// string constructor with varnames
  ///
  /// It is given a string, calls the parser and obtains a Prolog term that should be a callable
  /// goal and a list of variables. Useful for top-level simulation. Works within the current module.
  inline YAPQuery(char *s, YAPTerm &vnames): YAPPredicate(s, q_g, vnames)
  {
    initQuery( q_g );
  }
  /// set flags for query execution, currently only for exception handling
  void setFlag(int flag) {q_flags |= flag; }
  /// reset flags for query execution, currently only for exception handling
  void resetFlag(int flag) {q_flags &= ~flag; }
  ///  first query
  ///
  /// actually implemented by calling the next();
  inline bool first() { return next(); }
  /// ask for the next solution of the current query
  /// same call for every solution
  bool next();
  /// represent the top-goal
  char *text();
  /// remove alternatives in the current search space, and finish the current query
  void cut();
  /// finish the current query: undo all bindings.
  void close();
  /// query variables.
  YAPListTerm namedVars();
};

// Java support

/// This class implements a callback Prolog-side. It will be inherited by the Java or Python
/// class that actually implements the callback.
class YAPCallback {
public:
      virtual ~YAPCallback() { printf("~YAPCallback\n"); }
      virtual void run() { __android_log_print(ANDROID_LOG_INFO, __FUNCTION__, "callback");  }
      virtual void run(char *s) {  }
};

/**
 * @brief YAP Engine: takes care of the execution environment
   where we can go executing goals.
 *
 *
 */
class YAPEngine {
private:
  YAPCallback *_callback;
  YAP_init_args init_args;
  YAPError yerror;
public:
  YAPEngine(char *savedState = (char *)NULL,
            size_t stackSize = 0,
            size_t trailSize = 0,
            size_t maxStackSize = 0,
            size_t maxTrailSize = 0,
            char *libDir = (char *)NULL,
            char *bootFile = (char *)NULL,
            char *goal = (char *)NULL,
            char *topLevel = (char *)NULL,
            bool script = FALSE,
            bool fastBoot = FALSE,
            YAPCallback *callback=(YAPCallback *)NULL);  /// construct a new engine, including aaccess to callbacks
  /// kill engine
  ~YAPEngine() { delYAPCallback(); }
  /// remove current callback
  void delYAPCallback() { _callback = 0; }
  /// set a new callback
  void setYAPCallback(YAPCallback *cb) { delYAPCallback(); _callback = cb; }
  /// execute the callback.
  void run() { if (_callback) _callback->run(); }
  /// execute the callback with a text argument.
  void run( char *s) {  if (_callback) _callback->run(s); }
  /// execute the callback with a text argument.
  YAPError hasError( ) {  return yerror; }
  /// build a query on the engine
  YAPQuery *query( char *s );
};

/**
 * @}
 *
 */
