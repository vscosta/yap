#ifndef YAPT_HH
#define YAPT_HH 1

/**
 * @brief Generic Prolog Term
 */
class YAPTerm {
  friend class YAPPredicate;
  friend class YAPPrologPredicate;
  friend class YAPQuery;
  friend class YAPModule;
  friend class YAPModuleProp;
protected:
  yhandle_t t; /// handle to term, equivalent to term_t
  void mk(Term t0); /// internal method to convert from term to handle
  Term gt(); /// get handle and obtain term
public:
  ~YAPTerm() {};
  YAPTerm(Term tn) {  mk( tn ); } /// private method to convert from Term (internal YAP representation) to YAPTerm
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
  inline Term term() { return gt(); } /// private method to convert from YAPTerm to Term (internal YAP representation)
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
class YAPVarTerm: private YAPTerm {
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
class YAPApplTerm: YAPTerm {
  friend class YAPTerm;
  YAPApplTerm(Term t0) { mk(t0); }
public:
  ~YAPApplTerm() {  }
  YAPApplTerm(YAPFunctor f, YAPTerm ts[]);
  YAPApplTerm(YAPFunctor f);
  YAPFunctor getFunctor();
  YAPTerm getArg(int i);
};

/**
 * @brief List Constructor Term
 */
class YAPPairTerm: YAPTerm {
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
  YAPIntegerTerm(intptr_t i);
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
  */
  //      YAPListTerm( vector<YAPTerm> v );
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
 * @brief String Term
 */
class YAPStringTerm: private YAPTerm {
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
 * Term Representation of an Atom
 */
class YAPAtomTerm: YAPTerm {
  friend class YAPModule;
  // Constructor: receives a C-atom;
  YAPAtomTerm(Atom a)  { mk( MkAtomTerm(a) ); }
  YAPAtomTerm(Term t): YAPTerm(t)  { IsAtomTerm(t); }
  // Getter for Prolog atom
  Term getTerm() { return t; }
public:
  // Constructor: receives an atom;
  YAPAtomTerm(YAPAtom a): YAPTerm() { mk( MkAtomTerm(a.a) ); }
  // Constructor: receives a sequence of ISO-LATIN1 codes;  
  YAPAtomTerm(char *s) ;
  // Constructor: receives a sequence of up to n ISO-LATIN1 codes;  
  YAPAtomTerm(char *s, size_t len);
  // Constructor: receives a sequence of wchar_ts, whatever they may be;  
  YAPAtomTerm(wchar_t *s) ;
  // Constructor: receives a sequence of n wchar_ts, whatever they may be;  
  YAPAtomTerm(wchar_t *s, size_t len);
  // Getter: outputs the atom;  
  YAPAtom getAtom() { return YAPAtom(AtomOfTerm( gt() )); }
  // Getter: outputs the name as a sequence of ISO-LATIN1 codes;  
  const char *getName() { return AtomOfTerm( gt() )->StrOfAE; }
};

#endif /* YAPT_HH */
