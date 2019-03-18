/**
 * @file yapt.hh
 */

#ifndef X_API
#define X_API
#endif

/**
 *   @defgroup yap-cplus-term-handling Term Handling in the YAP interface.
 *
 *   @{
 *
 *   @ingroup yap-cplus-interface
 *   @tableofcontents
 *
 *
 * These classes offer term construction and access. Terms are seens
 * as objects that inherit from a virtual class, Currently, all
 * terms must reside in the stack and may be moved around during
 * garbage collection. Term objects use an handle, in the SWI-Prolog style.
 *
 * Notice that terms  are forcefully destroyed during backtracking.
 *
 */

#include <vector>

#ifndef YAPT_HH
#define YAPT_HH 1

class YAPError;

extern "C" {

X_API extern Term YAP_MkCharPTerm(char *n);
}

/**
 * @brief Generic Prolog Term
 */
class X_API YAPTerm {
  friend class YAPPredicate;
  friend class YAPPrologPredicate;
  friend class YAPQuery;
  friend class YAPModule;
  friend class YAPModuleProp;
  friend class YAPApplTerm;
  friend class YAPListTerm;

protected:
  yhandle_t t; /// handle to term, equivalent to term_t

public:
  Term gt() {
    CACHE_REGS
    // fprintf(stderr,"?%d,%lx,%p\n",t,LOCAL_HandleBase[t], HR);
    // Yap_DebugPlWriteln(LOCAL_HandleBase[t]);
    return Yap_GetFromSlot(t);
  };

  void mk(Term t0) {
    CACHE_REGS t = Yap_InitSlot(t0);
    // fprintf(stderr,"+%d,%lx,%p,%p",t,t0,HR,ASP); Yap_DebugPlWriteln(t0);
  };

  void put(Term t0) {
    Yap_PutInHandle(t, t0);
    // fprintf(stderr,"+%d,%lx,%p,%p",t,t0,HR,ASP); Yap_DebugPlWriteln(t0);
  };

  YAPTerm(Term tn) { mk(tn); };
#ifdef SWIGPYTHON
//   YAPTerm(struct _object *inp) {
// Term tinp = pythonToYAP(inp);
//  t = Yap_InitSlot(tinp);
//}
#endif
  /// private method to convert from Term (internal YAP representation) to
  /// YAPTerm
  // do nothing constructor
  YAPTerm() { t = 0; };
  // YAPTerm(yhandle_t i) { t = i; };
  /// pointer to term
  YAPTerm(void *ptr);
  /// parse string s and construct a term.
  YAPTerm(char *s) {
    Term tp = 0;
    mk(YAP_ReadBuffer(s, &tp));
  }

#if 1
  /// Term destructor, tries to recover slot
  virtual ~YAPTerm(){
      //  fprintf(stderr,"-%d,%lx,%p ",t,LOCAL_HandleBase[t] ,HR);
      /*    if (!t)
            return;
          //          Yap_DebugPlWriteln(LOCAL_HandleBase[t]);
          LOCAL_HandleBase[t] = TermFreeTerm;
          while (LOCAL_HandleBase[LOCAL_CurSlot - 1] == TermFreeTerm) {
            LOCAL_CurSlot--;
          }
          */
  };
#endif

  /// construct a term out of an integer (if you know object type use
  /// YAPIntegerTerm)
  /// YAPTerm(long int num) { mk(MkIntegerTerm(num)); }
  /// construct a term out of an integer (if you know object type use
  /// YAPIntegerTerm)
  /// YAPTerm(double num) { mk(MkFloatTerm(num)); }
  /// parse string s and construct a term.
  /// YAPTerm(YAPFunctor f, YAPTerm ts[]);
  /// extract the tag of a term, after dereferencing.
  YAP_tag_t tag();
  /// copy the term ( term copy )
  Term deepCopy();
  /// numbervars ( int start, bool process=false )
  intptr_t numberVars(intptr_t start, bool skip_singletons = false);
  inline Term term() {
    return Deref(gt());
  } /// from YAPTerm to Term (internal YAP representation)
  YAPTerm arg(int i) {
    BACKUP_MACHINE_REGS();
    Term t0 = gt();
    YAPTerm tf;
    if (!IsApplTerm(t0) && !IsPairTerm(t))
      return (Term)0;
    tf = YAPTerm(ArgOfTerm(i, t0));
    RECOVER_MACHINE_REGS();
    return tf;
  };

  inline void bind(Term b) { LOCAL_HandleBase[t] = b; }
  inline void bind(YAPTerm *b) { LOCAL_HandleBase[t] = b->term(); }
  /// from YAPTerm to Term (internal YAP representation)
  /// fetch a sub-term
  Term &operator[](arity_t n);
  // const YAPTerm *vars();
  /// this term is == to t1
  virtual bool exactlyEqual(YAPTerm t1) {
    bool out;
    BACKUP_MACHINE_REGS();
    out = Yap_eq(gt(), t1.term());
    RECOVER_MACHINE_REGS();
    return out;
  };

  /// t = t1
  virtual bool unify(YAPTerm t1) {
    intptr_t out;
    BACKUP_MACHINE_REGS();
    out = Yap_unify(gt(), t1.term());
    RECOVER_MACHINE_REGS();
    return out;
  };

  /// we can unify t and t1
  virtual bool unifiable(YAPTerm t1) {
    bool out;
    BACKUP_MACHINE_REGS();
    out = Yap_eq(gt(), t1.term());
    RECOVER_MACHINE_REGS();
    return out;
  };

  /// t =@= t1, the two terms are equal up to variable renamingvirtual bool
  /// variant(
  inline virtual YAP_Term variant(YAPTerm t1) {
    intptr_t out;
    BACKUP_MACHINE_REGS();
    out = Yap_Variant(gt(), t1.term());
    RECOVER_MACHINE_REGS();
    return out;
  };

  virtual intptr_t hashTerm(size_t sz, size_t depth, bool variant) {
    intptr_t out;

    BACKUP_MACHINE_REGS();
    out = Yap_TermHash(gt(), sz, depth, variant);
    RECOVER_MACHINE_REGS();
    return out;
  };
  /// term hash,
  virtual bool isVar() { return IsVarTerm(gt()); }   /// type check for unound
  virtual bool isAtom() { return IsAtomTerm(gt()); } ///  type check for atom
  virtual bool isInteger() {
    return IsIntegerTerm(gt());
  } /// type check for integer
  virtual bool isFloat() {
    return IsFloatTerm(gt());
  } /// type check for floating-point
  virtual bool isString() {
    return IsStringTerm(gt());
  } /// type check for a string " ... "
  virtual bool isCompound() {
    return !(IsVarTerm(gt()) || IsNumTerm(gt()));
  }                                                  /// is a primitive term
  virtual bool isAppl() { return IsApplTerm(gt()); } /// is a structured term
  virtual bool isPair() { return IsPairTerm(gt()); } /// is a pair term
  virtual bool isGround() { return Yap_IsGroundTerm(gt()); } /// term is ground
  virtual bool isList() { return Yap_IsListTerm(gt()); }     /// term is a list

  /// extract the argument i of the term, where i in 1...arityvoid
  /// *Yap_RepStreamFromId(int sno)
  virtual Term getArg(arity_t i);

  /// extract the arity of the term
  /// variables have arity 0
  virtual inline arity_t arity() {
    Term t0 = gt();

    if (IsApplTerm(t0)) {
      Functor f = FunctorOfTerm(t0);
      if (IsExtensionFunctor(f))
        return 0;
      return ArityOfFunctor(f);
    } else if (IsPairTerm(t0)) {
      return 2;
    }
    return 0;
  }

  /// return a string with a textual representation of the term
  virtual const char *text() {
    CACHE_REGS
    char *os;

    BACKUP_MACHINE_REGS();
    if (!(os = Yap_TermToBuffer(Yap_GetFromSlot(t), Handle_vars_f))) {
      RECOVER_MACHINE_REGS();
      return 0;
    }
    RECOVER_MACHINE_REGS();
    return os;
  };

  /// return a handle to the term
  inline yhandle_t handle() { return t; };

  /// whether the term actually refers to a live object
  inline bool initialized() { return t != 0; };
};

/**
 * @brief Compound Term
 */
class X_API YAPApplTerm : public YAPTerm {
  friend class YAPTerm;

public:
YAPApplTerm(Term t0) { mk(t0); }
YAPApplTerm(Functor f, Term ts[]) {
    BACKUP_MACHINE_REGS();
    Term t0 = Yap_MkApplTerm(f, f->ArityOfFE, ts);
    mk(t0);
    RECOVER_MACHINE_REGS();
  };
  YAPApplTerm(YAPFunctor f, YAPTerm ts[]);
  YAPApplTerm(const std::string s, unsigned int arity) {
    mk(Yap_MkNewApplTerm(Yap_MkFunctor(Yap_LookupAtom(s.c_str()), arity),
                         arity));
  };
    YAPApplTerm(const std::string s, std::vector<Term> ts);
    YAPApplTerm(const std::string s, std::vector<YAPTerm> ts);
  YAPApplTerm(YAPFunctor f);
  inline Functor functor() { return FunctorOfTerm(gt()); }
  inline YAPFunctor getFunctor() { return YAPFunctor(FunctorOfTerm(gt())); }
    YAPApplTerm(const std::string f, YAPTerm a1);
    YAPApplTerm(const std::string f, YAPTerm a1, YAPTerm a2);
    YAPApplTerm(const std::string f, YAPTerm a1, YAPTerm a2, YAPTerm a3);
    YAPApplTerm(const std::string f, YAPTerm a1, YAPTerm a2, YAPTerm a3,  YAPTerm a4);

    Term getArg(arity_t i) {
    BACKUP_MACHINE_REGS();
    Term t0 = gt();
    Term tf;
    tf = ArgOfTerm(i, t0);
    RECOVER_MACHINE_REGS();
    return tf;
  };
  void putArg(int i, Term targ) {
    // BACKUP_MACHINE_REGS();
    Term t0 = gt();
    RepAppl(t0)[i] = Deref(targ);
    // RECOVER_MACHINE_REGS();
  };
  void putArg(int i, YAPTerm t) {
    // BACKUP_MACHINE_REGS();
    Term t0 = gt();
    RepAppl(t0)[i] = t.term();
    // RECOVER_MACHINE_REGS();
  };
  virtual bool isVar() { return false; }     /// type check for unbound
  virtual bool isAtom() { return false; }    ///  type check for atom
  virtual bool isInteger() { return false; } /// type check for integer
  virtual bool isFloat() { return false; }   /// type check for floating-point
  virtual bool isString() { return false; }  /// type check for a string " ... "
  virtual bool isCompound() { return true; } /// is a primitive term
  virtual bool isAppl() { return true; }     /// is a structured term
  virtual bool isPair() { return false; }    /// is a pair term
  virtual bool isGround() { return true; }   /// term is ground
  virtual bool isList() { return false; }    /// [] is a list
};

/**
 * @brief List Constructor Term
 */
class X_API YAPPairTerm : public YAPTerm {
  friend class YAPTerm;

public:
  YAPPairTerm(Term t0) {
    t0 = Deref(t0);
    if (IsPairTerm(t0) || t0 == TermNil)
      mk(t0);
    else
      Yap_ThrowError(TYPE_ERROR_LIST, t0, "YAPPairTerms");
  }
  YAPPairTerm(YAPTerm hd, YAPTerm tl);
  YAPPairTerm();
  Term getHead() { return (HeadOfTerm(gt())); }
  Term getTail() { return (TailOfTerm(gt())); }
  YAPTerm car() { return YAPTerm(HeadOfTerm(gt())); }
  bool nil() { return gt() == TermNil; }
  YAPPairTerm cdr() { return YAPPairTerm(TailOfTerm(gt())); }
  std::vector<Term> listToArray();
  std::vector<YAPTerm> listToVector();
};

/**
 * @brief Number Term
 */

class X_API YAPNumberTerm : public YAPTerm {
public:
  YAPNumberTerm(){};
  bool isTagged() { return IsIntTerm(gt()); }
};

/**
 * @brief Integer Term
 */

class X_API YAPIntegerTerm : public YAPNumberTerm {
public:
  YAPIntegerTerm(intptr_t i);
  intptr_t getInteger() { return IntegerOfTerm(gt()); };
};

/**
 * @brief Floating Point Term
 */

class X_API YAPFloatTerm : public YAPNumberTerm {
public:
  YAPFloatTerm(double dbl) { mk(MkFloatTerm(dbl)); };

  double getFl() { return FloatOfTerm(gt()); };
};

class X_API YAPListTerm : public YAPTerm {
public:
  /// Create a list term out of a standard term. Check if a valid operation.
  ///
  /// @param[in] the term
  YAPListTerm() { mk(TermNil); /* else type_error */ }
  /// Create an empty list term.
  ///
  /// @param[in] the term
  YAPListTerm(Term t0) { mk(t0); /* else type_error */ }
  /// Create a list term out of an array of terms.
  ///
  /// @param[in] the array of terms
  /// @param[in] the length of the array
  YAPListTerm(YAPTerm ts[], size_t n);
  //      YAPListTerm( vector<YAPTerm> v );
  /// Return the number of elements in a list term.
  size_t length() {
    Term *tailp;
    Term t1 = gt();
    return Yap_SkipList(&t1, &tailp);
  }
  /// Extract the nth element.
  Term &operator[](size_t n);
  /// Extract the first element of a list.
  ///
  /// @param[in] the list
  Term car();
  /// Extract the tail elements of a list.
  ///
  /// @param[in] the list
  Term cdr();
  /// copy a list.
  ///
  /// @param[in] the list
  Term dup();

  /// Check if the list is empty.
  ///
  /// @param[in] the list
  inline bool nil() {
    return gt() == TermNil;
  }

  ;
};

/**
 * @brief String Term
 */
class X_API YAPStringTerm : public YAPTerm {
public:
  /// your standard constructor
  YAPStringTerm(char *s);
  /// use this one to construct length limited strings
  YAPStringTerm(char *s, size_t len);
  /// construct using wide chars
  YAPStringTerm(wchar_t *s);
  /// construct using length-limited wide chars
  YAPStringTerm(wchar_t *s, size_t len);
  const char *getString() { return StringOfTerm(gt()); }
};

/**
 * @brief Atom Term
 * Term Representation of an Atom
 */
class X_API YAPAtomTerm : public YAPTerm {
  friend class YAPModule;
  // Constructor: receives a C-atom;
  YAPAtomTerm(Term t) : YAPTerm(t) { IsAtomTerm(t); }

public:
  YAPAtomTerm(Atom a) { mk(MkAtomTerm(a)); }
  //> Constructor: receives an atom;
  YAPAtomTerm(YAPAtom a) : YAPTerm() { mk(MkAtomTerm(a.a)); }
  //> Constructor: receives a sequence of UTF-8 codes;
  YAPAtomTerm(char s[]);
  // Constructor: receives a sequence of up to n UTF-8 codes;
  YAPAtomTerm(char *s, size_t len);
  // Constructor: receives a sequence of wchar_ts, whatever they may be;
  YAPAtomTerm(wchar_t *s);
  // Constructor: receives a sequence of n wchar_ts, whatever they may be;
  YAPAtomTerm(wchar_t *s, size_t len);
  // Constructor: receives a std::string;
  //   YAPAtomTerm(std::string s) { mk(MkAtomTerm(Yap_LookupAtom(s.c_str())));
  //   };
  bool isVar() { return false; }           /// type check for unbound
  bool isAtom() { return true; }           ///  type check for atom
  bool isInteger() { return false; }       /// type check for integer
  bool isFloat() { return false; }         /// type check for floating-point
  bool isString() { return false; }        /// type check for a string " ... "
  bool isCompound() { return false; }      /// is a primitive term
  bool isAppl() { return false; }          /// is a structured term
  bool isPair() { return false; }          /// is a pair term
  virtual bool isGround() { return true; } /// term is ground
  virtual bool isList() { return gt() == TermNil; } /// [] is a list
  // Getter: outputs the atom;
  YAPAtom getAtom() { return YAPAtom(AtomOfTerm(gt())); }
  // Getter: outputs the name as a sequence of ISO-LATIN1 codes;
  const char *text() { return (const char *)AtomOfTerm(gt())->StrOfAE; }
};
#endif /* YAPT_HH */

/**
 * @brief Variable Term
 */
class X_API YAPVarTerm : public YAPTerm {
  friend class YAPTerm;

public:
  /// constructor
  YAPVarTerm() { mk(MkVarTerm()); };
  /// get the internal representation
  CELL *getVar() { return VarOfTerm(gt()); }
  /// is the variable bound to another one
  YAPVarTerm(Term t) {
    if (IsVarTerm(t)) {
      mk(t);
    }
  }
  /// type check for unbound
  bool unbound() { return IsUnboundVar(VarOfTerm(gt())); }
  inline bool isVar() { return true; }      
  inline bool isAtom() { return false; }     ///  type check for atom
  inline bool isInteger() { return false; }  /// type check for integer
  inline bool isFloat() { return false; }    /// type check for floating-point
  inline bool isString() { return false; }   /// type check for a string " ... "
  inline bool isCompound() { return false; } /// is a primitive term
  inline bool isAppl() { return false; }     /// is a structured term
  inline bool isPair() { return false; }     /// is a pair term
  inline bool isGround() { return false; }   /// term is ground
  inline bool isList() { return false; }     /// term is a list
};

/// @}
