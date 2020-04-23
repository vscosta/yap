/// @file yapa.hh
///
/// @brief C++ Interface to atoms and their properties.

#ifndef YAPA_HH
#define YAPA_HH 1

/**
 *
 *   @defgroup yap-cplus-interface-atoms Prolog Atoms
 *
 *  @brief Symbols and associated propeeties.
 *
 *   @ingroup yap-cplus-interface
 *    @tableofcontents
 *  @{
 *
 *
 * Prolog interns symbols, called *atoms* in a Hash table, usually
 called the atom table_. Each entry in this table stores the atom's
 name, but also may have a linked list of _properties_ We use
 properties to represent all kinds of data including.  data-base
 tecords, predicates. operators, and more.
 *
 * In a nutshell:
 *   - YAPAtom serves as the gateway to the data-base;
 *
 *   - YAProp abstracts most data-base objects.
 *
 *    - PropTag allows distinguishing the different classes of YAPProp.
 */
enum PropTag {
  /// predicate
  PRED_TAG                  = PEProp, // 0x0000,
  /// db key, may be associated with a functor
  DB_TAG                    = DBProperty, // 0x8000,
  /// generic functor, may include sub-properties
  FUNCTOR_TAG               = FunctorProperty, // 0xBB00,
  // SPARSE_FUNCTOR_TAG        = 0xFFDF,
  /// arithmetic function
  ARITHMETIC_PROPERTY_TAG   = ExpProperty, // 0xFFE0,
  /// map the atom to an integer
  TRANSLATION_TAG           = TranslationProperty, // 0xFFF4,
  /// ensure the atom may not be garbafe colected
  HOLD_TAG                  = HoldProperty, // 0xFFF6
  /// named mutEX
  MUTEX_TAG                 = MutexProperty, // 0xFFF6,
  /// A typed array, may be in-db or in-stack deped
  ARRAY_TAG                 = ArrayProperty, // 0xFFF7,
  /// module
  MODULE_TAG                = ModProperty, // 0xFFFA,
  /// the original SICStus blackboard
  BLACKBOARD_TAG            = BBProperty, // 0xFFFB,
  /// associate an atomic value with the atom
  VALUE_TAG                 = ValProperty, // 0xFFFC,
  /// Demoen's proposal for gkobal variables
  GLOBAL_VAR_TAG            = GlobalProperty, // 0xFFFD
  /// SWI-STYLE ATOM Extension
  BLOB_TAG                  = BlobProperty, // 0xFFFE,
  /// Prolog operator,
  OPERATOR_TAG              = OpProperty, // 0xFFFF,
};

/**
 * @brief Atom
 * A YAP data-base is a collection of atoms, where each atom has a name
 * and a set of Properties. Examples of properties include functors,
 * predicates, operators, modules, almost everything.
 *
 */
class X_API YAPAtom {
  friend class YAPEngine;
  friend class YAPModuleProp;
  friend class YAPPredicate;
  friend class YAPFunctor;
  friend class YAPAtomTerm;
  friend class YAProp;
  friend class YAPModule;
  friend class YAPQuery;
  Atom a;
  /// construct new YAPAtom from Atom
  YAPAtom( Atom at ) { a = at; }
public:
  /// construct new YAPAtom from UTF-8 string
  YAPAtom( const char * s) { a = Yap_LookupAtom( s ); }
  /// construct new YAPAtom from UTF-8 string
  YAPAtom( const wchar_t * s) { CACHE_REGS a = UTF32ToAtom( s PASS_REGS ); }
  /// construct new YAPAtom from wide string
  //YAPAtom( const wchar_t * s) { a = Yap_LookupMaybeWideAtom( s ); }
  /// construct new YAPAtom from max-length string
  YAPAtom( const char * s, size_t len) { a = Yap_LookupAtomWithLength( s, len ); }
  /// get name of atom
  const char *getName(void);
  ///  get name of  (other way)
  inline const char *text(void)  { return getName(); } ;
  /// get prop of type
  Prop getProp( PropTag tag ) { return Yap_GetAProp( a , (PropFlags)tag ); };
    /// as Atom
  Atom asAtom() { return a; };
};

/**
 * @brief Prop
 * A YAP Propery is  ameta-class for everything one can in a atom.
 * Examples of properties include functors,
 * predicates, operators, modules, almost everything.
 *
 */
class X_API YAPProp {
  friend class YAPModuleProp;
  friend class YAPFunctor;
  /// does nothing, p is defined by the subclass
  YAPProp() { }
  /// get type of prop
  PropTag tag( Prop p )  { return (PropTag)(p->KindOfPE); }
public:
  /// get name of property
  //    virtual YAPAtom name();
  virtual ~YAPProp() {};

};

/**
 * @brief YAPFunctor represents Prolog functors Name/Arity
 */
class X_API YAPFunctor : public YAPProp {
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
    CACHE_REGS f = Yap_MkFunctor(UTF32ToAtom(s PASS_REGS), arity);
  }
  /// Getter: extract name of functor as an atom
  ///
  /// this is for external usage.
  inline  YAPAtom name() { return YAPAtom(NameOfFunctor(f)); };

  /// Getter: extract arity of functor as an unsigned integer
  ///
  /// this is for external usage.
  inline arity_t arity() { return ArityOfFunctor(f); };
  /// Getter: extract functor as C pointer
  ///
  /// inline Functor functor() { return f; };
};


#endif /* YAPA_HH */
/// @}
