
class YAPEngine;
class YAPAtom;
class YAPFunctor;
class YAPApplTerm;
class YAPPairTerm;
class YAPQuery;
class YAPPredicate;


class YAPError {
public:
  static const int SYNTAX_ERROR = 0x10000; //> syntax error
  static const int DOMAIN_ERROR = 0x20000;  //> usually illegal parameter, like asin( 2 )
  static const int TYPE_ERROR = 0x40000; //> usually illegal parameter in the language, like ( 2 mod 3.0 )
  static const int PERMISSION_ERROR = 0x80000; //> wrong para,eter
  static const int EVALUATION_ERROR = 0x100000; //> bad arithmetic expressions
  static const int RESOURCE_ERROR = 0x200000;   //> no resource available, like MEM
  static const int REPRESENTATION_ERROR = 0x400000; //> bad UTF-8 strings, etc
  static const int EXISTËNCE_ERROR = 0x400000; //> object not found
  static const int PROFILER = 0x400000; //> improve profiling support.
  static const int OTHER_ERROR = 0x800000; //> anything rldr.,,,,,,,,,,,,,,,,,,,,
};

class YAPErrorClass {
public:
  static const int SYNTAX_ERROR = 0x10000;
  static const int DOMAIN_ERROR = 0x20000;
  static const int TYPE_ERROR = 0x40000;
  static const int PERMISSION_ERROR = 0x80000;
  static const int EVALUATION_ERROR = 0x100000;
  static const int RESOURCE_ERROR = 0x200000;
  static const int REPRESENTATION_ERROR = 0x400000;
  static const int OTHER_ERROR = 0x800000;
};

