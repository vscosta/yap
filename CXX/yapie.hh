

class YAPError {
public:
  static const int YAP_SYNTAX_ERROR = 0x10000; //> syntax error
  static const int YAP_DOMAIN_ERROR = 0x20000;  //> usually illegal parameter, like asin( 2 )
  static const int YAP_TYPE_ERROR = 0x40000; //> usually illegal parameter in the language, like ( 2 mod 3.0 )
  static const int YAP_PERMISSION_ERROR = 0x80000; //> wrong para,eter
  static const int YAP_EVALUATION_ERROR = 0x100000; //> bad arithmetic expressions
  static const int YAP_RESOURCE_ERROR = 0x200000;   //> no resource available, like MEM
  static const int YAP_REPRESENTATION_ERROR = 0x400000; //> bad UTF-8 strings, etc
  static const int YAP_EXISTENCE_ERROR = 0x800000; //> object not found
  static const int YAP_PROFILER = 0x100000; //> improve profiling support.
  static const int YAP_OTHER_ERROR = 0x2000000; //> anything 
};

