
#ifndef YAPIE_HH
#define YAPIE_HH

class YAPPPredicate;
class YAPTerm;

/// take information on a Prolog error:
class YAPError {
  std::string name, errorClass, info;
  int swigcode;

public:
  /// error handling when receiving the error term
  YAPError(){};
  /// we just know the error number
  /// exact error ID
  yap_error_number getID() { return LOCAL_ActiveError->errorNo; };
  /// class of error
  yap_error_class_number getErrorClass() {
    return Yap_errorClass(LOCAL_ActiveError->errorNo);
  };
  /// where in the code things happened;
  const char *getFile() { return LOCAL_ActiveError->errorFile; };
  /// predicate things happened;
  Int getLine() { return LOCAL_ActiveError->errorLine; };
  /// the term that caused the bug
  // YAPTerm getCulprit(LOCAL_ActiveError->errorFile){};
  /// text describing the Error
  const char *text();
};

#endif
