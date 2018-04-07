                                                            /**
 *   @file yapie.hh
 *
 *   @defgroup yap-cplus-error-hanadlinge Errir Handling in the YAP interface.
 *
 *   @brief this is an attempt at supporting error 
 *
 *   @ingroup yap-cplus-interface
 *    @tableofcontents
 *
 *   @{
 *
 * These classes define an object that we can then throw when an error
 * or unexoected event interrupts YAP. Often, the object is built by
 * YAP itself. One can also define one's own error objects.
 *
 * Errors will be thrown from the `C++` code, and may be processed in
 * very different ways. The error object should provide as much data asa
 * possible.
 */


#ifndef YAPIE_HH
#define YAPIE_HH

class X_API YAPPPredicate;
class X_API YAPTerm;

/// take information on a Prolog error:
class X_API YAPError {
  yap_error_number ID;
  std::string goal, info;
  int swigcode;
  
public:
  YAPError(){
      if (LOCAL_ActiveError == nullptr)
          return;
    ID = LOCAL_ActiveError->errorNo;
      if (ID != YAP_NO_ERROR) {};
    std::cerr << "Error detected" << ID << "\n";
  }
  YAPError(yap_error_descriptor_t *des){
      ID = des->errorNo;
      if (ID != YAP_NO_ERROR) {};
    std::cerr << "Error detected" << ID << "\n";
  }
  /// error handler object with initial data when receiving the error term
  YAPError(yap_error_number id, YAPTerm culprit, std::string txt);
  
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
  std::string text();
};

#endif

/// @}
