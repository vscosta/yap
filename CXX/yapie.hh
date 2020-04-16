                                                            /**
 *   @file yapie.hh
 *
 *   @defgroup yap-cplus-error-hanadlinge Error Handling in the YAP interface.
 *
 *   @brief  error handling in C++ and OO languages
 *
 *   @ingroup yap-cplus-interface
 *  
 *
 *   @{
 *
 * These classes define an object that we can then throw when an error
 * or unexoected event interrupts YAP. Often, the object is built by
 * YAP itself. One can also define one's own error objects.
 *
 * Errors will be thrown from the `C++` code, and may be processed in
 * very different ways. The error object should provide as much data as
 * possible.
 */


#ifndef YAPIE_HH
#define YAPIE_HH

class X_API YAPPPredicate;



/// take information on a Prolog error:
class X_API YAPError {
		      //int swigcode;
  yap_error_descriptor_t *info;

public:
  /// wraps the default error descriptor
  YAPError() {
    info = LOCAL_ActiveError;
    if (!info)
      LOCAL_ActiveError = info = (yap_error_descriptor_t *)calloc( sizeof( yap_error_descriptor_t ), 1);
    //  if (info->errorNo != YAP_NO_ERROR) {};
    //std::cerr << "Error detected" << info->errorNo << "\n";
  }
  /// if des != nullptr, wrap a preexisting error descriptor;
  /// otherwise, generate a new one
  YAPError(yap_error_descriptor_t *des) {
    if (des)
    info=  des;
    else info = (yap_error_descriptor_t *)calloc( sizeof( yap_error_descriptor_t ), 1);
    //      if (info->errorNo != YAP_NO_ERROR) {};
    //std::cerr << "Error detected" << info->errorNo << "\n";

  }


  /// error handler object with initial data when receiving the error term
  //  YAPError( std::string file, std::string function, int lineno,
  //	    yap_error_number id, YAPTerm culprit, std::string txt) {
  //  info = new yap_error_descriptor_t;
  //  Yap_MkErrorRecord(info, file.c_str(), function.c_str(), lineno, id, culprit.term(), txt.c_str());
  //}

   /// error handler object with initial data when receiving the error term
  YAPError( const char * file, const char * function, int lineno,
	    yap_error_number id, YAP_Term culprit, const char * txt) {
    info = (yap_error_descriptor_t *)calloc( sizeof( yap_error_descriptor_t ), 1);
    Yap_MkErrorRecord(info, file, function, lineno, id, culprit, txt);
  }

  /// short version
#define SOURCE()  __FILE__, __FUNCTION__, __LINE__
  
  /// we just know the error number
  /// exact error ID
  yap_error_number getID() { return info->errorNo; };
  /// class of error
  yap_error_class_number getErrorClass() {
    return Yap_errorClass(info->errorNo);
  };
  /// where in the code things happened;
  const char *getFile() { return info->errorFile; };
  /// predicate things happened;
  Int getLine() { return info->errorLine; };
  /// the term that caused the bug
  // YAPTerm getCulprit(info->errorFile){};
  /// text describing the Error
  std::string text();
};

#endif

/// @}
