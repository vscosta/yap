#ifndef YAPQ_HH
#define YAPQ_HH 1

/**
   Queries and engines
*/

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
  Term vs;
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
  inline YAPQuery(const char *s): YAPPredicate(s, q_g, vs)
  {
    vnames = YAPTerm( vs );
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
  /// current module for the engine
  YAPModule currentModule( ) { return YAPModule( ) ; }
};

#endif /* YAPQ_HH */
