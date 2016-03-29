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
class YAPQuery: public YAPPredicate, public open_query_struct {
  YAPListTerm vnames;
  YAPTerm goal;
  Term names;
  Term t;

  void  initOpenQ();
  void initQuery( Term t );
  void initQuery( YAPTerm ts[], arity_t arity  );
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
  /// goal.
  inline YAPQuery(const char *s): YAPPredicate(s, t, names)
  {
    vnames = YAPListTerm( names );
       
    initQuery( t );
  };

  /// set flags for query execution, currently only for exception handling
  void setFlag(int flag) {q_flags |= flag; }
  /// reset flags for query execution, currently only for exception handling
  void resetFlag(int flag) {q_flags &= ~flag; }
  ///`b  first query
  ///
  /// actually implemented by calling the next();
  inline bool first() { return next(); }
  /// ask for the next solution of the current query
  /// same call for every solution
  bool next();
  /// represent the top-goal
  const char *text();
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
  virtual ~YAPCallback() { }
  virtual void run() { LOG("callback");  }
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
  ////void run() { if (_callback) _callback->run(); }
  /// execute the callback with a text argument.
  void run( char *s) {  if (_callback) _callback->run(s); }
  /// execute the callback with a text argument.
  YAPError hasError( ) {  return yerror.get(); }
  /// build a query on the engine
  YAPQuery *query( const char *s ) {
    return new YAPQuery( s );
  };
  /// current module for the engine
  YAPModule currentModule( ) { return YAPModule( ) ; }
  /// current directory for the engine
  const char *currentDir( ) {
      char dir[1024];
      std::string s = Yap_getcwd(dir, 1024-1);
      return s.c_str();
  }
};

#endif /* YAPQ_HH */
