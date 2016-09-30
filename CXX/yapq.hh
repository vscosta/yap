#ifndef YAPQ_HH
#define YAPQ_HH 1

class YAPPredicate;

/**
   Queries and engines
*/

/**
 * @brief Queries
 *
 * interface to a YAP Query;
 * uses an SWI-like status info internally.
 */
class YAPQuery : public YAPPredicate {
  bool q_open;
  int q_state;
  yhandle_t q_g, q_handles;
  struct yami *q_p, *q_cp;
  jmp_buf q_env;
  int q_flags;
  YAP_dogoalinfo q_h;
  YAPQuery *oq;
  YAPListTerm vnames;
  YAPTerm goal;
  // temporaries
  Term tgoal, names;

  void openQuery();

public:
  /// main constructor, uses a predicate and an array of terms
  ///
  /// It is given a YAPPredicate _p_ , and an array of terms that must have at
  /// least
  /// the same arity as the functor.
  YAPQuery(YAPPredicate p, YAPTerm t[]);
  /// full constructor,
  ///
  /// It is given a functor, module, and an array of terms that must have at
  /// least
  /// the same arity as the functor.
  YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm t[]);
  /// functor/term constructor,
  ///
  /// It is given a functor, and an array of terms that must have at least
  /// the same arity as the functor. Works within the current module.
  YAPQuery(YAPFunctor f, YAPTerm t[]);
  /// string constructor without varnames
  ///
  /// It is given a string, calls the parser and obtains a Prolog term that
  /// should be a callable
  /// goal.
  inline YAPQuery(const char *s) : YAPPredicate(s, tgoal, names) {
    BACKUP_H();
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "got game %d",
                        LOCAL_CurSlot);
    if (!ap)
      return;
    goal = YAPTerm(tgoal);
    vnames = YAPListTerm(names);
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "%s", vnames.text());
    openQuery();
    RECOVER_H();
  };
  /// string constructor with just an atom
  ///
  /// It is given an atom, and a Prolog term that should be a callable
  /// goal, say `main`, `init`, `live`.
  inline YAPQuery(YAPAtom g) : YAPPredicate(g) {
    goal = YAPAtomTerm(g);
    vnames = YAPListTerm();
    openQuery();
  };

  /// set flags for query execution, currently only for exception handling
  void setFlag(int flag) { q_flags |= flag; }
  /// reset flags for query execution, currently only for exception handling
  void resetFlag(int flag) { q_flags &= ~flag; }
  /// first query
  ///
  /// actually implemented by calling the next();
  inline bool first() { return next(); }
  /// ask for the next solution of the current query
  /// same call for every solution
  bool next();
  /// does this query have open choice-points?
  /// or is it deterministic?
  bool deterministic();
  /// represent the top-goal
  const char *text();
  /// remove alternatives in the current search space, and finish the current
  /// query
  void cut();
  /// finish the current query: undo all bindings.
  void close();
  /// query variables.
  YAPListTerm namedVars();
  /// query variables, but copied out
  YAPListTerm namedVarsCopy();
  /// convert a ref to a binding.
  YAPTerm getTerm(yhandle_t t);
  /// simple YAP Query;
  /// just calls YAP and reports success or failure, Useful when we just
  /// want things done, eg YAPCommand("load_files(library(lists), )")
  inline bool command() {
    bool rc = next();
    close();
    return rc;
  };
};

// Java support

/// This class implements a callback Prolog-side. It will be inherited by the
/// Java or Python
/// class that actually implements the callback.
class YAPCallback {
public:
  virtual ~YAPCallback() {}
  virtual void run() { LOG("callback"); }
  virtual void run(char *s) {}
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
  void doInit(YAP_file_type_t BootMode);

public:
  /// construct a new engine; may use a variable number of arguments
  YAPEngine(
      char *savedState = (char *)NULL, char *bootFile = (char *)NULL,
      size_t stackSize = 0, size_t trailSize = 0, size_t maxStackSize = 0,
      size_t maxTrailSize = 0, char *libDir = (char *)NULL,
      char *goal = (char *)NULL, char *topLevel = (char *)NULL,
      bool script = FALSE, bool fastBoot = FALSE,
      YAPCallback *callback = (YAPCallback *)
          NULL); /// construct a new engine, including aaccess to callbacks
                 /// construct a new engine using argc/argv list of arguments
  YAPEngine(int argc, char *argv[],
            YAPCallback *callback = (YAPCallback *)NULL);
  /// kill engine
  ~YAPEngine() { delYAPCallback(); }
  /// remove current callback
  void delYAPCallback() { _callback = 0; }
  /// set a new callback
  void setYAPCallback(YAPCallback *cb) {
    delYAPCallback();
    _callback = cb;
  }
  /// execute the callback.
  ////void run() { if (_callback) _callback->run(); }
  /// execute the callback with a text argument.
  void run(char *s) {
    if (_callback)
      _callback->run(s);
  }
  /// stop yap
  void close() { Yap_exit(0); }

  /// execute the callback with a text argument.
  bool hasError() { return LOCAL_Error_TYPE != YAP_NO_ERROR; }
  /// build a query on the engine
  YAPQuery *query(const char *s) { return new YAPQuery(s); };
  /// current module for the engine
  YAPModule currentModule() { return YAPModule(); }
  /// given a handle, fetch a term from the engine
  inline YAPTerm getTerm(yhandle_t h) { return YAPTerm(h); }
  /// current directory for the engine
  bool call(YAPPredicate ap, YAPTerm ts[]);
  /// current directory for the engine
  bool goal(YAPTerm t);

  const char *currentDir() {
    char dir[1024];
    std::string s = Yap_getcwd(dir, 1024 - 1);
    return s.c_str();
  };
  /// report YAP version as a string
  const char *version() {
    std::string s = Yap_version();
    return s.c_str();
  };
#ifdef SWIGPYTHON
  inline void share(PyObject *arg) {
    LOCAL_shared = arg;
  };
#endif
};

#endif /* YAPQ_HH */
