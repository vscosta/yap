/**
 *   @file yapq.hh
 ^
 *   @defgroup yap-cplus-query-hanadlinge Query Handling in the YAP interface.
 *   @brief Engine and Query Management
 *
 *   @ingroup yap-cplus-interface
 *    @tableofcontents
 *
 * @{
 * 
 * These classes wrap  engine ana of query.Ann engine is an environment where we can rum 
 * Prolog, that is, where we can run queries.
 *
 */

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
class YAPQuery : public YAPPredicate
{
  bool q_open;
  int q_state;
  yhandle_t q_g, q_handles;
  struct yami *q_p, *q_cp;
  sigjmp_buf q_env;
  int q_flags;
  YAP_dogoalinfo q_h;
  YAPQuery *oq;
  Term names;
  Term goal;
  // temporaries

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
  //YAPQuery(YAPFunctor f, YAPTerm t[]);
  /// string constructor without varnames
  ///
  /// It is given a string, calls the parser and obtains a Prolog term that
  /// should be a callable
  /// goal.
  inline YAPQuery(const char *s) : YAPPredicate(s, goal, names)
  {
    BACKUP_H();
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "got game %ld",
                        LOCAL_CurSlot);
    if (!ap)
      return;
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "%s", vnames.text());
    openQuery();
    RECOVER_H();
  };
  /// string constructor with just an atom
  ///
  /// It is given an atom, and a Prolog term that should be a callable
  /// goal, say `main`, `init`, `live`.
  inline YAPQuery(YAPAtom g) : YAPPredicate(g)
  {
    goal = YAPAtomTerm(g).gt();
    names = TermNil;
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
  /// finish the current query: undo all bindings.
  void close();
  /// query variables.
  void cut();
  Term namedVars();
  /// query variables, but copied out
  Term namedVarsCopy();
  /// convert a ref to a binding.
  YAPTerm getTerm(yhandle_t t);
  /// simple YAP Query;
  /// just calls YAP and reports success or failure, Useful when we just
  /// want things done, eg YAPCommand("load_files(library(lists), )")
  inline bool command()
  {
    bool rc = next();
    close();
    return rc;
  };
};

// Java support

/// This class implements a callback Prolog-side. It will be inherited by the
/// Java or Python
/// class that actually implements the callback.
class YAPCallback
{
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
class YAPEngine
{
private:
  YAPCallback *_callback;
  YAP_init_args init_args;
  YAPError yerror;
  void doInit(YAP_file_type_t BootMode);
  YAP_dogoalinfo q;

public:
  /// construct a new engine; may use a variable number of arguments
  YAPEngine(
      char *savedState = (char *)NULL, char *bootFile = (char *)NULL,
      size_t stackSize = 0, size_t trailSize = 0, size_t maxStackSize = 0,
      size_t maxTrailSize = 0, char *libDir = (char *)NULL,
      char *goal = (char *)NULL, char *topLevel = (char *)NULL,
      bool script = FALSE, bool fastBoot = FALSE,
      bool embedded = true,
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
  void setYAPCallback(YAPCallback *cb)
  {
    delYAPCallback();
    _callback = cb;
  }
  /// execute the callback.
  ////void run() { if (_callback) _callback->run(); }
  /// execute the callback with a text argument.
  void run(char *s)
  {
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
  bool goalt(YAPTerm Yt) { return Yt.term(); };
  /// current directory for the engine
  bool mgoal(Term t, Term tmod);
  /// current directory for the engine

  bool goal(Term t)
  {
    return mgoal(t, CurrentModule);
  }
  /// reset Prolog state
  void reSet();
  /// release: assune that there are no stack pointers, just release memory
  // for last execution
  void release();

  const char *currentDir()
  {
    char dir[1024];
    std::string s = Yap_getcwd(dir, 1024 - 1);
    return s.c_str();
  };
  /// report YAP version as a string
  const char *version()
  {
    std::string s = Yap_version();
    return s.c_str();
  };
  Term fun(Term t);
};

#endif /* YAPQ_HH */

/// @}


