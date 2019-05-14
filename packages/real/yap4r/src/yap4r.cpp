#include <Rcpp.h>

#undef Realloc
#undef Malloc
#undef Free
#include <yapi.hh>

#include <string>
#include <vector>

#include "real.h"

using namespace Rcpp;

class yap4r  {

  YAPEngine *yap;
  YAPQuery *q;
  std::vector<YAPTerm> args;
  bool failed;

public:
  SEXP qsexp;
  yap4r();
  bool query(std::string p_name, GenericVector sexps = R_NilValue,
             std::string p_module = "user");
  bool more();
  bool done();
  bool eval_text(std::string l);
  bool run(SEXP s);
  SEXP peek(int i);
  bool compile(std::string s);
  bool library(std::string s);
};

yap4r::yap4r() {
  YAPEngineArgs *yargs = new YAPEngineArgs();
  yap = new YAPEngine(yargs);
};

bool yap4r::query(std::string query) {
  if (q) {
    q->close();
    q = nullptr;
  }
  yhandle_t t;
  arity_t arity;
    q = new YAPQuery(qt);
    t = qt.handle();
    YAPFunctor f = YAPFunctor(p_name.c_str(), arity);
    YAPAtomTerm mod = YAPAtomTerm(p_module.c_str());
    t = YAPApplTerm(p_name.c_str(), args.data()).handle();
    q = new YAPQuery(f, mod, args.data());
  }
  if (q == nullptr)
    return false;
  bool rc = q->next();
  if (!rc) {
    failed = true;
    q = nullptr;
  }
  if (rc)
    qsexp = term_to_sexp(t, false);

  return rc;
}

bool yap4r::run(SEXP l) {
  yhandle_t yh = Yap_InitHandle(MkVarTerm());
  if (!sexp_to_pl(yh, l))
    return false;
  return yap->mgoal(Yap_GetFromHandle(yh), USER_MODULE);
}

bool yap4r::eval_text(std::string l) {
      YAPAtomTerm t = YAPAtomTerm(l.c_str());
  return yap->mgoal(YAPApplTerm("eval_text", t).term(), MkAtomTerm(Yap_LookupAtom("real")));
}

bool yap4r::compile(std::string s) {
  YAPTerm t;
  t = YAPAtomTerm(s.c_str());
  return yap->mgoal(YAPApplTerm("compile", t).term(), USER_MODULE);
}
bool yap4r::library(std::string s) {
  YAPTerm t;
  t = YAPApplTerm("library", YAPAtomTerm(s.c_str()));
  return yap->mgoal(YAPApplTerm("compile", t).term(), USER_MODULE);
}

bool yap4r::more() {
  bool rc = true;
  if (failed)
    return false;
  if (q)
    rc = q->next();
  if (!rc) {
    failed = true;
  }
  return rc;
}

bool yap4r::done() {

  if (failed)
    return false;
  if (q)
    q->cut();
  q = NULL;
  return true;
}

SEXP yap4r::peek(int i) {
  if (failed || q == nullptr)
    return R_MissingArg;
  if (i == 0)
    return qsexp;
  return term_to_sexp(Yap_InitSlot(Yap_XREGS[i]), false);
}

RCPP_MODULE(yap4r) {
  class_<yap4r>("yap4r")
      .constructor("create an object encapsulating a Prolog engine")
      .method("query", &yap4r::query, "create an active query within the enginefrom text")
      .method("more", &yap4r::more, "ask for an extra solution")
      .method("done", &yap4r::done, "terminate the query")
      .method("eval_text", &yap4r::eval_text, "terminate the query")
      .method("run", &yap4r::run, "terminate the query")
      .method("compile", &yap4r::compile, "compile the file")
      .method("library", &yap4r::library, "compile the library")
      .method("peek", &yap4r::peek, "load arg[i] into R")
    ;
}
