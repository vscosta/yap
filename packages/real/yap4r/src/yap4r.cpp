#include <Rcpp.h>


// [[Rcpp::plugins("cpp11")]]

#undef Realloc
#undef Malloc
#undef Free
#include <yapi.hh>

#include <string>
#include <vector>

#include "real.h"

extern "C" {
extern void install_real(void);
}

using namespace Rcpp;

class yap4r  {

  YAPEngine *yap;
  YAPQuery *q;
  std::vector<YAPTerm> args;
  bool failed;
  Functor functorEvalText,  functorCompile,   functorLibrary;

public:
  SEXP qsexp;
  yap4r();
  SEXP query(std::string query);
  SEXP more();
  bool done();
  bool eval(std::string l);
  bool run(SEXP s);
  SEXP peek(int i);
  bool compile(std::string s);
  bool library(std::string s);
};

// [[Rcpp::export(rng = false)]]


//' Wrapper allowing R to control an YAP environment.
//'
//' First, it creates an YAP engine using the C++ YAPEngine class;
//' Second, it installs the Real library, both Prolog and C.
//' 
//' @examples
//' library(yap4r)
//' y <- new(yap4r)
//'
yap4r::yap4r() {
  YAPEngineArgs *yargs = new YAPEngineArgs();
  yargs->setEmbedded(true);
  yap = new YAPEngine(yargs);
  functorEvalText = YAPFunctor("text_query",1).fun();
  functorCompile =  YAPFunctor("compile",1).fun();
  functorLibrary =  YAPFunctor("library",1).fun();
  install_real();
  library("real");
};

LogicalVector f(){
  LogicalVector rc = {false};
  return rc;
}


//' query the YAP database x a text query. The output is a
//' S-EXP.
//' 
//' @param: query as a list of characters
//' 
//' The function often starts a sequence:
//'   + `query(S)`: create a query and get the first answer;
//'   + `more()`: get an extra answer;
//'   + `done()`: recover the query.
//' 
//' An answer is a vector of S-expressions, with one per query
//' variable.
//'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
//' @examples
//' y$query('current_predicate(X,Y)')
//' # $X
//' # text_query
//' #   
//' #  $Y
//' #  text_query()
//' #
//' 
//' 
//'  y$query('between(1,10,X)')
//' # $X
//' #  [1] 1
//' 
//' @export
SEXP yap4r::query(std::string query) {
 
  if (q) {
    q->close();
      q = nullptr;
  }
   YAP_StartSlots();
   query = "r_query( ("+query+ ") ) ";
   q = new YAPQuery(query.c_str());
  failed = false;
  if (q == nullptr) {
    return f();
  }
  bool rc = q->next();
  if (!rc) {
    failed = true;
      q = nullptr;
       throw YAPError(SOURCE(), EVALUATION_ERROR_R_ENVIRONMENT, TermNil, "");
  }
  if (rc) {    
    return term_to_sexp(q->namedVarTerms()->handle(), false);
  }
   return f();
}

//' ask for more solutions from a query.
//'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
//' @examples
//' y$more()
//' # [X=r_query,Y=r_query(_20256)]
//' # $X
//' #   r_query
//' #   
//' #   $Y
//' #   r_query()
//' # $X
//' # text_query
//' #   
//' #  $Y
//' #  text_query()
//' #
//'
//' 
//'  y$query('between(1,10,X)')
//' # $X
//' #  [1] 1
//'
//' @export
SEXP yap4r::more() {
  bool rc = true;
  if (failed)
    return f();
  if (q)
    rc = q->next();
  std::cerr << q->namedVarTerms()->text() << "\n";
  if (!rc) {
    failed = true;
  }
  if (rc)
    return term_to_sexp(q->namedVarTerms()->handle(), false);
  return f();
}



//' ask for more solutions from a query.
//'
//'
//' @export
bool yap4r::done() {

  if (failed)
    return false;
  if (q)
    q->cut();
  q = NULL;
  return true;
}


//' run a Prolog term represented as a sexp.
//'
//'
//' @export
bool yap4r::run(SEXP l) {
  yhandle_t yh = Yap_InitHandle(MkVarTerm());
  if (!sexp_to_pl(yh, l))
    return false;
  return yap->mgoal(Yap_GetFromHandle(yh), USER_MODULE);
}


//' run a Prolog term represented as text.
//'
//' @example
//' # example session with the Aleph ILP system.
//' library(yap4r)
//` y <- new(yap4r)
//' y$compile('aleph')
//- setwd('~/ilp/carcinogenesis')
//' y$eval('read(aleph)')
//' y$eval('induce')
//' @export
bool yap4r::eval(std::string l) {
  Term t = MkAtomTerm(Yap_LookupAtom(l.c_str()));
  return yap->mgoal(Yap_MkApplTerm(functorEvalText, 1, &t), USER_MODULE);
}


//' compile Prolog file
//'
//' @example
//' y$compile('aleph')
//' @export
bool yap4r::compile(std::string s) {
  Term t = MkAtomTerm(Yap_LookupAtom(s.c_str()));
  return yap->mgoal(Yap_MkApplTerm(functorCompile, 1, &t), USER_MODULE);
}

//' compile Prolog library
//'
//' @example
//' u$library('lists')
//' @export
bool yap4r::library(std::string s) {
  Term t = MkAtomTerm(Yap_LookupAtom(s.c_str()));
  t = Yap_MkApplTerm(functorLibrary, 1, &t);
  return yap->mgoal(Yap_MkApplTerm(functorCompile, 1, &t), USER_MODULE);
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
      .method("eval", &yap4r::eval, "eval query as text")
      .method("run", &yap4r::run, "eval query as S-EXP")
      .method("compile", &yap4r::compile, "compile the file")
      .method("library", &yap4r::library, "compile the library")
      .method("peek", &yap4r::peek, "load arg[i] into R")
    ;
}
