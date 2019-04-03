#include <Rcpp.h>

#undef Realloc
#undef Malloc
#undef Free
#include <yapi.hh>

#include <vector>
#include <string>

#include "real.h"


using namespace Rcpp;

class yap4r {

  YAPEngine *yap;
  YAPQuery *q;
  std::vector<YAPTerm> args;
  bool failed;
  
public:
  yap4r();
  bool query(std::string p_name,std::string p_module,Rcpp::GenericVector sexps);
  bool more();
  bool done();
  SEXP peek(int i);
};

  yap4r::yap4r() {
  YAPEngineArgs *yargs = new YAPEngineArgs();
  yap = new YAPEngine(yargs);
};




  bool yap4r::query(std::string p_name,std::string p_module,Rcpp::GenericVector sexps) {
 
  if (q) {
    q->close();
    q = nullptr;
  }
  std::vector<Term> args = std::vector<Term>();
  yhandle_t sls = Yap_NewHandles(sexps.length());
  for (int i=0; i<sexps.length();i++) {
    if (!sexp_to_pl(sls+i, sexps[i]))
      return false;
    args.push_back( Yap_GetFromSlot(sls+i) );
  }
  if (i==0) {
    YAPTerm qt = YAPAtomTerm(p_name);
  q = new YAPQuery(qt);
  } else {
    YAPFunctor f= YAPFunctor(p_name, args.length());
    YAPAtomTerm mod = YAPAtomTerm(p_module);
    q = new YAPQuery(f,mod,args.data());
  }
  if (q == nullptr)
    return false;
  bool rc = q->next();
  if (!rc) {
    failed = true;
    q = nullptr;
  }
  return true;
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
    if (failed || q==nullptr)
	return R_MissingArg;
    return term_to_sexp(Yap_InitSlot(Yap_XREGS[i]), false);
  }


  RCPP_MODULE(mod_yap4r) {
    class_<yap4r>( "yap4r" )
      .constructor("create an object encapsulating a Prolog engine")
      .method( "query", &yap4r::query, "create an active query within the engine")
      .method( "more", &yap4r::more, "ask for an extra solution")
      .method( "done", &yap4r::done, "terminate the query")
      .method( "peek", &yap4r::peek, "load arg[i] into R")
      ;
}
