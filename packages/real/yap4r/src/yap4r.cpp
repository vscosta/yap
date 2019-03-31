#include <Rcpp.h>

#undef Realloc
#undef Malloc
#undef Free
#include <yapi.hh>

#include <vector>

#include "real.h"


using namespace Rcpp;

class YAP4R {

  YAPEngine *yap;
  YAPQuery *q;
  std::vector<YAPTerm> args;
  bool failed;
  
public:
//[[Rcpp::export]]

YAP4R() {
  YAPEngineArgs *yargs = new YAPEngineArgs();
  yap = new YAPEngine(yargs);
};


//[[Rcpp::export]]
bool query(std::string p_name,std::string p_module,  SEXP sexp) {
 
  YAPPairTerm tmp;
  if (q) {
    q->close();
    q = NULL;
  }
  if (!sexp_to_pl(tmp.handle(), sexp))
    return false;
   args = tmp.listToVector();
  YAPTerm ts[1], hd;
  YAPTerm qt = YAPApplTerm(p_name,args);
  q = new YAPQuery(qt);
  return true;
};

  
//[[Rcpp::export]]
  bool next() {
    bool rc = true;
    if (failed)
      return false;
    if (q)
      rc = next();
    if (!rc) {
      failed = true;
    }
    return rc;
  }

//[[Rcpp::export]]
  bool cut() {
    bool rc = true;
    if (failed)
      return false;
    if (q)
      rc = cut();
    q = NULL;
    return rc;
  };

//[[Rcpp::export]]
  SEXP ask(int i) {
    if (failed || q==nullptr)
	return R_MissingArg;
    return term_to_sexp(YAPTerm(Yap_XREGS[i]).handle(), false);
  };


  
};

  RCPP_MODULE(mod_yap4r) {
    Rcpp::class_<YAP4R>( "YAP4R" )
  .constructor("documentation for default constructor")
 .method( "query", &YAP4R::query )
.method( "next", &YAP4R::next )
.method( "ask", &YAP4R::ask )
.method( "cut", &YAP4R::cut )
  ;
;
}
