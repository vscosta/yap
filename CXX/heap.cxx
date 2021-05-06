#if 0
#include <cxx4yap.hh>
#include <queue>

using namespace std;

class YAPDBTerm:  YAPTerm {
   void * d;
public:
  YAPDBTerm() {
      d=0;
    }

    YAPDBTerm(Term inp) {
      d=Yap_StoreTermInDB( t, 0);
    }

   Term getTerm() {
      return Yap_FetchTermFromDB( d );
    }

void release() {
    Yap_ReleaseTermFromDB(d);
    d= 0;
    }
 
 ~YAPDBTerm() {
    Yap_ReleaseTermFromDB(d);
    d= 0;
    }
  };


class X_API YAPHeap: public std::priority_queue<YAPDBTerm>, public YAPDBTerm  {
  // Create the queue


      // now, let us link to Prolog


    bool proxy(const std::string f, YAPHeap  me,   std::list<Term> inp,  std::list<Term> out ) {
          //> Test whether container is empty,

          if (f == "empty") return empty();
	  if (f == "size")  {   out.push_front(MkIntegerTerm( size() )) ; return true; }
          //> return size (public member function )
	  if (f == "top") {   out.push_front(  (const_cast<YAPDBTerm&>(top())).getTerm() ) ; return true; }
	  if (f == "push") {    push (YAPDBTerm(inp.front())); return true; }
	  if (f == "emplace") {  emplace (YAPDBTerm(inp.front()) ); return true; }
	  if (f == "pop") { (const_cast<YAPDBTerm&>( top())).release(); pop () ; return true; }
      }



      YAPHeap(size_t sz)
      {
      }
      
      ~YAPHeap() {
	while(!empty()) {
	  (const_cast<YAPDBTerm&>(top())).release();
	   pop();
      }
};


/// CXX code
static Int
cxx_object(void) {

  Term t = Deref(ARG1);

    
    //must_be_text(t)
    const char *s = Yap_TextTermToText(t);
    if (!s || s[0] == '\0')
        return false;
    Term o = cxx[s](t);
    return Yap_unify(ARG2, MkAddressTerm(o)    );
}
      };

static int
  cxx_method(void)
{
  Term t = Deref(ARG1);

    
    //must_be_text(t)
    const char *s = Yap_TextTermToText(t);
    if (!s || s[0] == '\0')
        return false;
    Term o = cxx[s](t);
    return Yap_unify(ARG2, MkAddressTerm(o)    );


}
#endif
