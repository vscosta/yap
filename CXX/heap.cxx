#include <yapi.h>
#include <vector>
#include <queue>


template <class T, class Alloc = std::vector<T> > class MyVector {
      size_t maxlen;
    size_t currlen;
  T data;
public:
    MyVector<T> () : data (nullptr), maxlen(0), currlen(0) { }
    MyVector<T> (int maxlen) : data (new T [maxlen]), maxlen(maxlen), currlen(0) { }

    MyVector<T> (const MyVector& o) {
        std::cout << "copy ctor called" << std::endl;
        data = new T [o.maxlen];
        maxlen = o.maxlen;
        currlen = o.currlen;
        std::copy(o.data, o.data + o.maxlen, data);
    }

    MyVector<T> (const MyVector<T>&& o) {
        std::cout << "move ctor called" << std::endl;
        data = o.data;
        maxlen = o.maxlen;
        currlen = o.currlen;
    }

    void push_term (const Term& i) {
      H0 = HR = HB = data;
      ASP = data+currlen-256;
      Term j =  CopyTermToArena(i, false, false,
		      nullptr, nullptr,
		      nullptr PASS_REGS);
        if (currlen >= maxlen) {
            maxlen *= 2;
            auto newdata = new T [maxlen];
            std::copy(data, data + currlen, newdata);
            if (data) {
                delete[] data;
            }
            data = newdata;
        }
    }

    friend std::ostream& operator<<(std::ostream &os, const MyVector<T>& o) {
        auto s = o.data;
        auto e = o.data + o.currlen;;
        while (s < e) {
            os << "[" << *s << "]";
            s++;
        }
        return os;
    }


};

template <class T, class Alloc = MyVector<T> > class MyQueue {


/// C code
static Int
queue(void) {

  Term t = Deref(ARG1);

  MyQueue<Term> o;
  return  Yap_unify(ARG2, MkAddressTerm(o)    );
      };

  static int qclod;
{
  Term t = Deref(ARG1);
  o = <static cast><
    
    //must_be_text(t)
    const char *s = Yap_TextTermToText(t);
    if (!s || s[0] == '\0')
        return false;
    Term o = cxx[s](t);
    return Yap_unify(ARG2, MkAddressTerm(o)    );
}


