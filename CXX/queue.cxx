#include <yapi.h>
#include <vector>
#include <queue>


template <class T, class Alloc = std::vector<T> > class YAPVector {
      int maxlen;
    int currlen;
  auto data;
  virtual shift();
  {
    auto datamax = data+currlen, d;
    for (d;i<currlend<datamax;d++) {
      if (*(CELL*)d< end_e) {
	d += SizeOfOpaqueTerm((CELL))*d)-1;
      if (IsVarTerm(d) {
	if (<static T*>
  }
public:
    YAPVector<T> () : data (nullptr), maxlen(0), currlen(0) { }
    YAPVector<T> (int maxlen) : data (new T [maxlen]), maxlen(maxlen), currlen(0) { }

    YAPVector<T> (const YAPVector& o) {
        std::cout << "copy ctor called" << std::endl;
        data = new T [o.maxlen];
        maxlen = o.maxlen;
        currlen = o.currlen;
        std::copy(o.data, o.data + o.maxlen, data);
    }

    YAPVector<T> (const YAPVector<T>&& o) {
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

    friend std::ostream& operator<<(std::ostream &os, const YAPVector<T>& o) {
        auto s = o.data;
        auto e = o.data + o.currlen;;
        while (s < e) {
            os << "[" << *s << "]";
            s++;
        }
        return os;
    }


};

template class  YAPQueue<CELL> {
  /// WE use a structure of the form  {&H,&T}<H>...{&H,&T}<H>
  int last_item;
      int maxlen;
    int currlen;
  auto data;
  void shift(CELL *old);
  {
    CELL * datamax = data+currlen, *d;
    for (d;i<currlend<datamax;d++) {
      if (IsVarTerm(*d)) {
	  CELL *v = VarOfTerm(*d);
	if (*d< end_e) {
	  d += SizeOfOpaqueTerm(d,*d)-1;
	  else if (v >= data && v < currlen) {
	    *d += old-data;
	  }
	}
      } else if (IsApplTerm(*d)) {
	CELL *v = RepAppl(*d);
	if (v >= data && v < currlen) {
	  *d = AbsPair(*d+(old-data));
	  }
      } else if (IsPairTerm(*d)) {
	CELL *v = RepAppl(*d);
	if (v >= data && v < currlen) {
	  *d = AbsPair(*d+(old-data));
	  }
      }
    }}
public:
};
	  }

/// C code
YAPQueue(void) {
  YAPQueue()
    {
  last_item = 0;
      };
};


extern "C" {
  static int queue(void)
  {
    YAPQueue o =  YAPQueue::YAPQueue();
    return Yap_unify(ARG1,MkAddressTerm(o));
  }
  static int queue_push(void)
  {
    YAPQueue o = AddressOfTerm(Deref(ARG1));
    o.
    return Yap_unify(ARG1,MkAddressTerm(ARG1));
  }
  static int queue_close(void) {
 YAPQueue o   =AddressOfTerm(Deref(ARG1);
            std::copy(data, data + currlen, newdata);
			     auto old = data;

			     data = newdata;
			     shift(data);
	    bool rc = Yap_unify(ARG2, AbsPair(HR)    ) && Yap_unify(HR[item+1],ARG3);
			     HR += currlen;
			     return rc;
}

			     void Yap_InitQueue(void)
{
  CACHE_REGS
  Term cm = CurrentModule;
  Yap_InitCPred("queue", 1, queue, 0);
  Yap_InitCPred("queue_push", 1, queue_push, 0);
  Yap_InitCPred("queue_pop", 2, queue_close, 0);


  static int queue_push(void) {
  Term t = Deref(ARG1);
  o = <static cast>
    o[last_item+1] = 
    return Yap_unify(ARG2, AbsPair(HR+(last_item+1));
}

