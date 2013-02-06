#ifndef HORUS_LIFTEDUTILS_H
#define HORUS_LIFTEDUTILS_H

#include <string>

#include <vector>
#include <unordered_map>

#include "TinySet.h"
#include "Util.h"


using namespace std;


class Symbol
{
  public:
    Symbol (void) : id_(Util::maxUnsigned()) { }

    Symbol (unsigned id) : id_(id) { }

    operator unsigned (void) const { return id_; }

    bool valid (void) const { return id_ != Util::maxUnsigned(); }

    static Symbol invalid (void) { return Symbol(); }

  private:
    unsigned id_;

    friend ostream& operator<< (ostream &os, const Symbol& s);
};


class LogVar
{
  public:
    LogVar (void) : id_(Util::maxUnsigned()) { }

    LogVar (unsigned id) : id_(id) { }

    operator unsigned (void) const  { return id_; }

    LogVar& operator++ (void);

    bool valid (void) const;

  private:
    unsigned id_;

    friend ostream& operator<< (ostream &os, const LogVar& X);
};



inline LogVar&
LogVar::operator++ (void)
{
  assert (valid());
  id_ ++;
  return *this;
}



inline bool
LogVar::valid (void) const
{
  return id_ != Util::maxUnsigned();
}



namespace std {

template <> struct hash<Symbol> {
  size_t operator() (const Symbol& s) const {
    return std::hash<unsigned>() (s);
  }
};

template <> struct hash<LogVar> {
  size_t operator() (const LogVar& X) const {
    return std::hash<unsigned>() (X);
  }
};

};


typedef vector<Symbol>   Symbols;
typedef vector<Symbol>   Tuple;
typedef vector<Tuple>    Tuples;
typedef vector<LogVar>   LogVars;
typedef TinySet<Symbol>  SymbolSet;
typedef TinySet<LogVar>  LogVarSet;
typedef TinySet<Tuple>   TupleSet;


ostream& operator<< (ostream &os, const Tuple& t);


namespace LiftedUtils {

Symbol getSymbol (const string&);

void printSymbolDictionary (void);

}



class Ground
{
  public:
    Ground (Symbol f) : functor_(f) { }

    Ground (Symbol f, const Symbols& args) : functor_(f), args_(args) { }

    Symbol functor (void) const { return functor_; }

    Symbols args (void) const { return args_; }

    size_t arity (void) const { return args_.size(); }

    bool isAtom (void) const { return args_.empty(); }

  private:
    Symbol   functor_;
    Symbols  args_;

    friend ostream& operator<< (ostream &os, const Ground& gr);
};

typedef vector<Ground> Grounds;



class Substitution
{
  public:
    void add (LogVar X_old, LogVar X_new);

    void rename (LogVar X_old, LogVar X_new);

    LogVar newNameFor (LogVar X) const;

    bool containsReplacementFor (LogVar X) const;

    size_t nrReplacements (void) const;

    LogVars getDiscardedLogVars (void) const;

  private:
    unordered_map<LogVar, LogVar> subs_;

    friend ostream& operator<< (ostream &os, const Substitution& theta);

};




inline void
Substitution::add (LogVar X_old, LogVar X_new)
{
  assert (Util::contains (subs_, X_old) == false);
  subs_.insert (make_pair (X_old, X_new));
}



inline void
Substitution::rename (LogVar X_old, LogVar X_new)
{
  assert (Util::contains (subs_, X_old));
  subs_.find (X_old)->second = X_new;
}



inline LogVar
Substitution::newNameFor (LogVar X) const
{
  unordered_map<LogVar, LogVar>::const_iterator it;
  it = subs_.find (X);
  if (it != subs_.end()) {
    return subs_.find (X)->second;
  }
  return X;
}



inline bool
Substitution::containsReplacementFor (LogVar X) const
{
  return Util::contains (subs_, X);
}



inline size_t
Substitution::nrReplacements (void) const
{
  return subs_.size();
}


#endif // HORUS_LIFTEDUTILS_H

