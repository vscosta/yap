#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDUTILS_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDUTILS_H_

#include <vector>
#include <unordered_map>
#include <string>
#include <ostream>

#include "TinySet.h"
#include "Util.h"


namespace horus {

class Symbol
{
  public:
    Symbol (void) : id_(Util::maxUnsigned()) { }

    Symbol (unsigned id) : id_(id) { }

    operator unsigned (void) const { return id_; }

    bool valid (void) const { return id_ != Util::maxUnsigned(); }

    static Symbol invalid (void) { return Symbol(); }

  private:
    friend std::ostream& operator<< (std::ostream&, const Symbol&);

    unsigned id_;
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
    friend std::ostream& operator<< (std::ostream&, const LogVar&);

    unsigned id_;
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

}  // namespace horus


namespace std {

template <> struct hash<horus::Symbol> {
  size_t operator() (const horus::Symbol& s) const {
    return std::hash<unsigned>() (s);
  }
};

template <> struct hash<horus::LogVar> {
  size_t operator() (const horus::LogVar& X) const {
    return std::hash<unsigned>() (X);
  }
};

}  // namespace std


namespace horus {

typedef std::vector<Symbol>  Symbols;
typedef std::vector<Symbol>  Tuple;
typedef std::vector<Tuple>   Tuples;
typedef std::vector<LogVar>  LogVars;
typedef TinySet<Symbol>      SymbolSet;
typedef TinySet<LogVar>      LogVarSet;
typedef TinySet<Tuple>       TupleSet;


std::ostream& operator<< (std::ostream&, const Tuple&);


namespace LiftedUtils {

Symbol getSymbol (const std::string&);

void printSymbolDictionary (void);

}



class Ground
{
  public:
    Ground (Symbol f) : functor_(f) { }

    Ground (Symbol f, const Symbols& args)
        : functor_(f), args_(args) { }

    Symbol functor (void) const { return functor_; }

    Symbols args (void) const { return args_; }

    size_t arity (void) const { return args_.size(); }

    bool isAtom (void) const { return args_.empty(); }

  private:
    friend std::ostream& operator<< (std::ostream&, const Ground&);

    Symbol   functor_;
    Symbols  args_;
};

typedef std::vector<Ground> Grounds;



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
    friend std::ostream& operator<< (
        std::ostream&, const Substitution&);

    std::unordered_map<LogVar, LogVar> subs_;
};




inline void
Substitution::add (LogVar X_old, LogVar X_new)
{
  assert (Util::contains (subs_, X_old) == false);
  subs_.insert (std::make_pair (X_old, X_new));
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
  std::unordered_map<LogVar, LogVar>::const_iterator it;
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

}  // namespace horus

#endif // YAP_PACKAGES_CLPBN_HORUS_LIFTEDUTILS_H_

