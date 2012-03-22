#ifndef HORUS_LIFTEDUTILS_H
#define HORUS_LIFTEDUTILS_H

#include <limits>
#include <string>
#include <vector>
#include <unordered_map>


#include "TinySet.h"
#include "Util.h"


using namespace std;


class Symbol
{
  public:
    Symbol (void) : id_(numeric_limits<unsigned>::max()) { }
    Symbol (unsigned id) : id_(id) { }
    operator unsigned (void) const { return id_; }
    bool valid (void) const { return id_ != numeric_limits<unsigned>::max(); }
    static Symbol invalid (void) { return Symbol(); }
    friend ostream& operator<< (ostream &os, const Symbol& s);
  private:
    unsigned id_;
};


class LogVar
{
  public:
    LogVar (void) : id_(numeric_limits<unsigned>::max()) { }
    LogVar (unsigned id) : id_(id) { }
    operator unsigned (void) const  { return id_; }

    LogVar& operator++ (void)
    {
      assert (valid());
      id_ ++;
      return *this;
    }

    bool valid (void) const
    { 
      return id_ != numeric_limits<unsigned>::max();
    }

    friend ostream& operator<< (ostream &os, const LogVar& X);
  private:
    unsigned id_;
};


namespace std {
template <> struct hash<Symbol> {
  size_t operator() (const Symbol& s) const {
    return std::hash<unsigned>() (s);
  }};

template <> struct hash<LogVar> {
  size_t operator() (const LogVar& X) const {
    return std::hash<unsigned>() (X);
  }};
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
Symbol  getSymbol (const string&);
void    printSymbolDictionary (void);
}



class Ground
{
  public:
    Ground (Symbol f) : functor_(f) { }
    Ground (Symbol f, const Symbols& args) : functor_(f), args_(args) { }

    Symbol    functor (void) const  { return functor_; }
    Symbols   args    (void) const  { return args_; }
    unsigned  arity   (void) const  { return args_.size(); }
    bool      isAtom  (void) const  { return args_.size() == 0; }
    friend ostream& operator<< (ostream &os, const Ground& gr);
  private:
    Symbol    functor_;
    Symbols   args_;
};
typedef vector<Ground> Grounds;



class ConstraintTree;
class ObservedFormula
{
  public:
    ObservedFormula (Symbol f, unsigned a, unsigned ev) 
        : functor_(f), arity_(a), evidence_(ev), constr_(0) { }

    ObservedFormula (Symbol f, unsigned ev, const Tuple& tuple) 
        : functor_(f), arity_(tuple.size()), evidence_(ev), constr_(0)
    {
      addTuple (tuple);
    }

    Symbol          functor   (void) const  { return functor_;    }
    unsigned        arity     (void) const  { return arity_;      }
    unsigned        evidence  (void) const  { return evidence_;   }
    ConstraintTree* constr    (void) const  { return constr_;     }
    bool            isAtom    (void) const  { return arity_ == 0; }

    void addTuple (const Tuple& t);
    friend ostream& operator<< (ostream &os, const ObservedFormula opv);
  private:
    Symbol            functor_;
    unsigned          arity_;
    unsigned          evidence_;
    ConstraintTree*   constr_;
};
typedef vector<ObservedFormula*> ObservedFormulas;



class Substitution
{
  public:
    void add (LogVar X_old, LogVar X_new)
    {
      subs_.insert (make_pair (X_old, X_new));
    }
    void rename (LogVar X_old, LogVar X_new)
    {
      assert (subs_.find (X_old) != subs_.end());
      subs_.find (X_old)->second = X_new;
    }
    LogVar newNameFor (LogVar X) const
    {
      assert (subs_.find (X) != subs_.end());
      return subs_.find (X)->second;
    }
  private:
    unordered_map<LogVar, LogVar> subs_;
};


#endif // HORUS_LIFTEDUTILS_H

