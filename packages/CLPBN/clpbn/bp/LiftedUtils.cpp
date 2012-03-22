#include <cassert>

#include <algorithm>
#include <iostream>
#include <sstream>

#include "LiftedUtils.h"
#include "ConstraintTree.h"


namespace LiftedUtils {


unordered_map<string, unsigned> symbolDict;


Symbol
getSymbol (const string& symbolName)
{
  unordered_map<string, unsigned>::iterator it
      = symbolDict.find (symbolName);
  if (it != symbolDict.end()) {
    return it->second;
  } else {
    symbolDict[symbolName] = symbolDict.size() - 1;
    return symbolDict.size() - 1;
  }
}



void
printSymbolDictionary (void)
{
  unordered_map<string, unsigned>::const_iterator it
      = symbolDict.begin();
  while (it != symbolDict.end()) {
    cout << it->first << " -> " << it->second << endl;
    it ++;
  }
}

}



ostream& operator<< (ostream &os, const Symbol& s)
{
  unordered_map<string, unsigned>::const_iterator it
      = LiftedUtils::symbolDict.begin();
  while (it != LiftedUtils::symbolDict.end() && it->second != s) {
    it ++;
  }
  assert (it != LiftedUtils::symbolDict.end());
  os << it->first;
  return os;
}



ostream& operator<< (ostream &os, const LogVar& X)
{
  const string labels[] = {
      "A", "B", "C", "D", "E", "F", 
      "G", "H", "I", "J", "K", "M"  };
  (X >= 12) ? os << "X_" << X.id_ : os << labels[X];
  return os;
}



ostream& operator<< (ostream &os, const Tuple& t)
{
  os << "(" ;
  for (unsigned i = 0; i < t.size(); i++) {
    os << ((i != 0) ? "," : "") << t[i];
  }
  os << ")" ;
  return os;
}



ostream& operator<< (ostream &os, const Ground& gr)
{
  os << gr.functor();
  os << "(" ;
  for (unsigned i = 0; i < gr.args().size(); i++) {
    if (i != 0) os << ", " ;
    os << gr.args()[i];
  }
  os << ")" ;
  return os;
}



void
ObservedFormula::addTuple (const Tuple& t)
{
  if (constr_ == 0) {
    LogVars lvs (arity_);
    for (unsigned i = 0; i < arity_; i++) {
      lvs[i] = i;
    }
    constr_ = new ConstraintTree (lvs);
  }
  constr_->addTuple (t);
}



ostream& operator<< (ostream &os, const ObservedFormula of)
{
  os << of.functor_ << "/" << of.arity_;
  os << "|" << of.constr_->tupleSet();
  os << " [evidence=" << of.evidence_ << "]";
  return os;
}

