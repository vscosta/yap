#include <cassert>

#include <iostream>

#include "LiftedUtils.h"


namespace Horus {

namespace LiftedUtils {

std::unordered_map<std::string, unsigned> symbolDict;


Symbol
getSymbol (const std::string& symbolName)
{
  std::unordered_map<std::string, unsigned>::iterator it
      = symbolDict.find (symbolName);
  if (it != symbolDict.end()) {
    return it->second;
  } else {
    symbolDict[symbolName] = symbolDict.size() - 1;
    return symbolDict.size() - 1;
  }
}



void
printSymbolDictionary()
{
  std::unordered_map<std::string, unsigned>::const_iterator it
      = symbolDict.begin();
  while (it != symbolDict.end()) {
    std::cout << it->first << " -> " << it->second << std::endl;
    ++ it;
  }
}

}



std::ostream&
operator<< (std::ostream& os, const Symbol& s)
{
  std::unordered_map<std::string, unsigned>::const_iterator it
      = LiftedUtils::symbolDict.begin();
  while (it != LiftedUtils::symbolDict.end() && it->second != s) {
    ++ it;
  }
  assert (it != LiftedUtils::symbolDict.end());
  os << it->first;
  return os;
}



std::ostream&
operator<< (std::ostream& os, const LogVar& X)
{
  const std::string labels[] = {
      "A", "B", "C", "D", "E", "F",
      "G", "H", "I", "J", "K", "M"  };
  (X >= 12) ? os << "X_" << X.id_ : os << labels[X];
  return os;
}



std::ostream&
operator<< (std::ostream& os, const Tuple& t)
{
  os << "(" ;
  for (size_t i = 0; i < t.size(); i++) {
    os << ((i != 0) ? "," : "") << t[i];
  }
  os << ")" ;
  return os;
}



std::ostream&
operator<< (std::ostream& os, const Ground& gr)
{
  os << gr.functor();
  os << "(" ;
  for (size_t i = 0; i < gr.args().size(); i++) {
    if (i != 0) os << ", " ;
    os << gr.args()[i];
  }
  os << ")" ;
  return os;
}



LogVars
Substitution::getDiscardedLogVars() const
{
  LogVars discardedLvs;
  std::set<LogVar> doneLvs;
  std::unordered_map<LogVar, LogVar>::const_iterator it
      = subs_.begin();
  while (it != subs_.end()) {
    if (Util::contains (doneLvs, it->second)) {
      discardedLvs.push_back (it->first);
    } else {
      doneLvs.insert (it->second);
    }
    ++ it;
  }
  return discardedLvs;
}



std::ostream&
operator<< (std::ostream& os, const Substitution& theta)
{
  std::unordered_map<LogVar, LogVar>::const_iterator it;
  os << "[" ;
  it = theta.subs_.begin();
  while (it != theta.subs_.end()) {
    if (it != theta.subs_.begin()) os << ", " ;
    os << it->first << "->" << it->second ;
    ++ it;
  }
  os << "]" ;
  return os;
}

}  // namespace Horus

