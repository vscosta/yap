#include "ProbFormula.h"


int ProbFormula::freeGroup_ = 0;



bool
ProbFormula::sameSkeletonAs (const ProbFormula& f) const
{
  return functor_ == f.functor() && logVars_.size() == f.arity();
}



bool
ProbFormula::contains (LogVar lv) const
{
  return Util::contains (logVars_, lv);
}



bool
ProbFormula::contains (LogVarSet s) const
{
  return LogVarSet (logVars_).contains (s);
}



int
ProbFormula::indexOf (LogVar X) const
{
  int pos = std::distance (
      logVars_.begin(),
      std::find (logVars_.begin(), logVars_.end(), X));
  if (pos == (int)logVars_.size()) {
    pos = -1;
  }
  return pos;
}



bool
ProbFormula::isAtom (void) const
{
  return logVars_.size() == 0;
}



bool
ProbFormula::isCounting (void) const
{
  return countedLogVar_.valid();
}



LogVar
ProbFormula::countedLogVar (void) const
{
  assert (isCounting());
  return countedLogVar_;
}


    
void
ProbFormula::setCountedLogVar (LogVar lv)
{
  countedLogVar_ = lv;
}



void
ProbFormula::rename (LogVar oldName, LogVar newName)
{
  for (unsigned i = 0; i < logVars_.size(); i++) {
    if (logVars_[i] == oldName) {
      logVars_[i] = newName;
    }
  }
  if (isCounting() && countedLogVar_ == oldName) {
    countedLogVar_ = newName;
  }
}


bool operator== (const ProbFormula& f1, const ProbFormula& f2)
{
  return f1.group_ == f2.group_;
  //return functor_ == f.functor_ && logVars_ == f.logVars_ ;
}



std::ostream& operator<< (ostream &os, const ProbFormula& f)
{
  os << f.functor_;
  if (f.isAtom() == false) {
    os << "(" ;
    for (unsigned i = 0; i < f.logVars_.size(); i++) {
     if (i != 0) os << ",";
      if (f.isCounting() && f.logVars_[i] == f.countedLogVar_) {
        os << "#" ;
      }
      os << f.logVars_[i];
    }
    os << ")" ;
  }
  os << "::" << f.range_;
  return os;
}



unsigned
ProbFormula::getNewGroup (void)
{
  freeGroup_ ++;
  return freeGroup_;
}



ostream& operator<< (ostream &os, const ObservedFormula& of)
{
  os << of.functor_ << "/" << of.arity_;
  os << "|" << of.constr_.tupleSet();
  os << " [evidence=" << of.evidence_ << "]";
  return os;
}

