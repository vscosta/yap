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
  return std::find (logVars_.begin(), logVars_.end(), lv) != 
         logVars_.end();
}



bool
ProbFormula::contains (LogVarSet s) const
{
  return LogVarSet (logVars_).contains (s);
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



bool
ProbFormula::operator== (const ProbFormula& f) const
{
  return functor_ == f.functor_ && logVars_ == f.logVars_ ;
}



ostream& operator<< (ostream &os, const ProbFormula& f)
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

