#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cassert>

#include "Factor.h"
#include "FgVarNode.h"


int Factor::indexCount_ = 0;

Factor::Factor (FgVarNode* var) {
  vs_.push_back (var);
  int nParams = var->getDomainSize();
  // create a uniform distribution
  double val = 1.0 / nParams;
  ps_ = ParamSet (nParams, val);
  id_ = indexCount_;
  indexCount_ ++;
}



Factor::Factor (const FgVarSet& vars) {
  vs_ = vars;
  int nParams = 1;
  for (unsigned i = 0; i < vs_.size(); i++) {
    nParams *= vs_[i]->getDomainSize();
  }
  // create a uniform distribution
  double val = 1.0 / nParams;
  ps_ = ParamSet (nParams, val);
  id_ = indexCount_;
  indexCount_ ++;
}



Factor::Factor (FgVarNode* var,
                const ParamSet& params)
{
  vs_.push_back (var);
  ps_ = params;
  id_ = indexCount_;
  indexCount_ ++;
}



Factor::Factor (const FgVarSet& vars,
                const ParamSet& params)
{
  vs_ = vars;
  ps_ = params;
  id_ = indexCount_;
  indexCount_ ++;
}



const FgVarSet&
Factor::getFgVarNodes (void) const
{
  return vs_;
}



FgVarSet&
Factor::getFgVarNodes (void)
{
  return vs_;
}



const ParamSet&
Factor::getParameters (void) const
{
  return ps_;
}



ParamSet&
Factor::getParameters (void)
{
  return ps_;
}



void
Factor::setParameters (const ParamSet& params)
{
  //cout << "ps size:     " << ps_.size() << endl;
  //cout << "params size: " << params.size() << endl;
  assert (ps_.size() == params.size());
  ps_ = params;
}



Factor&
Factor::operator= (const Factor& g)
{
  FgVarSet vars = g.getFgVarNodes();
  ParamSet params = g.getParameters();
  return *this;
}



Factor&
Factor::operator*= (const Factor& g)
{
  FgVarSet gVs        = g.getFgVarNodes();
  const ParamSet& gPs = g.getParameters();

  bool hasCommonVars = false;
  vector<int> varIndexes;
  for (unsigned i = 0; i < gVs.size(); i++) {
    int idx = getIndexOf (gVs[i]);
    if (idx == -1) {
      insertVariable (gVs[i]);
      varIndexes.push_back (vs_.size() - 1);
    } else {
      hasCommonVars = true;
      varIndexes.push_back (idx);
    }
  }
 
  if (hasCommonVars) {
   vector<int> offsets (gVs.size());
    offsets[gVs.size() - 1] = 1;
    for (int i = gVs.size() - 2; i >= 0; i--) {
      offsets[i] = offsets[i + 1] * gVs[i + 1]->getDomainSize();
    }
    vector<CptEntry> entries = getCptEntries();
    for (unsigned i = 0; i < entries.size(); i++) {
      int idx = 0;
      const DomainConf conf = entries[i].getParentConfigurations();
      for (unsigned j = 0; j < varIndexes.size(); j++) {
        idx += offsets[j] * conf[varIndexes[j]];
      }
      //cout << "ps_[" << i << "] = " << ps_[i] << " * " ;
      //cout << gPs[idx] << " , idx = " << idx << endl;
      ps_[i] = ps_[i] * gPs[idx];
    }
  } else {
    // if the originally factors doesn't have common factors.
    // we don't have to make domain comparations
    unsigned idx = 0;
    for (unsigned i = 0; i < ps_.size(); i++) {
      //cout << "ps_[" << i << "] = " << ps_[i] << " * " ;
      //cout << gPs[idx] << " , idx = " << idx << endl;
      ps_[i] = ps_[i] * gPs[idx];
      idx ++;
      if (idx >= gPs.size()) {
        idx = 0;
      }
    }
  }
  return *this;
}



void
Factor::insertVariable (FgVarNode* var)
{
  int c = 0;
  ParamSet newPs (ps_.size() * var->getDomainSize());
  for (unsigned i = 0; i < ps_.size(); i++) {
    for (int j = 0; j < var->getDomainSize(); j++) {
      newPs[c] = ps_[i];
      c ++;
    }
  }
  vs_.push_back (var);
  ps_ = newPs;
}



void
Factor::marginalizeVariable (const FgVarNode* var) {
  int varIndex = getIndexOf (var);
  marginalizeVariable (varIndex);
}



void
Factor::marginalizeVariable (unsigned varIndex)
{
  assert (varIndex >= 0 && varIndex < vs_.size());
  int distOffset    = 1;
  int leftVarOffset = 1;
  for (unsigned i = vs_.size() - 1; i > varIndex; i--) {
    distOffset    *= vs_[i]->getDomainSize();
    leftVarOffset *= vs_[i]->getDomainSize();
  }
  leftVarOffset *= vs_[varIndex]->getDomainSize();

  int ds             = vs_[varIndex]->getDomainSize();
  int count          = 0;
  int offset         = 0;
  int startIndex     = 0;
  int currDomainIdx  = 0;
  unsigned newPsSize = ps_.size() / ds;
  ParamSet newPs;
  newPs.reserve (newPsSize);

  stringstream ss;
  ss << "marginalizing " << vs_[varIndex]->getLabel();
  ss << " from factor " << getLabel() << endl;
  while (newPs.size() < newPsSize) {
    ss << "  sum = ";
    double sum = 0.0;
    for (int j = 0; j < ds; j++) {
      if (j != 0) ss << " + ";
      ss << ps_[offset];
      sum    = sum    + ps_[offset];
      offset = offset + distOffset;
    }
    newPs.push_back (sum);
    count ++;
    if (varIndex == vs_.size() - 1) {
      offset = count * ds;
    } else {
      offset = offset - distOffset + 1;
      if ((offset % leftVarOffset) == 0) {
        currDomainIdx ++;
        startIndex = leftVarOffset * currDomainIdx;
        offset = startIndex;
        count = 0;
      } else {
        offset = startIndex + count;
      }
    }
    ss << " = " << sum << endl;
  }
  //cout << ss.str() << endl;
  ps_ = newPs;
  vs_.erase (vs_.begin() + varIndex);
}



string
Factor::getLabel (void) const
{
  stringstream ss;
  ss << "f(" ;
  // ss << "Φ(" ;
  for (unsigned i = 0; i < vs_.size(); i++) {
    if (i != 0) ss << ", " ;
    ss << "v" << vs_[i]->getVarId();
  }
  ss << ")" ;
  return ss.str();
}



string
Factor::toString (void) const
{
  stringstream ss;
  ss << "vars: " ;
  for (unsigned i = 0; i < vs_.size(); i++) {
    if (i != 0) ss << ", " ;
    ss << "v" << vs_[i]->getVarId();
  }
  ss << endl;
  vector<CptEntry> entries = getCptEntries();
  for (unsigned i = 0; i < entries.size(); i++) {
    ss << "Φ(" ;
    char s = 'a' ;
    const DomainConf& conf = entries[i].getParentConfigurations();
    for (unsigned j = 0; j < conf.size(); j++) {
      if (j != 0) ss << "," ;
      ss << s << conf[j] + 1;
      s++;
    }
    ss << ") = " << ps_[entries[i].getParameterIndex()] << endl;
  }
  return ss.str();
}



vector<CptEntry>
Factor::getCptEntries (void) const
{
  vector<DomainConf> confs (ps_.size());
  for (unsigned i = 0; i < ps_.size(); i++) {
    confs[i].resize (vs_.size());
  }

  int nReps = 1;
  for (int i = vs_.size() - 1; i >= 0; i--) {
    unsigned index = 0;
    while (index < ps_.size()) {
      for (int j = 0; j < vs_[i]->getDomainSize(); j++) {
        for (int r = 0; r < nReps; r++) {
          confs[index][i] = j; 
          index++;
        }
      }
    }
    nReps *= vs_[i]->getDomainSize();
  }
  
  vector<CptEntry> entries;
  for (unsigned i = 0; i < ps_.size(); i++) {
    for (unsigned j = 0; j < vs_.size(); j++) {
    }
    entries.push_back (CptEntry (i, confs[i]));
  }
  return entries;
}



int
Factor::getIndexOf (const FgVarNode* var) const
{
  for (unsigned i = 0; i < vs_.size(); i++) {
    if (vs_[i] == var) {
      return i;
    }
  }
  return -1;
}



Factor operator* (const Factor& f, const Factor& g)
{
  Factor r = f;
  r *= g;
  return r;
}

