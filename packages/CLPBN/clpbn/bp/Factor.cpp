#include <cstdlib>
#include <cassert>

#include <iostream>
#include <sstream>

#include "Factor.h"
#include "FgVarNode.h"


Factor::Factor (const Factor& g)
{
  copyFactor (g);
}



Factor::Factor (FgVarNode* var)
{
  Factor (FgVarSet() = {var});
}



Factor::Factor (const FgVarSet& vars)
{
  vars_ = vars;
  int nParams = 1;
  for (unsigned i = 0; i < vars_.size(); i++) {
    nParams *= vars_[i]->getDomainSize();
  }
  // create a uniform distribution
  double val = 1.0 / nParams;
  dist_ = new Distribution (ParamSet (nParams, val));
}



Factor::Factor (FgVarNode* var,
                const ParamSet& params)
{
  vars_.push_back (var);
  dist_ = new Distribution (params);
}



Factor::Factor (FgVarSet& vars,
                Distribution* dist)
{
  vars_ = vars;
  dist_ = dist;
}



Factor::Factor (const FgVarSet& vars,
                const ParamSet& params)
{
  vars_ = vars;
  dist_ = new Distribution (params);
}



void
Factor::setParameters (const ParamSet& params)
{
  assert (dist_->params.size() == params.size());
  dist_->updateParameters (params);
}



void
Factor::copyFactor (const Factor& g)
{
  vars_ = g.getFgVarNodes();
  dist_ = new Distribution (g.getDistribution()->params);
}



void
Factor::multiplyByFactor (const Factor& g, const vector<CptEntry>* entries)
{
  if (vars_.size() == 0) {
    copyFactor (g);
    return;
  }

  const FgVarSet& gVs = g.getFgVarNodes();
  const ParamSet& gPs = g.getParameters();

  bool factorsAreEqual = true;
  if (gVs.size() == vars_.size()) {
    for (unsigned i = 0; i < vars_.size(); i++) {
      if (gVs[i] != vars_[i]) {
        factorsAreEqual = false;
        break;
      }
    }
  } else {
    factorsAreEqual = false;
  }

  if (factorsAreEqual) {
    // optimization: if the factors contain the same set of variables,
    // we can do 1 to 1 operations on the parameteres
    for (unsigned i = 0; i < dist_->params.size(); i++) {
      dist_->params[i] *= gPs[i];
    }
  } else {
    bool hasCommonVars = false;
    vector<unsigned> gVsIndexes;
    for (unsigned i = 0; i < gVs.size(); i++) {
      int idx = getIndexOf (gVs[i]);
      if (idx == -1) {
        insertVariable (gVs[i]);
        gVsIndexes.push_back (vars_.size() - 1);
      } else {
        hasCommonVars = true;
        gVsIndexes.push_back (idx);
      }
    }
    if (hasCommonVars) {
      vector<unsigned> gVsOffsets (gVs.size());
      gVsOffsets[gVs.size() - 1] = 1;
      for (int i = gVs.size() - 2; i >= 0; i--) {
        gVsOffsets[i] = gVsOffsets[i + 1] * gVs[i + 1]->getDomainSize();
      }

      if (entries == 0) {
        entries = &getCptEntries();
      }

      for (unsigned i = 0; i < entries->size(); i++) {
        unsigned idx = 0;
        const DConf& conf = (*entries)[i].getDomainConfiguration();
        for (unsigned j = 0; j < gVsIndexes.size(); j++) {
          idx += gVsOffsets[j] * conf[ gVsIndexes[j] ];
        }
        dist_->params[i] = dist_->params[i] * gPs[idx];
      }
    } else {
      // optimization: if the original factors doesn't have common variables,
      // we don't need to marry the states of the common variables
      unsigned count = 0;
      for (unsigned i = 0; i < dist_->params.size(); i++) {
        dist_->params[i] *= gPs[count];
        count ++;
        if (count >= gPs.size()) {
          count = 0;
        }
      }
    }
  }
}



void
Factor::insertVariable (FgVarNode* var)
{
  assert (getIndexOf (var) == -1);
  ParamSet newPs;
  newPs.reserve (dist_->params.size() * var->getDomainSize());
  for (unsigned i = 0; i < dist_->params.size(); i++) {
    for (unsigned j = 0; j < var->getDomainSize(); j++) {
      newPs.push_back (dist_->params[i]);
    }
  }
  vars_.push_back (var);
  dist_->updateParameters (newPs);
}



void
Factor::removeVariable (const FgVarNode* var)
{
  int varIndex = getIndexOf (var);
  assert (varIndex >= 0 && varIndex < (int)vars_.size());

  // number of parameters separating a different state of `var', 
  // with the states of the remaining variables fixed
  unsigned varOffset = 1;

  // number of parameters separating a different state of the variable
  // on the left of `var', with the states of the remaining vars fixed
  unsigned leftVarOffset = 1;

  for (int i = vars_.size() - 1; i > varIndex; i--) {
    varOffset     *= vars_[i]->getDomainSize();
    leftVarOffset *= vars_[i]->getDomainSize();
  }
  leftVarOffset *= vars_[varIndex]->getDomainSize();

  unsigned offset    = 0;
  unsigned count1    = 0;
  unsigned count2    = 0;
  unsigned newPsSize = dist_->params.size() / vars_[varIndex]->getDomainSize();

  ParamSet newPs;
  newPs.reserve (newPsSize);

  // stringstream ss;
  // ss << "marginalizing " << vars_[varIndex]->getLabel();
  // ss << " from factor " << getLabel() << endl;
  while (newPs.size() < newPsSize) {
    // ss << "  sum = ";
    double sum = 0.0;
    for (unsigned i = 0; i < vars_[varIndex]->getDomainSize(); i++) {
      // if (i != 0) ss << " + ";
      // ss << dist_->params[offset];
      sum    += dist_->params[offset];
      offset += varOffset;
    }
    newPs.push_back (sum);
    count1 ++;
    if (varIndex == (int)vars_.size() - 1) {
      offset = count1 * vars_[varIndex]->getDomainSize();
    } else {
      if (((offset - varOffset + 1) % leftVarOffset) == 0) {
        count1 = 0;
        count2 ++;
      }
      offset = (leftVarOffset * count2) + count1;
    }
    // ss << " = " << sum << endl;
  }
  // cout << ss.str() << endl;
  vars_.erase (vars_.begin() + varIndex);
  dist_->updateParameters (newPs);
}



const vector<CptEntry>&
Factor::getCptEntries (void) const
{
  if (dist_->entries.size() == 0) {
    vector<DConf> confs (dist_->params.size());
    for (unsigned i = 0; i < dist_->params.size(); i++) {
      confs[i].resize (vars_.size());
    }

    unsigned nReps = 1;
    for (int i = vars_.size() - 1; i >= 0; i--) {
      unsigned index = 0;
      while (index < dist_->params.size()) {
        for (unsigned j = 0; j < vars_[i]->getDomainSize(); j++) {
          for (unsigned r = 0; r < nReps; r++) {
            confs[index][i] = j; 
            index++;
          }
        }
      }
      nReps *= vars_[i]->getDomainSize();
    }
    dist_->entries.clear();
    dist_->entries.reserve (dist_->params.size());
    for (unsigned i = 0; i < dist_->params.size(); i++) {
      dist_->entries.push_back (CptEntry (i, confs[i]));
    }
  }
  return dist_->entries;
}



string
Factor::getLabel (void) const
{
  stringstream ss;
  ss << "Φ(" ;
  for (unsigned i = 0; i < vars_.size(); i++) {
    if (i != 0) ss << "," ;
    ss << vars_[i]->getLabel();
  }
  ss << ")" ;
  return ss.str();
}



void
Factor::printFactor (void)
{
  stringstream ss;
  ss << getLabel() << endl;
  ss << "--------------------" << endl;
  VarSet vs;
  for (unsigned i = 0; i < vars_.size(); i++) {
    vs.push_back (vars_[i]);
  }
  vector<string> domainConfs = Util::getInstantiations (vs);
  const vector<CptEntry>& entries = getCptEntries();
  for (unsigned i = 0; i < entries.size(); i++) {
    ss << "Φ(" << domainConfs[i] << ")" ;
    unsigned idx = entries[i].getParameterIndex();
    ss << " = " << dist_->params[idx] << endl;
  }
  cout << ss.str();
}



int
Factor::getIndexOf (const FgVarNode* var) const
{
  for (unsigned i = 0; i < vars_.size(); i++) {
    if (vars_[i] == var) {
      return i;
    }
  }
  return -1;
}

