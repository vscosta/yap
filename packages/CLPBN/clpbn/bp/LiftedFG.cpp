
#include "LiftedFG.h"
#include "FgVarNode.h"
#include "Factor.h"
#include "Distribution.h"

LiftedFG::LiftedFG (const FactorGraph& fg)
{
  groundFg_  = &fg;
  freeColor_ = 0;

  const FgVarSet& varNodes = fg.getFgVarNodes();
  const FactorSet& factors = fg.getFactors();
  varColors_.resize (varNodes.size());
  factorColors_.resize (factors.size());
  for (unsigned i = 0; i < factors.size(); i++) {
    factors[i]->setIndex (i);
  }

  // create the initial variable colors
  VarColorMap colorMap;
  for (unsigned i = 0; i < varNodes.size(); i++) {
    unsigned dsize = varNodes[i]->getDomainSize();
    VarColorMap::iterator it = colorMap.find (dsize);
    if (it == colorMap.end()) {
      it = colorMap.insert (make_pair (
          dsize, vector<Color> (dsize + 1,-1))).first; 
    }
    unsigned idx;
    if (varNodes[i]->hasEvidence()) {
      idx = varNodes[i]->getEvidence();
    } else {
      idx = dsize;
    }
    vector<Color>& stateColors = it->second;
    if (stateColors[idx] == -1) {
      stateColors[idx] = getFreeColor();
    }
    setColor (varNodes[i], stateColors[idx]);
  }

  // create the initial factor colors
  DistColorMap distColors;
  for (unsigned i = 0; i < factors.size(); i++) {
    Distribution* dist = factors[i]->getDistribution();
    DistColorMap::iterator it = distColors.find (dist);
    if (it == distColors.end()) {
      it = distColors.insert (make_pair (dist, getFreeColor())).first;
    }
    setColor (factors[i], it->second);
  }

  VarSignMap    varGroups;
  FactorSignMap factorGroups;
  bool groupsHaveChanged = true;
  unsigned nIter = 0;
  while (groupsHaveChanged || nIter == 1) {
    nIter ++;
    if (Statistics::numCreatedNets == 4) {
      cout << "--------------------------------------------" << endl;
      cout << "Iteration " << nIter << endl;
      cout << "--------------------------------------------" << endl;
    }

    unsigned prevFactorGroupsSize = factorGroups.size();
    factorGroups.clear();
    // set a new color to the factors with the same signature
    for (unsigned i = 0; i < factors.size(); i++) {
      const string& signatureId = getSignatureId (factors[i]);
      // cout << factors[i]->getLabel() << " signature: " ;
      // cout<< signatureId << endl;
      FactorSignMap::iterator it = factorGroups.find (signatureId);
      if (it == factorGroups.end()) {
        it = factorGroups.insert (make_pair (signatureId, FactorSet())).first;
      }
      it->second.push_back (factors[i]);
    }
    if (nIter > 0)
    for (FactorSignMap::iterator it = factorGroups.begin();
        it != factorGroups.end(); it++) {
      Color newColor = getFreeColor();
      FactorSet& groupMembers = it->second;
      for (unsigned i = 0; i < groupMembers.size(); i++) {
        setColor (groupMembers[i], newColor);
      }
    }

    // set a new color to the variables with the same signature
    unsigned prevVarGroupsSize = varGroups.size();
    varGroups.clear();
    for (unsigned i = 0; i < varNodes.size(); i++) {
      const string& signatureId = getSignatureId (varNodes[i]);
      VarSignMap::iterator it = varGroups.find (signatureId);
      // cout << varNodes[i]->getLabel() << " signature: " ;
      // cout << signatureId << endl;
      if (it == varGroups.end()) {
        it = varGroups.insert (make_pair (signatureId, FgVarSet())).first;
      }
      it->second.push_back (varNodes[i]);
    }
    if (nIter > 0)
    for (VarSignMap::iterator it = varGroups.begin();
        it != varGroups.end(); it++) {
      Color newColor = getFreeColor();
      FgVarSet& groupMembers = it->second;
      for (unsigned i = 0; i < groupMembers.size(); i++) {
        setColor (groupMembers[i], newColor);
      }
    }

    //if (nIter >= 3) cout << "bigger than three: " << nIter << endl;
    groupsHaveChanged = prevVarGroupsSize != varGroups.size()
        || prevFactorGroupsSize != factorGroups.size();
  }

  printGroups (varGroups, factorGroups);
  for (VarSignMap::iterator it = varGroups.begin();
      it != varGroups.end(); it++) {
    CFgVarSet vars = it->second;
    VarCluster* vc = new VarCluster (vars);
    for (unsigned i = 0; i < vars.size(); i++) {
      vid2VarCluster_.insert (make_pair (vars[i]->getVarId(), vc));
    }
    varClusters_.push_back (vc);
  }

  for (FactorSignMap::iterator it = factorGroups.begin();
      it != factorGroups.end(); it++) {
    VarClusterSet varClusters;
    Factor* groundFactor = it->second[0];
    FgVarSet groundVars = groundFactor->getFgVarNodes();
    for (unsigned i = 0; i < groundVars.size(); i++) {
      Vid vid = groundVars[i]->getVarId();
      varClusters.push_back (vid2VarCluster_.find (vid)->second);
    }
    factorClusters_.push_back (new FactorCluster (it->second, varClusters));
  }
}



LiftedFG::~LiftedFG (void)
{
  for (unsigned i = 0; i  < varClusters_.size(); i++) {
    delete varClusters_[i];
  }
  for (unsigned i = 0; i  < factorClusters_.size(); i++) {
    delete factorClusters_[i];
  }
}



string
LiftedFG::getSignatureId (FgVarNode* var) const
{
  stringstream ss;
  CFactorSet myFactors = var->getFactors();
  ss << myFactors.size();
  for (unsigned i = 0; i < myFactors.size(); i++) {
    ss << "." << getColor (myFactors[i]);
    ss << "." << myFactors[i]->getIndexOf(var);
  }
  ss << "." << getColor (var);
  return ss.str();
}



string
LiftedFG::getSignatureId (Factor* factor) const
{
  stringstream ss;
  CFgVarSet myVars = factor->getFgVarNodes();
  ss << myVars.size();
  for (unsigned i = 0; i < myVars.size(); i++) {
    ss << "." << getColor (myVars[i]);
  }
  ss << "." << getColor (factor);
  return ss.str();
}



FactorGraph*
LiftedFG::getCompressedFactorGraph (void)
{
  FactorGraph* fg = new FactorGraph();
  for (unsigned i = 0; i < varClusters_.size(); i++) {
    FgVarNode* var = varClusters_[i]->getGroundFgVarNodes()[0];
    FgVarNode* newVar = new FgVarNode (var);
    newVar->setIndex (i);
    varClusters_[i]->setRepresentativeVariable (newVar);
    fg->addVariable (newVar);
  }

  for (unsigned i = 0; i < factorClusters_.size(); i++) {
    FgVarSet myGroundVars;
    const VarClusterSet& myVarClusters = factorClusters_[i]->getVarClusters();
    for (unsigned j = 0; j < myVarClusters.size(); j++) {
      myGroundVars.push_back (myVarClusters[j]->getRepresentativeVariable());
    }
    Factor* newFactor = new Factor (myGroundVars,
        factorClusters_[i]->getGroundFactors()[0]->getDistribution());
    factorClusters_[i]->setRepresentativeFactor (newFactor);
    fg->addFactor (newFactor);
  }
  return fg;
}



unsigned
LiftedFG::getGroundEdgeCount (FactorCluster* fc, VarCluster* vc) const
{
  CFactorSet clusterGroundFactors = fc->getGroundFactors();
  FgVarNode* var = vc->getGroundFgVarNodes()[0];
  unsigned count = 0;
  for (unsigned i = 0; i < clusterGroundFactors.size(); i++) {
    if (clusterGroundFactors[i]->getIndexOf (var) != -1) {
      count ++;
    }
  }
  /*
  CFgVarSet vars = vc->getGroundFgVarNodes();
  for (unsigned i = 1; i < vars.size(); i++) {
    FgVarNode* var = vc->getGroundFgVarNodes()[i];
    unsigned count2 = 0;
    for (unsigned i = 0; i < clusterGroundFactors.size(); i++) {
      if (clusterGroundFactors[i]->getIndexOf (var) != -1) {
        count2 ++;
      }
    }
    if (count != count2) { cout << "oops!" << endl; abort(); }
  }
  */
  return count;
}



void
LiftedFG::printGroups (const VarSignMap& varGroups,
                       const FactorSignMap& factorGroups) const
{
  cout << "variable groups:" << endl;
  unsigned count = 0;
  for (VarSignMap::const_iterator it = varGroups.begin();
      it != varGroups.end(); it++) {
    const FgVarSet& groupMembers = it->second;
    if (groupMembers.size() > 0) {
      cout << ++count << ": " ;
      //if (groupMembers.size() > 1) {
        for (unsigned i = 0; i < groupMembers.size(); i++) {
          cout << groupMembers[i]->getLabel() << " " ;
        }
      //}
      cout << endl;
    }
  }
  cout << endl;
  cout << "factor groups:" << endl;
  count = 0;
  for (FactorSignMap::const_iterator it = factorGroups.begin();
      it != factorGroups.end(); it++) {
    const FactorSet& groupMembers = it->second;
    if (groupMembers.size() > 0) {
      cout << ++count << ": " ;
      //if (groupMembers.size() > 1) {
        for (unsigned i = 0; i < groupMembers.size(); i++) {
          cout << groupMembers[i]->getLabel() << " " ;
        }
    //}
    cout << endl;
    }
  }
}

