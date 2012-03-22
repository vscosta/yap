
#include "CFactorGraph.h"
#include "Factor.h"
#include "Distribution.h"


bool CFactorGraph::checkForIdenticalFactors = true;

CFactorGraph::CFactorGraph (const FactorGraph& fg)
{
  groundFg_  = &fg;
  freeColor_ = 0;

  const FgVarSet& varNodes = fg.getVarNodes();
  varSignatures_.reserve (varNodes.size());
  for (unsigned i = 0; i < varNodes.size(); i++) {
    unsigned c = (varNodes[i]->neighbors().size() * 2) + 1;
    varSignatures_.push_back (Signature (c));
  }

  const FgFacSet& facNodes = fg.getFactorNodes();
  factorSignatures_.reserve (facNodes.size());
  for (unsigned i = 0; i < facNodes.size(); i++) {
    unsigned c = facNodes[i]->neighbors().size() + 1;
    factorSignatures_.push_back (Signature (c));
  }

  varColors_.resize (varNodes.size());
  factorColors_.resize (facNodes.size());
  setInitialColors();
  createGroups();
}



CFactorGraph::~CFactorGraph (void)
{
  for (unsigned i = 0; i  < varClusters_.size(); i++) {
    delete varClusters_[i];
  }
  for (unsigned i = 0; i  < facClusters_.size(); i++) {
    delete facClusters_[i];
  }
}



void
CFactorGraph::setInitialColors (void)
{
  // create the initial variable colors
  VarColorMap colorMap;
  const FgVarSet& varNodes = groundFg_->getVarNodes();
  for (unsigned i = 0; i < varNodes.size(); i++) {
    unsigned dsize = varNodes[i]->nrStates();
    VarColorMap::iterator it = colorMap.find (dsize);
    if (it == colorMap.end()) {
      it = colorMap.insert (make_pair (
          dsize, vector<Color> (dsize+1,-1))).first; 
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

  const FgFacSet& facNodes = groundFg_->getFactorNodes();
  if (checkForIdenticalFactors) {
    for (unsigned i = 0, s = facNodes.size(); i < s; i++) {
      Distribution* dist1 = facNodes[i]->getDistribution();
      for (unsigned j = 0; j < i; j++) {
        Distribution* dist2 = facNodes[j]->getDistribution();
        if (dist1 != dist2 && dist1->params == dist2->params) {
          if (facNodes[i]->factor()->getRanges() == 
              facNodes[j]->factor()->getRanges()) {
            facNodes[i]->factor()->setDistribution (dist2);
          }
        }
      }
    }
  }

  // create the initial factor colors
  DistColorMap distColors;
  for (unsigned i = 0; i < facNodes.size(); i++) {
    const Distribution* dist = facNodes[i]->getDistribution();
    DistColorMap::iterator it = distColors.find (dist);
    if (it == distColors.end()) {
      it = distColors.insert (make_pair (dist, getFreeColor())).first;
    }
    setColor (facNodes[i], it->second);
  }
}



void
CFactorGraph::createGroups (void)
{
  VarSignMap    varGroups;
  FacSignMap factorGroups;
  unsigned nIters = 0;
  bool groupsHaveChanged = true;
  const FgVarSet&    varNodes = groundFg_->getVarNodes();
  const FgFacSet& facNodes = groundFg_->getFactorNodes();

  while (groupsHaveChanged || nIters == 1) {
    nIters ++;

    unsigned prevFactorGroupsSize = factorGroups.size();
    factorGroups.clear();
    // set a new color to the factors with the same signature
    for (unsigned i = 0; i < facNodes.size(); i++) {
      const Signature& signature = getSignature (facNodes[i]);
      FacSignMap::iterator it = factorGroups.find (signature);
      if (it == factorGroups.end()) {
        it = factorGroups.insert (make_pair (signature, FgFacSet())).first;
      }
      it->second.push_back (facNodes[i]);
    }
    for (FacSignMap::iterator it = factorGroups.begin();
        it != factorGroups.end(); it++) {
      Color newColor = getFreeColor();
      FgFacSet& groupMembers = it->second;
      for (unsigned i = 0; i < groupMembers.size(); i++) {
        setColor (groupMembers[i], newColor);
      }
    }

    // set a new color to the variables with the same signature
    unsigned prevVarGroupsSize = varGroups.size();
    varGroups.clear();
    for (unsigned i = 0; i < varNodes.size(); i++) {
      const Signature& signature = getSignature (varNodes[i]);
      VarSignMap::iterator it = varGroups.find (signature);
      if (it == varGroups.end()) {
        it = varGroups.insert (make_pair (signature, FgVarSet())).first;
      }
      it->second.push_back (varNodes[i]);
    }
    for (VarSignMap::iterator it = varGroups.begin();
        it != varGroups.end(); it++) {
      Color newColor = getFreeColor();
      FgVarSet& groupMembers = it->second;
      for (unsigned i = 0; i < groupMembers.size(); i++) {
        setColor (groupMembers[i], newColor);
      }
    }

    groupsHaveChanged = prevVarGroupsSize != varGroups.size()
        || prevFactorGroupsSize != factorGroups.size();
  }
  //printGroups (varGroups, factorGroups);
  createClusters (varGroups, factorGroups);
}



void
CFactorGraph::createClusters (const VarSignMap& varGroups,
                          const FacSignMap& factorGroups)
{
  varClusters_.reserve (varGroups.size());
  for (VarSignMap::const_iterator it = varGroups.begin();
      it != varGroups.end(); it++) {
    const FgVarSet& groupVars = it->second;
    VarCluster* vc = new VarCluster (groupVars);
    for (unsigned i = 0; i < groupVars.size(); i++) {
      vid2VarCluster_.insert (make_pair (groupVars[i]->varId(), vc));
    }
    varClusters_.push_back (vc);
  }

  facClusters_.reserve (factorGroups.size());
  for (FacSignMap::const_iterator it = factorGroups.begin();
      it != factorGroups.end(); it++) {
    FgFacNode* groupFactor = it->second[0];
    const FgVarSet& neighs = groupFactor->neighbors();
    VarClusterSet varClusters;
    varClusters.reserve (neighs.size());
    for (unsigned i = 0; i < neighs.size(); i++) {
      VarId vid = neighs[i]->varId();
      varClusters.push_back (vid2VarCluster_.find (vid)->second);
    }
    facClusters_.push_back (new FacCluster (it->second, varClusters));
  }
}



const Signature&
CFactorGraph::getSignature (const FgVarNode* varNode)
{
  Signature& sign = varSignatures_[varNode->getIndex()];
  vector<Color>::iterator it = sign.colors.begin();
  const FgFacSet& neighs = varNode->neighbors();
  for (unsigned i = 0; i < neighs.size(); i++) {
    *it = getColor (neighs[i]);
    it ++;
    *it = neighs[i]->factor()->indexOf (varNode->varId());
    it ++;
  }
  *it = getColor (varNode);
  return sign;
}



const Signature&
CFactorGraph::getSignature (const FgFacNode* facNode)
{
  Signature& sign = factorSignatures_[facNode->getIndex()];
  vector<Color>::iterator it = sign.colors.begin();
  const FgVarSet& neighs = facNode->neighbors();
  for (unsigned i = 0; i < neighs.size(); i++) {
    *it = getColor (neighs[i]);
    it ++;
  }
  *it = getColor (facNode);
  return sign;
}



FactorGraph*
CFactorGraph::getCompressedFactorGraph (void)
{
  FactorGraph* fg = new FactorGraph();
  for (unsigned i = 0; i < varClusters_.size(); i++) {
    FgVarNode* var = varClusters_[i]->getGroundFgVarNodes()[0];
    FgVarNode* newVar = new FgVarNode (var);
    varClusters_[i]->setRepresentativeVariable (newVar);
    fg->addVariable (newVar);
  }

  for (unsigned i = 0; i < facClusters_.size(); i++) {
    const VarClusterSet& myVarClusters = facClusters_[i]->getVarClusters();
    VarNodes myGroundVars;
    myGroundVars.reserve (myVarClusters.size());
    for (unsigned j = 0; j < myVarClusters.size(); j++) {
      FgVarNode* v = myVarClusters[j]->getRepresentativeVariable();
      myGroundVars.push_back (v);
    }
    Factor* newFactor = new Factor (myGroundVars,
        facClusters_[i]->getGroundFactors()[0]->getDistribution());
    FgFacNode* fn = new FgFacNode (newFactor);
    facClusters_[i]->setRepresentativeFactor (fn);
    fg->addFactor (fn);
    for (unsigned j = 0; j < myGroundVars.size(); j++) {
      fg->addEdge (fn, static_cast<FgVarNode*> (myGroundVars[j]));
    }
  }
  fg->setIndexes();
  return fg;
}



unsigned
CFactorGraph::getGroundEdgeCount (
    const FacCluster* fc,
    const VarCluster* vc) const
{
  const FgFacSet& clusterGroundFactors = fc->getGroundFactors();
  FgVarNode* varNode = vc->getGroundFgVarNodes()[0];
  unsigned count = 0;
  for (unsigned i = 0; i < clusterGroundFactors.size(); i++) {
    if (clusterGroundFactors[i]->factor()->indexOf (varNode->varId()) != -1) {
      count ++;
    }
  }
  // CFgVarSet vars = vc->getGroundFgVarNodes();
  // for (unsigned i = 1; i < vars.size(); i++) {
  //   FgVarNode* var = vc->getGroundFgVarNodes()[i];
  //   unsigned count2 = 0;
  //   for (unsigned i = 0; i < clusterGroundFactors.size(); i++) {
  //     if (clusterGroundFactors[i]->getPosition (var) != -1) {
  //       count2 ++;
  //     }
  //   }
  //   if (count != count2) { cout << "oops!" << endl; abort(); }
  // }
  return count;
}



void
CFactorGraph::printGroups (const VarSignMap&    varGroups,
                       const FacSignMap& factorGroups) const
{
  unsigned count = 1;
  cout << "variable groups:" << endl;
  for (VarSignMap::const_iterator it = varGroups.begin();
      it != varGroups.end(); it++) {
    const FgVarSet& groupMembers = it->second;
    if (groupMembers.size() > 0) {
      cout << count << ": " ;
      for (unsigned i = 0; i < groupMembers.size(); i++) {
        cout << groupMembers[i]->label() << " " ;
      }
      count ++;
      cout << endl;
    }
  }

  count = 1;
  cout << endl << "factor groups:" << endl;
  for (FacSignMap::const_iterator it = factorGroups.begin();
      it != factorGroups.end(); it++) {
    const FgFacSet& groupMembers = it->second;
    if (groupMembers.size() > 0) {
      cout << ++count << ": " ;
      for (unsigned i = 0; i < groupMembers.size(); i++) {
        cout << groupMembers[i]->getLabel() << " " ;
      }
      count ++;
      cout << endl;
    }
  }
}

