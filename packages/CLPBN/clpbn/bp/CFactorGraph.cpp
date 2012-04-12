
#include "CFactorGraph.h"
#include "Factor.h"


bool CFactorGraph::checkForIdenticalFactors = true;

CFactorGraph::CFactorGraph (const FactorGraph& fg)
{
  groundFg_  = &fg;
  freeColor_ = 0;

  const VarNodes& varNodes = fg.varNodes();
  varSignatures_.reserve (varNodes.size());
  for (unsigned i = 0; i < varNodes.size(); i++) {
    unsigned c = (varNodes[i]->neighbors().size() * 2) + 1;
    varSignatures_.push_back (Signature (c));
  }

  const FacNodes& facNodes = fg.facNodes();
  facSignatures_.reserve (facNodes.size());
  for (unsigned i = 0; i < facNodes.size(); i++) {
    unsigned c = facNodes[i]->neighbors().size() + 1;
    facSignatures_.push_back (Signature (c));
  }

  varColors_.resize (varNodes.size());
  facColors_.resize (facNodes.size());
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
  const VarNodes& varNodes = groundFg_->varNodes();
  for (unsigned i = 0; i < varNodes.size(); i++) {
    unsigned dsize = varNodes[i]->range();
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

  const FacNodes& facNodes = groundFg_->facNodes();
  for (unsigned i = 0; i < facNodes.size(); i++) {
    facNodes[i]->factor().setDistId (Util::maxUnsigned());
  }
  // FIXME FIXME FIXME : pfl should give correct dist ids.
  if (checkForIdenticalFactors || true) {
    unsigned groupCount = 1;
    for (unsigned i = 0; i < facNodes.size(); i++) {
      Factor& f1 = facNodes[i]->factor();
      if (f1.distId() != Util::maxUnsigned()) {
        continue;
      }
      f1.setDistId (groupCount);
      for (unsigned j = i + 1; j < facNodes.size(); j++) {
        Factor& f2 = facNodes[j]->factor();
        if (f2.distId() != Util::maxUnsigned()) {
          continue;
        }
        if (f1.size()   == f2.size()   &&
            f1.ranges() == f2.ranges() &&
            f1.params() == f2.params()) {
          f2.setDistId (groupCount);
        }
      }
      groupCount ++;
    }
  }
  // create the initial factor colors
  DistColorMap distColors;
  for (unsigned i = 0; i < facNodes.size(); i++) {
    unsigned distId = facNodes[i]->factor().distId();
    DistColorMap::iterator it = distColors.find (distId);
    if (it == distColors.end()) {
      it = distColors.insert (make_pair (distId, getFreeColor())).first;
    }
    setColor (facNodes[i], it->second);
  }
}



void
CFactorGraph::createGroups (void)
{
  VarSignMap varGroups;
  FacSignMap facGroups;
  unsigned nIters = 0;
  bool groupsHaveChanged = true;
  const VarNodes& varNodes = groundFg_->varNodes();
  const FacNodes& facNodes = groundFg_->facNodes();

  while (groupsHaveChanged || nIters == 1) {
    nIters ++;

    unsigned prevFactorGroupsSize = facGroups.size();
    facGroups.clear();
    // set a new color to the factors with the same signature
    for (unsigned i = 0; i < facNodes.size(); i++) {
      const Signature& signature = getSignature (facNodes[i]);
      FacSignMap::iterator it = facGroups.find (signature);
      if (it == facGroups.end()) {
        it = facGroups.insert (make_pair (signature, FacNodes())).first;
      }
      it->second.push_back (facNodes[i]);
    }
    for (FacSignMap::iterator it = facGroups.begin();
        it != facGroups.end(); it++) {
      Color newColor = getFreeColor();
      FacNodes& groupMembers = it->second;
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
        it = varGroups.insert (make_pair (signature, VarNodes())).first;
      }
      it->second.push_back (varNodes[i]);
    }
    for (VarSignMap::iterator it = varGroups.begin();
        it != varGroups.end(); it++) {
      Color newColor = getFreeColor();
      VarNodes& groupMembers = it->second;
      for (unsigned i = 0; i < groupMembers.size(); i++) {
        setColor (groupMembers[i], newColor);
      }
    }

    groupsHaveChanged = prevVarGroupsSize != varGroups.size()
        || prevFactorGroupsSize != facGroups.size();
  }
  // printGroups (varGroups, facGroups);
  createClusters (varGroups, facGroups);
}



void
CFactorGraph::createClusters (
    const VarSignMap& varGroups,
    const FacSignMap& facGroups)
{
  varClusters_.reserve (varGroups.size());
  for (VarSignMap::const_iterator it = varGroups.begin();
      it != varGroups.end(); it++) {
    const VarNodes& groupVars = it->second;
    VarCluster* vc = new VarCluster (groupVars);
    for (unsigned i = 0; i < groupVars.size(); i++) {
      vid2VarCluster_.insert (make_pair (groupVars[i]->varId(), vc));
    }
    varClusters_.push_back (vc);
  }

  facClusters_.reserve (facGroups.size());
  for (FacSignMap::const_iterator it = facGroups.begin();
      it != facGroups.end(); it++) {
    FacNode* groupFactor = it->second[0];
    const VarNodes& neighs = groupFactor->neighbors();
    VarClusters varClusters;
    varClusters.reserve (neighs.size());
    for (unsigned i = 0; i < neighs.size(); i++) {
      VarId vid = neighs[i]->varId();
      varClusters.push_back (vid2VarCluster_.find (vid)->second);
    }
    facClusters_.push_back (new FacCluster (it->second, varClusters));
  }
}



const Signature&
CFactorGraph::getSignature (const VarNode* varNode)
{
  Signature& sign = varSignatures_[varNode->getIndex()];
  vector<Color>::iterator it = sign.colors.begin();
  const FacNodes& neighs = varNode->neighbors();
  for (unsigned i = 0; i < neighs.size(); i++) {
    *it = getColor (neighs[i]);
    it ++;
    *it = neighs[i]->factor().indexOf (varNode->varId());
    it ++;
  }
  *it = getColor (varNode);
  return sign;
}



const Signature&
CFactorGraph::getSignature (const FacNode* facNode)
{
  Signature& sign = facSignatures_[facNode->getIndex()];
  vector<Color>::iterator it = sign.colors.begin();
  const VarNodes& neighs = facNode->neighbors();
  for (unsigned i = 0; i < neighs.size(); i++) {
    *it = getColor (neighs[i]);
    it ++;
  }
  *it = getColor (facNode);
  return sign;
}



FactorGraph*
CFactorGraph::getGroundFactorGraph (void) const
{
  FactorGraph* fg = new FactorGraph();
  for (unsigned i = 0; i < varClusters_.size(); i++) {
    VarNode* var = varClusters_[i]->getGroundVarNodes()[0];
    VarNode* newVar = new VarNode (var);
    varClusters_[i]->setRepresentativeVariable (newVar);
    fg->addVarNode (newVar);
  }

  for (unsigned i = 0; i < facClusters_.size(); i++) {
    const VarClusters& myVarClusters = facClusters_[i]->getVarClusters();
   Vars myGroundVars;
    myGroundVars.reserve (myVarClusters.size());
    for (unsigned j = 0; j < myVarClusters.size(); j++) {
      VarNode* v = myVarClusters[j]->getRepresentativeVariable();
      myGroundVars.push_back (v);
    }
    FacNode* fn = new FacNode (Factor (myGroundVars,
        facClusters_[i]->getGroundFactors()[0]->factor().params()));
    facClusters_[i]->setRepresentativeFactor (fn);
    fg->addFacNode (fn);
    for (unsigned j = 0; j < myGroundVars.size(); j++) {
      fg->addEdge (static_cast<VarNode*> (myGroundVars[j]), fn);
    }
  }
  return fg;
}



unsigned
CFactorGraph::getEdgeCount (
    const FacCluster* fc,
    const VarCluster* vc) const
{
  unsigned count = 0;
  VarId vid = vc->getGroundVarNodes().front()->varId();
  const FacNodes& clusterGroundFactors = fc->getGroundFactors();
  for (unsigned i = 0; i < clusterGroundFactors.size(); i++) {
    if (clusterGroundFactors[i]->factor().contains (vid)) {
      count ++;
    }
  }
  // CVarNodes vars = vc->getGroundVarNodes();
  // for (unsigned i = 1; i < vars.size(); i++) {
  //   VarNode* var = vc->getGroundVarNodes()[i];
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
CFactorGraph::printGroups (
    const VarSignMap& varGroups,
    const FacSignMap& facGroups) const
{
  unsigned count = 1;
  cout << "variable groups:" << endl;
  for (VarSignMap::const_iterator it = varGroups.begin();
      it != varGroups.end(); it++) {
    const VarNodes& groupMembers = it->second;
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
  for (FacSignMap::const_iterator it = facGroups.begin();
      it != facGroups.end(); it++) {
    const FacNodes& groupMembers = it->second;
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

