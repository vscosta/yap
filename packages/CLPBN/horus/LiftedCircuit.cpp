#include <fstream>

#include "LiftedCircuit.h"


double
OrNode::weight (void) const
{
  double lw = leftBranch_->weight();
  double rw = rightBranch_->weight();
  return Globals::logDomain ? Util::logSum (lw, rw) : lw + rw;
}



double
AndNode::weight (void) const
{
  double lw = leftBranch_->weight();
  double rw = rightBranch_->weight();
  return Globals::logDomain ? lw + rw : lw * rw;
}



stack<pair<unsigned, unsigned>> SetOrNode::nrGrsStack;



double
SetOrNode::weight (void) const
{
  double weightSum = LogAware::addIdenty();
  for (unsigned i = 0; i < nrGroundings_ + 1; i++) {
    nrGrsStack.push (make_pair (i, nrGroundings_ - i));
    if (Globals::logDomain) {
      double w = std::log (Util::nrCombinations (nrGroundings_, i));
      weightSum = Util::logSum (weightSum, w + follow_->weight());
    } else {
      weightSum += Util::nrCombinations (nrGroundings_, i) * follow_->weight();
    }
  }
  return weightSum;
}



double
SetAndNode::weight (void) const
{
  double w = follow_->weight();
  return Globals::logDomain
      ? w * nrGroundings_
      : std::pow (w, nrGroundings_);
}



double
IncExcNode::weight (void) const
{
  double w = 0.0;
  if (Globals::logDomain) {
    w = Util::logSum (plus1Branch_->weight(), plus2Branch_->weight());
    w = std::log (std::exp (w) - std::exp (minusBranch_->weight()));
  } else {
    w = plus1Branch_->weight() + plus2Branch_->weight();
    w -= minusBranch_->weight();
  }
  return w;
}



double
LeafNode::weight (void) const
{
  assert (clauses().size() == 1);
  assert (clauses()[0].isUnit());
  Clause c = clauses()[0];
  double weight = c.literals()[0].weight();
  LogVarSet lvs = c.constr().logVarSet();
  lvs -= c.ipgLogVars();
  lvs -= c.positiveCountedLogVars();
  lvs -= c.negativeCountedLogVars();
  unsigned nrGroundings = 1;
  if (lvs.empty() == false) {
    ConstraintTree ct = c.constr();
    ct.project (lvs);
    nrGroundings = ct.size();
  }
  // TODO this only works for one counted log var
  if (c.positiveCountedLogVars().empty() == false) {
    nrGroundings *= SetOrNode::nrPositives();
  } else if (c.negativeCountedLogVars().empty() == false) {
    nrGroundings *= SetOrNode::nrNegatives();    
  }
  return Globals::logDomain 
      ? weight * nrGroundings
      : std::pow (weight, nrGroundings);
}



double
SmoothNode::weight (void) const
{
  // TODO and what happens if smoothing contains ipg or counted lvs ?
  Clauses cs = clauses();
  double totalWeight = LogAware::multIdenty();
  for (size_t i = 0; i < cs.size(); i++) {
    double posWeight = cs[i].literals()[0].weight();
    double negWeight = cs[i].literals()[1].weight();
    unsigned nrGroundings = cs[i].constr().size();
    if (Globals::logDomain) {
      totalWeight += (Util::logSum (posWeight, negWeight) * nrGroundings);
    } else {
      totalWeight *= std::pow (posWeight + negWeight, nrGroundings);
    }
  }
  return totalWeight;
}



double
TrueNode::weight (void) const
{
  return LogAware::multIdenty();
}



double
CompilationFailedNode::weight (void) const
{
  // we should not perform model counting
  // in compilation failed nodes
  // abort();
  return 0.0;
}



LiftedCircuit::LiftedCircuit (const LiftedWCNF* lwcnf)
    : lwcnf_(lwcnf)
{
  root_ = 0;
  Clauses clauses = lwcnf->clauses();
  compile (&root_, clauses);
  exportToGraphViz("circuit.dot");
  smoothCircuit();
  exportToGraphViz("circuit.smooth.dot");
  cout << "WEIGHTED MODEL COUNT = " << getWeightedModelCount() << endl;
}



void
LiftedCircuit::smoothCircuit (void)
{
  smoothCircuit (root_);
}



double
LiftedCircuit::getWeightedModelCount (void) const
{
  return root_->weight();
}



void
LiftedCircuit::exportToGraphViz (const char* fileName)
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "BayesBallGraph::exportToDotFile()" << endl;
    abort();
  }
  out << "digraph {" << endl;
  out << "ranksep=1" << endl; 
  exportToGraphViz (root_, out);
  out << "}" << endl;
  out.close();
}



void
LiftedCircuit::compile (
    CircuitNode** follow,
    Clauses& clauses)
{
  if (clauses.empty()) {
    *follow = new TrueNode ();
    return;
  }
  
  if (clauses.size() == 1 && clauses[0].isUnit()) {
    *follow = new LeafNode (clauses[0]);
    return;
  }

  if (tryUnitPropagation (follow, clauses)) {
    return;
  }
  
  if (tryIndependence (follow, clauses)) {
    return;
  }
  
  if (tryShannonDecomp (follow, clauses)) {
    return;
  }
  
  if (tryInclusionExclusion (follow, clauses)) {
    return;
  }
  
  //if (tryIndepPartialGrounding (follow, clauses)) {
  //  return;
  //}
  
  if (tryAtomCounting (follow, clauses)) {
    return;
  }
  
  if (tryGrounding (follow, clauses)) {
    return;
  }
  
  // assert (false);
  *follow = new CompilationFailedNode (clauses);
}



bool
LiftedCircuit::tryUnitPropagation (
    CircuitNode** follow,
    Clauses& clauses)
{
  // cout << "ALL CLAUSES:" << endl;
  // Clause::printClauses (clauses);
  for (size_t i = 0; i < clauses.size(); i++) {
    if (clauses[i].isUnit()) {
      // cout << clauses[i] << " is unit!" << endl;
      Clauses newClauses;
      for (size_t j = 0; j < clauses.size(); j++) {
        if (i != j) {
          LiteralId lid = clauses[i].literals()[0].lid();
          LogVarTypes types = clauses[i].logVarTypes (0);
          if (clauses[i].literals()[0].isPositive()) {
            if (clauses[j].containsPositiveLiteral (lid, types) == false) {
              Clause newClause = clauses[j];
              newClause.removeNegativeLiterals (lid, types);
              newClauses.push_back (newClause);
            }
          } else if (clauses[i].literals()[0].isNegative()) {
            if (clauses[j].containsNegativeLiteral (lid, types) == false) {
              Clause newClause = clauses[j];
              newClause.removePositiveLiterals (lid, types);
              newClauses.push_back (newClause);
            }
          }
        }
      }
      stringstream explanation;
      explanation << " UP on" << clauses[i].literals()[0];
      AndNode* andNode = new AndNode (clauses, explanation.str());
      Clauses leftClauses = {clauses[i]};
      compile (andNode->leftBranch(), leftClauses);
      compile (andNode->rightBranch(), newClauses);
      (*follow) = andNode;
      return true;     
    }   
  }
  return false;
}



bool
LiftedCircuit::tryIndependence (
    CircuitNode** follow,
    Clauses& clauses)
{
  if (clauses.size() == 1) {
    return false;
  }
  // TODO this independence is a little weak
  for (size_t i = 0; i < clauses.size(); i++) {
    bool indep = true;
    TinySet<LiteralId> lids1 = clauses[i].lidSet();
    for (size_t j = 0; j < clauses.size(); j++) {
      TinySet<LiteralId> lids2 = clauses[j].lidSet(); 
      if (i != j && ((lids1 & lids2).empty() == false)) {
        indep = false;
        break;
      }
    }
    if (indep == true) {
      Clauses newClauses = clauses;
      newClauses.erase (newClauses.begin() + i);
      stringstream explanation;
      explanation << " Independence on clause Nº " << i ;
      AndNode* andNode = new AndNode (clauses, explanation.str());
      Clauses indepClause = {clauses[i]};
      compile (andNode->leftBranch(), indepClause);
      compile (andNode->rightBranch(), newClauses);
      (*follow) = andNode;
      return true;
    }
  }
  return false;
}



bool
LiftedCircuit::tryShannonDecomp (
    CircuitNode** follow,
    Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size(); i++) {
    const Literals& literals = clauses[i].literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].isGround (clauses[i].constr(),clauses[i].ipgLogVars())) {
        Literal posLit (literals[j], false);
        Literal negLit (literals[j], true);
        ConstraintTree ct1 = clauses[i].constr();
        ConstraintTree ct2 = clauses[i].constr();
        Clause c1 (ct1);
        Clause c2 (ct2);
        c1.addLiteral (posLit);
        c2.addLiteral (negLit);
        Clauses leftClauses  = { c1 };
        Clauses rightClauses = { c2 };
        leftClauses.insert (leftClauses.end(), clauses.begin(), clauses.end());
        rightClauses.insert (rightClauses.end(), clauses.begin(), clauses.end());
        stringstream explanation;
        explanation << " SD on " << literals[j];
        OrNode* orNode = new OrNode (clauses, explanation.str());
        compile (orNode->leftBranch(),  leftClauses);
        compile (orNode->rightBranch(), rightClauses);
        (*follow) = orNode;
        return true;   
      }
    }
  }
  return false;
}



bool
LiftedCircuit::tryInclusionExclusion (
    CircuitNode** follow,
    Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size(); i++) {
    const Literals& literals = clauses[i].literals();
    for (size_t j = 0; j < literals.size(); j++) {
      bool indep = true;
      for (size_t k = 0; k < literals.size(); k++) {
        LogVarSet intersect = literals[j].logVarSet()
            & literals[k].logVarSet();
        if (j != k && intersect.empty() == false) {
          indep = false;
          break;
        }
      }
      if (indep) {
        // TODO i am almost sure that this will
        // have to be count normalized too!
        ConstraintTree really = clauses[i].constr();
        Clause c1 (really.projectedCopy (
            literals[j].logVars()));
        c1.addLiteral (literals[j]);
        Clause c2 = clauses[i];
        c2.removeLiteral (j);
        Clauses plus1Clauses = clauses;
        Clauses plus2Clauses = clauses;
        Clauses minusClauses = clauses;
        plus1Clauses.erase (plus1Clauses.begin() + i);
        plus2Clauses.erase (plus2Clauses.begin() + i);
        minusClauses.erase (minusClauses.begin() + i);
        plus1Clauses.push_back (c1);
        plus2Clauses.push_back (c2);
        minusClauses.push_back (c1);
        minusClauses.push_back (c2);
        IncExcNode* ieNode = new IncExcNode (clauses);
        compile (ieNode->plus1Branch(), plus1Clauses);
        compile (ieNode->plus2Branch(), plus2Clauses);
        compile (ieNode->minusBranch(), minusClauses);
        *follow = ieNode;
        return true;
      }
    }
  }
  return false;
}



bool
LiftedCircuit::tryIndepPartialGrounding (
    CircuitNode** follow,
    Clauses& clauses)
{ 
  // assumes that all literals have logical variables
  // else, shannon decomp was possible
  vector<unsigned> lvIndices;
  LogVarSet lvs = clauses[0].ipgCandidates();
  for (size_t i = 0; i < lvs.size(); i++) {
    lvIndices.clear();
    lvIndices.push_back (i);
    ConstraintTree ct = clauses[0].constr();
    ct.project ({lvs[i]});
    if (tryIndepPartialGroundingAux (clauses, ct, lvIndices)) {
      Clauses newClauses = clauses;
      for (size_t i = 0; i < clauses.size(); i++) {
        LogVar lv = clauses[i].ipgCandidates()[lvIndices[i]];
        newClauses[i].addIpgLogVar (lv);
      }
      SetAndNode* node = new SetAndNode (ct.size(), clauses);
      *follow = node;
      compile (node->follow(), newClauses);
      return true;
    }
  }
  return false;
}



bool
LiftedCircuit::tryIndepPartialGroundingAux (
    Clauses& clauses,
    ConstraintTree& ct,
    vector<unsigned>& lvIndices)
{
  for (size_t j = 1; j < clauses.size(); j++) {
    LogVarSet lvs2 = clauses[j].ipgCandidates();
    for (size_t k = 0; k < lvs2.size(); k++) {
      ConstraintTree ct2 = clauses[j].constr();
      ct2.project ({lvs2[k]});
      if (ct.tupleSet() == ct2.tupleSet()) {
        lvIndices.push_back (k);
        break;
      }
    }
    if (lvIndices.size() != j+1) {
      return false;
    }
  }
  return true;
}



bool
LiftedCircuit::tryAtomCounting (
    CircuitNode** follow,
    Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size(); i++) {
    Literals literals = clauses[i].literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].logVars().size() == 1) {
        // TODO check if not already in ipg and countedlvs
        unsigned nrGroundings = clauses[i].constr().projectedCopy (
            literals[j].logVars()).size();
        SetOrNode* setOrNode = new SetOrNode (nrGroundings, clauses);
        Clause c1 (clauses[i].constr().projectedCopy (literals[j].logVars()));
        Clause c2 (clauses[i].constr().projectedCopy (literals[j].logVars()));
        c1.addLiteral (literals[j]);
        c2.addAndNegateLiteral (literals[j]);
        c1.addPositiveCountedLogVar (literals[j].logVars().front());
        c2.addNegativeCountedLogVar (literals[j].logVars().front());
        clauses.push_back (c1);
        clauses.push_back (c2);        
        shatterCountedLogVars (clauses);
        compile (setOrNode->follow(), clauses);
        *follow = setOrNode;
        return true;
      }
    }
  }
  return false;
}



bool
LiftedCircuit::tryGrounding (
    CircuitNode**,
    Clauses&)
{
  return false;
  /*
  size_t bestClauseIdx = 0;
  size_t bestLogVarIdx = 0;
  unsigned minNrSymbols = Util::maxUnsigned();
  for (size_t i = 0; i < clauses.size(); i++) {
    LogVarSet lvs = clauses[i].constr().logVars();
    ConstraintTree ct = clauses[i].constr();
    for (unsigned j = 0; j < lvs.size(); j++) {
      unsigned nrSymbols = ct.nrSymbols (lvs[j]);
      if (nrSymbols < minNrSymbols) {
        minNrSymbols = nrSymbols;
        bestClauseIdx = i;
        bestLogVarIdx = j;
      }
    }
  }
  LogVar bestLogVar = clauses[bestClauseIdx].constr().logVars()[bestLogVarIdx];
  ConstraintTrees cts = clauses[bestClauseIdx].constr().ground (bestLogVar);
  return true;
  */
}



void
LiftedCircuit::shatterCountedLogVars (Clauses& clauses)
{
  while (shatterCountedLogVarsAux (clauses)) ;
}



bool
LiftedCircuit::shatterCountedLogVarsAux (Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size() - 1; i++) {
    for (size_t j = i + 1; j < clauses.size(); j++) {
      bool splitedSome = shatterCountedLogVarsAux (clauses, i, j);
      if (splitedSome) {
        return true;
      }
    }
  }
  return false;  
}



bool
LiftedCircuit::shatterCountedLogVarsAux (
    Clauses& clauses,
    size_t idx1,
    size_t idx2)
{
  Literals lits1 = clauses[idx1].literals();
  Literals lits2 = clauses[idx2].literals();
  for (size_t i = 0; i < lits1.size(); i++) {
    for (size_t j = 0; j < lits2.size(); j++) {
      if (lits1[i].lid() == lits2[j].lid()) {
        LogVars lvs1 = lits1[i].logVars();
        LogVars lvs2 = lits2[j].logVars();
        for (size_t k = 0; k < lvs1.size(); k++) {
          if (clauses[idx1].isCountedLogVar (lvs1[k])
              && clauses[idx2].isCountedLogVar (lvs2[k]) == false) {
            clauses.push_back (clauses[idx2]);
            clauses[idx2].addPositiveCountedLogVar (lvs2[k]);
            clauses.back().addNegativeCountedLogVar (lvs2[k]);
            return true;
          }
          if (clauses[idx2].isCountedLogVar (lvs2[k])
              && clauses[idx1].isCountedLogVar (lvs1[k]) == false) {
            clauses.push_back (clauses[idx1]);
            clauses[idx1].addPositiveCountedLogVar (lvs1[k]);
            clauses.back().addNegativeCountedLogVar (lvs1[k]);
            return true;
          }          
        }
      }      
    }
  }
  return false;
}



LogVarTypes
unionTypes (const LogVarTypes& types1, const LogVarTypes& types2)
{
  if (types1.empty()) {
    return types2;
  }
  if (types2.empty()) {
    return types1;
  }  
  assert (types1.size() == types2.size());
  LogVarTypes res;  
  for (size_t i = 0; i < types1.size(); i++) {
    if (types1[i] == LogVarType::POS_LV
        && types2[i] == LogVarType::POS_LV) {
      res.push_back (LogVarType::POS_LV);        
    } else if (types1[i] == LogVarType::NEG_LV
        && types2[i] == LogVarType::NEG_LV) {
      res.push_back (LogVarType::NEG_LV);
    } else {
      res.push_back (LogVarType::FULL_LV);
    }
  }
  return res;
}



vector<LogVarTypes>
getAllPossibleTypes (unsigned nrLogVars)
{
  if (nrLogVars == 0) {
    return {};
  }
  if (nrLogVars == 1) {
    return {{LogVarType::POS_LV},{LogVarType::NEG_LV}};
  }
  vector<LogVarTypes> res;
  Indexer indexer (vector<unsigned> (nrLogVars, 2));
  while (indexer.valid()) {
    LogVarTypes types;
    for (size_t i = 0; i < nrLogVars; i++) {
      if (indexer[i] == 0) {
        types.push_back (LogVarType::POS_LV);
      } else {
        types.push_back (LogVarType::NEG_LV);
      }
    }
    res.push_back (types);
    ++ indexer;
  }
  return res;
}



bool
containsTypes (const LogVarTypes& typesA, const LogVarTypes& typesB)
{
  for (size_t i = 0; i < typesA.size(); i++) {
    if (typesA[i] == LogVarType::FULL_LV) {

    } else if (typesA[i] == LogVarType::POS_LV
        && typesB[i] == LogVarType::POS_LV) {

    } else if (typesA[i] == LogVarType::NEG_LV
        && typesB[i] == LogVarType::NEG_LV) {

    } else {
      return false;
    }
  }
  return true;
}



LitLvTypesSet
LiftedCircuit::smoothCircuit (CircuitNode* node)
{
  assert (node != 0);
  LitLvTypesSet propagLits;

  switch (getCircuitNodeType (node)) {

    case CircuitNodeType::OR_NODE: {
      OrNode* casted = dynamic_cast<OrNode*>(node);
      LitLvTypesSet lids1 = smoothCircuit (*casted->leftBranch());
      LitLvTypesSet lids2 = smoothCircuit (*casted->rightBranch());
      LitLvTypesSet missingLeft  = lids2 - lids1;
      LitLvTypesSet missingRight = lids1 - lids2;
      createSmoothNode (missingLeft,  casted->leftBranch());
      createSmoothNode (missingRight, casted->rightBranch());
      propagLits |= lids1;
      propagLits |= lids2;
      break;
    }
  
    case CircuitNodeType::AND_NODE: {
      AndNode* casted = dynamic_cast<AndNode*>(node);
      LitLvTypesSet lids1 = smoothCircuit (*casted->leftBranch());
      LitLvTypesSet lids2 = smoothCircuit (*casted->rightBranch());
      propagLits |= lids1;
      propagLits |= lids2;
      break;
    }

    case CircuitNodeType::SET_OR_NODE: {
      SetOrNode* casted = dynamic_cast<SetOrNode*>(node);
      propagLits = smoothCircuit (*casted->follow());      
      TinySet<pair<LiteralId,unsigned>> litSet;
      for (size_t i = 0; i < propagLits.size(); i++) {
        litSet.insert (make_pair (propagLits[i].lid(),
            propagLits[i].logVarTypes().size()));
      }
      LitLvTypesSet missingLids;
      for (size_t i = 0; i < litSet.size(); i++) {
        vector<LogVarTypes> allTypes = getAllPossibleTypes (litSet[i].second);
        for (size_t j = 0; j < allTypes.size(); j++) {
          bool typeFound = false;
          for (size_t k = 0; k < propagLits.size(); k++) {
            if (litSet[i].first == propagLits[k].lid()
                && containsTypes (allTypes[j], propagLits[k].logVarTypes())) {
              typeFound = true;
              break;
            }
          }
          if (typeFound == false) {
            missingLids.insert (LiteralLvTypes (litSet[i].first, allTypes[j]));
          }
        }
      }
      createSmoothNode (missingLids, casted->follow());
      // TODO change propagLits to full lvs
      break;
    }
    
    case CircuitNodeType::SET_AND_NODE: {
      SetAndNode* casted = dynamic_cast<SetAndNode*>(node);
      propagLits = smoothCircuit (*casted->follow());
      break;
    }
    
    case CircuitNodeType::INC_EXC_NODE: {
      IncExcNode* casted = dynamic_cast<IncExcNode*>(node);
      LitLvTypesSet lids1 = smoothCircuit (*casted->plus1Branch());
      LitLvTypesSet lids2 = smoothCircuit (*casted->plus2Branch());
      LitLvTypesSet missingPlus1 = lids2 - lids1;
      LitLvTypesSet missingPlus2 = lids1 - lids2;
      createSmoothNode (missingPlus1, casted->plus1Branch());
      createSmoothNode (missingPlus2, casted->plus2Branch());
      propagLits |= lids1;
      propagLits |= lids2;
      break;
    }
    
    case CircuitNodeType::LEAF_NODE: {
      propagLits.insert (LiteralLvTypes (
          node->clauses()[0].literals()[0].lid(),
          node->clauses()[0].logVarTypes(0)));
    }
 
    default:
      break;
  }
  
  return propagLits;
}



void
LiftedCircuit::createSmoothNode (
    const LitLvTypesSet& missingLits,
    CircuitNode** prev)
{
  if (missingLits.empty() == false) {
    Clauses clauses;
    for (size_t i = 0; i < missingLits.size(); i++) {
      LiteralId lid = missingLits[i].lid();
      const LogVarTypes& types = missingLits[i].logVarTypes();
      Clause c = lwcnf_->createClauseForLiteral (lid);
      for (size_t j = 0; j < types.size(); j++) {
        LogVar X = c.literals().front().logVars()[j];
        if (types[j] == LogVarType::POS_LV) {
          c.addPositiveCountedLogVar (X);
        } else if (types[j] == LogVarType::NEG_LV) {
          c.addNegativeCountedLogVar (X);
        }
      }
      c.addAndNegateLiteral (c.literals()[0]);
      clauses.push_back (c);
    }
    SmoothNode* smoothNode = new SmoothNode (clauses);
    *prev = new AndNode ((*prev)->clauses(), smoothNode,
        *prev, " Smoothing");
  }
}



CircuitNodeType
LiftedCircuit::getCircuitNodeType (const CircuitNode* node) const
{
  CircuitNodeType type;
  if (dynamic_cast<const OrNode*>(node) != 0) {
    type = CircuitNodeType::OR_NODE;
  } else if (dynamic_cast<const AndNode*>(node) != 0) {
    type = CircuitNodeType::AND_NODE;
  } else if (dynamic_cast<const SetOrNode*>(node) != 0) {
    type = CircuitNodeType::SET_OR_NODE;
  } else if (dynamic_cast<const SetAndNode*>(node) != 0) {
    type = CircuitNodeType::SET_AND_NODE;
  } else if (dynamic_cast<const IncExcNode*>(node) != 0) {
    type = CircuitNodeType::INC_EXC_NODE;
  } else if (dynamic_cast<const LeafNode*>(node) != 0) {
    type = CircuitNodeType::LEAF_NODE;
  } else if (dynamic_cast<const SmoothNode*>(node) != 0) {
    type = CircuitNodeType::SMOOTH_NODE;
  } else if (dynamic_cast<const TrueNode*>(node) != 0) {
    type = CircuitNodeType::TRUE_NODE;
  } else if (dynamic_cast<const CompilationFailedNode*>(node) != 0) {
    type = CircuitNodeType::COMPILATION_FAILED_NODE;
  } else {
    assert (false);
  }
  return type;
}



void
LiftedCircuit::exportToGraphViz (CircuitNode* node, ofstream& os)
{
  assert (node != 0);

  static unsigned nrAuxNodes = 0;  
  stringstream ss;
  ss << "n" << nrAuxNodes;
  string auxNode = ss.str();
  nrAuxNodes ++;


  switch (getCircuitNodeType (node)) {
  
    case OR_NODE: {
      OrNode* casted = dynamic_cast<OrNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [label=\"∨\"]" << endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << node->explanation() << "\"]" ;
      os << endl;
      
      os << auxNode << " -> " ;
      os << escapeNode (*casted->leftBranch());
      os << " [label=\" " << (*casted->leftBranch())->weight() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->rightBranch());
      os << " [label=\" " << (*casted->rightBranch())->weight() << "\"]" ;
      os << endl;

      exportToGraphViz (*casted->leftBranch(),  os);
      exportToGraphViz (*casted->rightBranch(), os);
      break;
    }

    case AND_NODE: {
      AndNode* casted = dynamic_cast<AndNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [label=\"∧\"]" << endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << node->explanation() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->leftBranch());
      os << " [label=\" " << (*casted->leftBranch())->weight() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->rightBranch()) << endl;
      os << " [label=\" " << (*casted->rightBranch())->weight() << "\"]" ;
      os << endl;

      exportToGraphViz (*casted->leftBranch(),  os);
      exportToGraphViz (*casted->rightBranch(), os);
      break;
    }

    case SET_OR_NODE: {
      SetOrNode* casted = dynamic_cast<SetOrNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [label=\"∨(X)\"]" << endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << node->explanation() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->follow());
      os << " [label=\" " << (*casted->follow())->weight() << "\"]" ;
      os << endl;

      exportToGraphViz (*casted->follow(),  os);
      break;
    }

    case SET_AND_NODE: {
      SetAndNode* casted = dynamic_cast<SetAndNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [label=\"∧(X)\"]" << endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << node->explanation() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->follow());
      os << " [label=\" " << (*casted->follow())->weight() << "\"]" ;
      os << endl;

      exportToGraphViz (*casted->follow(),  os);
      break;
    }

    case INC_EXC_NODE: {
      IncExcNode* casted = dynamic_cast<IncExcNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [label=\"IncExc\"]" << endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << node->explanation() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->plus1Branch());
      os << " [label=\" " << (*casted->plus1Branch())->weight() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->plus2Branch());
      os << " [label=\" " << (*casted->plus2Branch())->weight() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->minusBranch()) << endl;
      os << " [label=\" " << (*casted->minusBranch())->weight() << "\"]" ;
      os << endl;

      exportToGraphViz (*casted->plus1Branch(), os);
      exportToGraphViz (*casted->plus2Branch(), os);
      exportToGraphViz (*casted->minusBranch(), os);
      break;
    }

    case LEAF_NODE: {
      printClauses (node, os, "style=filled,fillcolor=palegreen,");
      break;
    }

    case SMOOTH_NODE: {
      printClauses (node, os, "style=filled,fillcolor=lightblue,");
      break;
    }

    case TRUE_NODE: {
      os << escapeNode (node);
      os << " [shape=box,label=\"⊤\"]" ;
      os << endl;
      break;
    }

    case COMPILATION_FAILED_NODE: {
      printClauses (node, os, "style=filled,fillcolor=salmon,");
      break;
    }

    default:
      assert (false);
  }
}



string
LiftedCircuit::escapeNode (const CircuitNode* node) const
{
  stringstream ss;
  ss << "\"" << node << "\"" ;
  return ss.str();
}



void
LiftedCircuit::printClauses (
    const CircuitNode* node,
    ofstream& os,
    string extraOptions)
{
  const Clauses& clauses = node->clauses();
  if (node->clauses().empty() == false) {
    os << escapeNode (node);
    os << " [shape=box," << extraOptions << "label=\"" ;  
    for (size_t i = 0; i < clauses.size(); i++) {
      if (i != 0) os << "\\n" ;
      os << clauses[i];
    }
    os << "\"]" ;
    os << endl;
  }
}

