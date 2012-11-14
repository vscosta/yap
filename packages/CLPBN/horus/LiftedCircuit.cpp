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
    nrGrsStack.push (make_pair (nrGroundings_ - i, i));
    if (Globals::logDomain) {
      double w = std::log (Util::nrCombinations (nrGroundings_, i));
      weightSum = Util::logSum (weightSum, w + follow_->weight());
    } else {
      // cout << endl;
      // cout << "nr groundings = " << nrGroundings_ << endl;
      // cout << "nr positives  = " << nrPositives() << endl;
      // cout << "nr negatives  = " << nrNegatives() << endl;      
      // cout << "i             = " << i << endl;
      // cout << "nr combos     = " ;
      // cout << Util::nrCombinations (nrGroundings_, i) << endl;
      double w = follow_->weight();
      // cout << "weight        = " << w << endl;
      weightSum += Util::nrCombinations (nrGroundings_, i) * w;
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
  double weight = c.literals()[0].isPositive()
      ? lwcnf_.posWeight (c.literals().front().lid())
      : lwcnf_.negWeight (c.literals().front().lid());
  LogVarSet lvs = c.constr().logVarSet();
  lvs -= c.ipgLogVars();
  lvs -= c.posCountedLogVars();
  lvs -= c.negCountedLogVars();
  unsigned nrGroundings = 1;
  if (lvs.empty() == false) {
    ConstraintTree ct = c.constr();
    ct.project (lvs);
    nrGroundings = ct.size();
  }
  // cout << "calc weight for " << clauses().front() << endl;
  if (c.posCountedLogVars().empty() == false) {
    // cout << "  -> nr pos = " << SetOrNode::nrPositives() << endl;
    nrGroundings *= std::pow (SetOrNode::nrPositives(),
        c.nrPosCountedLogVars());
  }
  if (c.negCountedLogVars().empty() == false) {
    //cout << "  -> nr neg = " << SetOrNode::nrNegatives() << endl;
    nrGroundings *= std::pow (SetOrNode::nrNegatives(),
        c.nrNegCountedLogVars());
  }
  // cout << "  -> nr groundings = " << nrGroundings << endl;
  // cout << "  -> lit weight    = " << weight << endl;
  // cout << "  -> ret weight    = " << std::pow (weight, nrGroundings) << endl;  
  return Globals::logDomain 
      ? weight * nrGroundings
      : std::pow (weight, nrGroundings);
}



double
SmoothNode::weight (void) const
{
  Clauses cs = clauses();
  double totalWeight = LogAware::multIdenty();
  for (size_t i = 0; i < cs.size(); i++) {
    double posWeight = lwcnf_.posWeight (cs[i].literals()[0].lid());
    double negWeight = lwcnf_.negWeight (cs[i].literals()[0].lid());
    LogVarSet lvs = cs[i].constr().logVarSet();
    lvs -= cs[i].ipgLogVars();
    lvs -= cs[i].posCountedLogVars();
    lvs -= cs[i].negCountedLogVars();
    unsigned nrGroundings = 1;
    if (lvs.empty() == false) {
      ConstraintTree ct = cs[i].constr();
      ct.project (lvs);
      nrGroundings = ct.size();
    }
    // cout << "calc smooth weight for " << cs[i] << endl;
    if (cs[i].posCountedLogVars().empty() == false) {
      // cout << "  -> nr pos = " << SetOrNode::nrPositives() << endl;
      nrGroundings *= std::pow (SetOrNode::nrPositives(), 
          cs[i].nrPosCountedLogVars());
    }
    if (cs[i].negCountedLogVars().empty() == false) {
      // cout << "  -> nr neg = " << SetOrNode::nrNegatives() << endl;
      nrGroundings *= std::pow (SetOrNode::nrNegatives(),
          cs[i].nrNegCountedLogVars());      
    }
    // cout << "  -> pos+neg = " << posWeight + negWeight << endl;
    // cout << "  -> nrgroun = " << nrGroundings << endl;    
    if (Globals::logDomain) {
      totalWeight += (Util::logSum (posWeight, negWeight)
          * std::log (nrGroundings));
    } else {
      totalWeight *= std::pow (posWeight + negWeight, nrGroundings);
    }
    // cout << "  -> smooth weight  = " << totalWeight << endl;
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
  smoothCircuit (root_);
  exportToGraphViz("circuit.smooth.dot");
  cout << "--------------------------------------------------" << endl;
  cout << "--------------------------------------------------" << endl;  
  cout << "WEIGHTED MODEL COUNT = " << getWeightedModelCount() << endl;
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
    *follow = new LeafNode (clauses[0], *lwcnf_);
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

  if (tryIndepPartialGrounding (follow, clauses)) {
    return;
  }

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
  Clauses depClauses = { clauses[0] };
  Clauses indepClauses (clauses.begin() + 1, clauses.end());
  bool finish = false;
  while (finish == false) {
    finish = true;
    for (size_t i = 0; i < indepClauses.size(); i++) {
      if (independentClause (indepClauses[i], depClauses) == false) {
        depClauses.push_back (indepClauses[i]);
        indepClauses.erase (indepClauses.begin() + i);
        finish = false;
        break;
      }
    }
  }
  if (indepClauses.empty() == false) {
    AndNode* andNode = new AndNode (clauses, " Independence");
    compile (andNode->leftBranch(), depClauses);
    compile (andNode->rightBranch(), indepClauses);
    (*follow) = andNode;
    return true;
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
    Literals depLits = { clauses[i].literals().front() };
    Literals indepLits (clauses[i].literals().begin() + 1,
        clauses[i].literals().end());
    bool finish = false;
    while (finish == false) {
      finish = true;
      for (size_t j = 0; j < indepLits.size(); j++) {
        if (independentLiteral (indepLits[j], depLits) == false) {
          depLits.push_back (indepLits[j]);
          indepLits.erase (indepLits.begin() + j);
          finish = false;
          break;
        }
      }
    }
    if (indepLits.empty() == false) {
      LogVarSet lvs1;
      for (size_t j = 0; j < depLits.size(); j++) {
        lvs1 |= depLits[j].logVarSet();
      }
      if (clauses[i].constr().isCountNormalized (lvs1) == false) {
        break;
      }
      LogVarSet lvs2;
      for (size_t j = 0; j < indepLits.size(); j++) {
        lvs2 |= indepLits[j].logVarSet();
      }
      if (clauses[i].constr().isCountNormalized (lvs2) == false) {
        break;
      }
      Clause c1 (clauses[i].constr().projectedCopy (lvs1));
      for (size_t j = 0; j < depLits.size(); j++) {
        c1.addLiteral (depLits[j]);
      }
      Clause c2 (clauses[i].constr().projectedCopy (lvs2));
      for (size_t j = 0; j < indepLits.size(); j++) {
        c2.addLiteral (indepLits[j]);
      }
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
      stringstream explanation;
      explanation << " IncExc on clause nº " << i + 1;
      IncExcNode* ieNode = new IncExcNode (clauses, explanation.str());
      compile (ieNode->plus1Branch(), plus1Clauses);
      compile (ieNode->plus2Branch(), plus2Clauses);
      compile (ieNode->minusBranch(), minusClauses);
      *follow = ieNode;
      return true;
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
  LogVars rootLogVars;
  LogVarSet lvs = clauses[0].ipgCandidates();
  for (size_t i = 0; i < lvs.size(); i++) {
    rootLogVars.clear();
    rootLogVars.push_back (lvs[i]);
    ConstraintTree ct = clauses[0].constr().projectedCopy ({lvs[i]});
    if (tryIndepPartialGroundingAux (clauses, ct, rootLogVars)) {
      Clauses newClauses = clauses;
      for (size_t j = 0; j < clauses.size(); j++) {
        newClauses[j].addIpgLogVar (rootLogVars[j]);
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
    LogVars& rootLogVars)
{
  for (size_t i = 1; i < clauses.size(); i++) {
    LogVarSet lvs = clauses[i].ipgCandidates();
    for (size_t j = 0; j < lvs.size(); j++) {
      ConstraintTree ct2 = clauses[i].constr().projectedCopy ({lvs[j]});
      if (ct.tupleSet() == ct2.tupleSet()) {
        rootLogVars.push_back (lvs[j]);
        break;
      }
    }
    if (rootLogVars.size() != i + 1) {
      return false;
    }
  }
  // verifies if the IPG logical vars appear in the same positions
  unordered_map<LiteralId, size_t> positions;
  for (size_t i = 0; i < clauses.size(); i++) {
    const Literals& literals = clauses[i].literals();
    for (size_t j = 0; j < literals.size(); j++) {
      size_t idx = literals[j].indexOfLogVar (rootLogVars[i]);
      assert (idx != literals[j].nrLogVars());
      unordered_map<LiteralId, size_t>::iterator it;
      it = positions.find (literals[j].lid());
      if (it != positions.end()) {
        if (it->second != idx) {
          return false;
        }
      } else {
        positions[literals[j].lid()] = idx;
      }
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
    if (clauses[i].nrPosCountedLogVars() > 0
        || clauses[i].nrNegCountedLogVars() > 0) {
      // only allow one atom counting node per branch
      return false;
    }
  }
  for (size_t i = 0; i < clauses.size(); i++) {
    Literals literals = clauses[i].literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].nrLogVars() == 1
          && ! clauses[i].isIpgLogVar (literals[j].logVars().front())
          && ! clauses[i].isCountedLogVar (literals[j].logVars().front())) {
        unsigned nrGroundings = clauses[i].constr().projectedCopy (
            literals[j].logVars()).size();
        SetOrNode* setOrNode = new SetOrNode (nrGroundings, clauses);
        Clause c1 (clauses[i].constr().projectedCopy (literals[j].logVars()));
        Clause c2 (clauses[i].constr().projectedCopy (literals[j].logVars()));
        c1.addLiteral (literals[j]);
        c2.addLiteralComplemented (literals[j]);
        c1.addPosCountedLogVar (literals[j].logVars().front());
        c2.addNegCountedLogVar (literals[j].logVars().front());
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
            clauses[idx2].addPosCountedLogVar (lvs2[k]);
            clauses.back().addNegCountedLogVar (lvs2[k]);
            return true;
          }
          if (clauses[idx2].isCountedLogVar (lvs2[k])
              && clauses[idx1].isCountedLogVar (lvs1[k]) == false) {
            clauses.push_back (clauses[idx1]);
            clauses[idx1].addPosCountedLogVar (lvs1[k]);
            clauses.back().addNegCountedLogVar (lvs1[k]);
            return true;
          }          
        }
      }      
    }
  }
  return false;
}



bool
LiftedCircuit::independentClause (
    Clause& clause,
    Clauses& otherClauses) const
{
  for (size_t i = 0; i < otherClauses.size(); i++) {
    if (Clause::independentClauses (clause, otherClauses[i]) == false) {
      return false;
    }
  }
  return true;
}



bool
LiftedCircuit::independentLiteral (
    const Literal& lit,
    const Literals& otherLits) const
{
  for (size_t i = 0; i < otherLits.size(); i++) {
    if (lit.lid() == otherLits[i].lid()
        || (lit.logVarSet() & otherLits[i].logVarSet()).empty() == false) {
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
            missingLids.insert (LitLvTypes (litSet[i].first, allTypes[j]));
          }
        }
      }
      createSmoothNode (missingLids, casted->follow());
      for (size_t i = 0; i < propagLits.size(); i++) {
        propagLits[i].setAllFullLogVars();
      }
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
      propagLits.insert (LitLvTypes (
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
      Clause c = lwcnf_->createClause (lid);
      for (size_t j = 0; j < types.size(); j++) {
        LogVar X = c.literals().front().logVars()[j];
        if (types[j] == LogVarType::POS_LV) {
          c.addPosCountedLogVar (X);
        } else if (types[j] == LogVarType::NEG_LV) {
          c.addNegCountedLogVar (X);
        }
      }
      c.addLiteralComplemented (c.literals()[0]);
      clauses.push_back (c);
    }
    SmoothNode* smoothNode = new SmoothNode (clauses, *lwcnf_);
    *prev = new AndNode ((*prev)->clauses(), smoothNode,
        *prev, " Smoothing");
  }
}



vector<LogVarTypes>
LiftedCircuit::getAllPossibleTypes (unsigned nrLogVars) const
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
LiftedCircuit::containsTypes (
    const LogVarTypes& typesA,
    const LogVarTypes& typesB) const
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
  string opStyle = "shape=circle,width=0.7,margin=\"0.0,0.0\"," ;

  switch (getCircuitNodeType (node)) {
  
    case OR_NODE: {
      OrNode* casted = dynamic_cast<OrNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [" << opStyle << "label=\"∨\"]" << endl;
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

      os << auxNode << " [" << opStyle << "label=\"∧\"]" << endl;
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

      os << auxNode << " [" << opStyle << "label=\"∨(X)\"]" << endl;
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

      os << auxNode << " [" << opStyle << "label=\"∧(X)\"]" << endl;
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

      os << auxNode << " [" << opStyle << "label=\"+ - +\"]" ;
      os << endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << node->explanation() << "\"]" ;
      os << endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->plus1Branch());
      os << " [label=\" " << (*casted->plus1Branch())->weight() << "\"]" ;
      os << endl;
      
      os << auxNode << " -> " ;
      os << escapeNode (*casted->minusBranch()) << endl;
      os << " [label=\" " << (*casted->minusBranch())->weight() << "\"]" ;
      os << endl;      

      os << auxNode << " -> " ;
      os << escapeNode (*casted->plus2Branch());
      os << " [label=\" " << (*casted->plus2Branch())->weight() << "\"]" ;
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

