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



double
SetOrNode::weight (void) const
{
  // TODO
  return 0.0;
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
  LogVarSet lvs = c.constr().logVarSet() - c.ipgLogVars();
  unsigned nrGroundings = 1;
  if (lvs.empty() == false) {
    ConstraintTree ct = c.constr();
    ct.project (lvs);
    nrGroundings = ct.size();
  }
  assert (nrGroundings != 0);
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
  abort();
  return 0.0;
}



LiftedCircuit::LiftedCircuit (const LiftedWCNF* lwcnf)
    : lwcnf_(lwcnf)
{
  root_ = 0;
  Clauses ccc = lwcnf->clauses();
  //ccc.erase (ccc.begin() + 5, ccc.end());
  //Clause c2 = ccc.front();
  //c2.removeLiteralByIndex (1);
  //ccc.push_back (c2);
  
  //compile (&root_, lwcnf->clauses());
  Clauses cccc = {ccc[6],ccc[4]};
  cccc.front().removeLiteral (2);
  compile (&root_, cccc);
  exportToGraphViz("circuit.dot");
  smoothCircuit();
  exportToGraphViz("smooth.dot");
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
    static int count = 0; count ++;
    *follow = new LeafNode (clauses[0]);
    if (count == 1) {
    //  Clause c (new ConstraintTree({}));
    //  c.addLiteral (Literal (100,{}));
    //  *follow = new LeafNode (c);
    }
     if (count == 2) {
    //  Clause c (new ConstraintTree({}));
    //  c.addLiteral (Literal (101,{}));
    //  *follow = new LeafNode (c);
    }
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
  for (size_t i = 0; i < clauses.size(); i++) {
    if (clauses[i].isUnit()) {
      Clauses newClauses;
      for (size_t j = 0; j < clauses.size(); j++) {
        if (i != j) {
          LiteralId lid = clauses[i].literals()[0].lid();
          if (clauses[i].literals()[0].isPositive()) {
            if (clauses[j].containsPositiveLiteral (lid) == false) {
              Clause newClause = clauses[j];
              newClause.removeNegativeLiterals (lid);
              newClauses.push_back (newClause);
            }
          } else if (clauses[i].literals()[0].isNegative()) {
            if (clauses[j].containsNegativeLiteral (lid) == false) {
              Clause newClause = clauses[j];
              newClause.removePositiveLiterals (lid);
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



TinySet<LiteralId>
LiftedCircuit::smoothCircuit (CircuitNode* node)
{
  assert (node != 0);
  TinySet<LiteralId> propagatingLids;

  switch (getCircuitNodeType (node)) {

    case CircuitNodeType::OR_NODE: {
      OrNode* casted = dynamic_cast<OrNode*>(node);
      TinySet<LiteralId> lids1 = smoothCircuit (*casted->leftBranch());
      TinySet<LiteralId> lids2 = smoothCircuit (*casted->rightBranch());
      TinySet<LiteralId> missingLeft  = lids2 - lids1;
      TinySet<LiteralId> missingRight = lids1 - lids2;
      if (missingLeft.empty() == false) {
        Clauses clauses;
        for (size_t i = 0; i < missingLeft.size(); i++) {
          Clause c = lwcnf_->createClauseForLiteral (missingLeft[i]);
          c.addAndNegateLiteral (c.literals()[0]);
          clauses.push_back (c);
        }
        SmoothNode* smoothNode = new SmoothNode (clauses);
        CircuitNode** prev = casted->leftBranch();
        AndNode* andNode = new AndNode ((*prev)->clauses(),
            smoothNode, *prev, " Smoothing");
        *prev = andNode;
      }
      if (missingRight.empty() == false) {
        Clauses clauses;
        for (size_t i = 0; i < missingRight.size(); i++) {
          Clause c = lwcnf_->createClauseForLiteral (missingRight[i]);
          c.addAndNegateLiteral (c.literals()[0]);
          clauses.push_back (c);
        }
        SmoothNode* smoothNode = new SmoothNode (clauses);
        CircuitNode** prev = casted->rightBranch();
        AndNode* andNode = new AndNode ((*prev)->clauses(), smoothNode,
            *prev, " Smoothing");
        *prev = andNode;
      }
      propagatingLids |= lids1;
      propagatingLids |= lids2;
      break;
    }
  
    case CircuitNodeType::AND_NODE: {
      AndNode* casted = dynamic_cast<AndNode*>(node);
      TinySet<LiteralId> lids1 = smoothCircuit (*casted->leftBranch());
      TinySet<LiteralId> lids2 = smoothCircuit (*casted->rightBranch());
      propagatingLids |= lids1;
      propagatingLids |= lids2;
      break;
    }

    case CircuitNodeType::SET_OR_NODE: {
      break;
    }
    
    case CircuitNodeType::SET_AND_NODE: {
      SetAndNode* casted = dynamic_cast<SetAndNode*>(node);
      propagatingLids = smoothCircuit (*casted->follow());
      break;
    }
    
    case CircuitNodeType::INC_EXC_NODE: {
      // TODO
      break;
    }
    
    case CircuitNodeType::LEAF_NODE: {
      propagatingLids.insert (node->clauses()[0].literals()[0].lid());
    }
    
    // case CircuitNodeType::SMOOTH_NODE:
    // case CircuitNodeType::TRUE_NODE:
    // case CircuitNodeType::COMPILATION_FAILED_NODE:
    
    default:
      break;
  }
  
  return propagatingLids;
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



string
LiftedCircuit::escapeNode (const CircuitNode* node) const
{
  stringstream ss;
  ss << "\"" << node << "\"" ;
  return ss.str();
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
      // TODO
      assert (false);
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
      printClauses (node, os);
      break;
    }
    
    case SMOOTH_NODE: {
      printClauses (node, os, "style=filled,fillcolor=chartreuse,");
      break;
    }
    
    case TRUE_NODE: {
      os << escapeNode (node);
      os << " [shape=box,label=\"⊤\"]" ;
      os << endl;
      break;
    }
    
    case COMPILATION_FAILED_NODE: {
      printClauses (node, os, "style=filled,fillcolor=brown1,");
      break;
    }
    
    default:
      assert (false);
  }
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

