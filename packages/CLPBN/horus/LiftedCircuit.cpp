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
LeafNode::weight (void) const
{
  assert (clauses().size() == 1);
  assert (clauses()[0].isUnit());
  Clause c = clauses()[0];
  double weight = c.literals()[0].weight();
  unsigned nrGroundings = c.constr()->size();
  assert (nrGroundings != 0);
  double www = Globals::logDomain 
      ? weight * nrGroundings
      : std::pow (weight, nrGroundings);
      
  cout << "leaf w: " << www << endl;
  
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
    unsigned nrGroundings = cs[i].constr()->size();
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
  compile (&root_, ccc);
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
    const Clauses& clauses)
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
  
  *follow = new FailNode (clauses);

}



bool
LiftedCircuit::tryUnitPropagation (
    CircuitNode** follow,
    const Clauses& clauses)
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
          }
          if (clauses[i].literals()[0].isNegative()) {
            if (clauses[j].containsNegativeLiteral (lid) == false) {
              Clause newClause = clauses[j];
              newClause.removePositiveLiterals (lid);
              newClauses.push_back (newClause);
            }
          }
        }
      }
      stringstream explanation;
      explanation << " UP of " << clauses[i];
      AndNode* andNode = new AndNode (clauses, explanation.str());
      compile (andNode->leftBranch(), {clauses[i]});
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
    const Clauses& clauses)
{
  if (clauses.size() == 1) {
    return false;
  }
  for (size_t i = 0; i < clauses.size(); i++) {
    bool indep = true;
    TinySet<LiteralId> lids1 = clauses[i].lidSet();
    for (size_t j = 0; j < clauses.size(); j++) {
      TinySet<LiteralId> lids2 = clauses[j].lidSet(); 
      if (((lids1 & lids2).empty() == false) && i != j) {
        indep = false;
        break;
      }
    }
    if (indep == true) {
      Clauses newClauses = clauses;
      newClauses.erase (newClauses.begin() + i);
      stringstream explanation;
      explanation << " independence" ;
      AndNode* andNode = new AndNode (clauses, explanation.str());
      compile (andNode->leftBranch(), {clauses[i]});
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
    const Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size(); i++) {
    const Literals& literals = clauses[i].literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].isGround (clauses[i].constr())) {
        Literal posLit (literals[j], false);
        Literal negLit (literals[j], true);
        ConstraintTree* ct1 = new ConstraintTree (*clauses[i].constr());
        ConstraintTree* ct2 = new ConstraintTree (*clauses[i].constr());
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
        (*follow) = orNode;
        compile (orNode->leftBranch(),  leftClauses);
        compile (orNode->rightBranch(), rightClauses);
        return true;   
      }
    }
  }
  return false;
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
        string explanation = " smoothing" ;
        AndNode* andNode = new AndNode ((*prev)->clauses(), smoothNode, *prev, explanation);
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
        string explanation = " smoothing" ;
        AndNode* andNode = new AndNode ((*prev)->clauses(), smoothNode, *prev, explanation);
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
      // TODO
    }
    
    case CircuitNodeType::SET_AND_NODE: {
      // TODO
    }
    
    case CircuitNodeType::INC_EXC_NODE: {
      // TODO
    }
    
    case CircuitNodeType::LEAF_NODE: {
      propagatingLids.insert (node->clauses()[0].literals()[0].lid());
    }
    
    // case CircuitNodeType::SMOOTH_NODE:
    // case CircuitNodeType::TRUE_NODE:
    // case CircuitNodeType::FAIL_NODE:
    
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
  } else if (dynamic_cast<const LeafNode*>(node) != 0) {
    type = CircuitNodeType::LEAF_NODE;
  } else if (dynamic_cast<const SmoothNode*>(node) != 0) {
    type = CircuitNodeType::SMOOTH_NODE;
  } else if (dynamic_cast<const TrueNode*>(node) != 0) {
    type = CircuitNodeType::TRUE_NODE;
  } else if (dynamic_cast<const FailNode*>(node) != 0) {
    type = CircuitNodeType::FAIL_NODE;
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
  
  static unsigned nrAndNodes = 0;
  static unsigned nrOrNodes  = 0;

  switch (getCircuitNodeType (node)) {
  
    case OR_NODE: {
      OrNode* casted = dynamic_cast<OrNode*>(node);
      const Clauses& clauses = node->clauses();
      if (clauses.empty() == false) {
      os << escapeNode (node) << " [shape=box,label=\"" ;
      for (size_t i = 0; i < clauses.size(); i++) {
        if (i != 0) os << "\\n" ;
        os << clauses[i];
      }
      os << "\"]" ;
      os << endl;
      }
      os << "or" << nrOrNodes << " [label=\"∨\"]" << endl;
      os << '"' << node << '"' << " -> " << "or" << nrOrNodes;
      os << " [label=\"" << node->explanation() << "\"]" << endl;
      os << "or" << nrOrNodes << " -> " ;
      os << escapeNode (*casted->leftBranch()) << endl;
      os << "or" << nrOrNodes << " -> " ;
      os << escapeNode (*casted->rightBranch()) << endl;
      nrOrNodes ++;
      exportToGraphViz (*casted->leftBranch(),  os);
      exportToGraphViz (*casted->rightBranch(), os);
      break;
    }
    
    case AND_NODE: {
      AndNode* casted = dynamic_cast<AndNode*>(node);
      const Clauses& clauses = node->clauses();
      os << escapeNode (node) << " [shape=box,label=\"" ;
      for (size_t i = 0; i < clauses.size(); i++) {
        if (i != 0) os << "\\n" ;
        os << clauses[i];
      }
      os << "\"]" ;
      os << endl;
      os << "and" << nrAndNodes << " [label=\"∧\"]" << endl;
      os << '"' << node << '"' << " -> " << "and" << nrAndNodes;
      os << " [label=\"" << node->explanation() << "\"]" << endl;
      os << "and" << nrAndNodes << " -> " ;
      os << escapeNode (*casted->leftBranch()) << endl;
      os << "and" << nrAndNodes << " -> " ;
      os << escapeNode (*casted->rightBranch()) << endl;
      nrAndNodes ++;
      exportToGraphViz (*casted->leftBranch(),  os);
      exportToGraphViz (*casted->rightBranch(), os);
      break;
    }
    
    case LEAF_NODE: {
      os << escapeNode (node);
      os << " [shape=box,label=\"" ;
      os << node->clauses()[0];
      os << "\"]" ;
      os << endl;
      break;
    }
    
    case SMOOTH_NODE: {
      os << escapeNode (node);
      os << " [shape=box,style=filled,fillcolor=chartreuse,label=\"" ;
      const Clauses& clauses = node->clauses();
      for (size_t i = 0; i < clauses.size(); i++) {
        if (i != 0) os << "\\n" ;
        os << clauses[i];
      }
      os << "\"]" ;
      os << endl;
      break;
    }
    
    case TRUE_NODE: {
      os << escapeNode (node);
      os << " [shape=box,label=\"⊤\"]" ;
      os << endl;
      break;
    }
    
    case FAIL_NODE: {
      os << escapeNode (node);
      os << " [shape=box,style=filled,fillcolor=brown1,label=\"" ;
      const Clauses& clauses = node->clauses();
      for (size_t i = 0; i < clauses.size(); i++) {
        if (i != 0) os << "\\n" ;
        os << clauses[i];
      }
      os << "\"]" ;
      os << endl;
      break;
    }
    
    default:
      assert (false);
  }
}

