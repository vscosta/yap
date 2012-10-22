#include <fstream>

#include "LiftedCircuit.h"


LiftedCircuit::LiftedCircuit (const LiftedWCNF* lwcnf)
    : lwcnf_(lwcnf)
{
  root_ = 0;
  Clauses ccc = lwcnf->clauses();
  //ccc.erase (ccc.begin() + 3, ccc.end());
  //Clause c2 = ccc.front();
  //c2.removeLiteralByIndex (1);
  //ccc.push_back (c2);
  
  
  //compile (&root_, lwcnf->clauses());
  compile (&root_, ccc);
  cout << "done compiling..." << endl;
  printToDot();
}



void
LiftedCircuit::printToDot (void)
{
  const char* fileName = "circuit.dot" ;
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "BayesBallGraph::exportToDotFile()" << endl;
    abort();
  }
  out << "digraph {" << endl;
  out << "ranksep=1" << endl; 
  printToDot (root_, out);
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
    *follow = new LeafNode (clauses[0]);
    return;
  }

  if (tryUnitPropagation (follow, clauses)) {
    return;
  }
  
  if (tryIndependence (follow, clauses)) {
    return;
  }
  
  if (tryShannonDecomposition (follow, clauses)) {
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
      compile (andNode->leftFollow(), {clauses[i]});
      compile (andNode->rightFollow(), newClauses);
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
      compile (andNode->leftFollow(), {clauses[i]});
      compile (andNode->rightFollow(), newClauses);
      (*follow) = andNode;
      return true;
    }
  }
  return false;
}



bool
LiftedCircuit::tryShannonDecomposition (
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
        compile (orNode->leftFollow(),  leftClauses);
        compile (orNode->rightFollow(), rightClauses);
        return true;   
      }
    }
  }
  return false;
}



string
LiftedCircuit::escapeNode (const CircuitNode* node) const
{
  stringstream ss;
  ss << "\"" << node << "\"" ;
  return ss.str();
}



void
LiftedCircuit::printToDot (CircuitNode* node, ofstream& os)
{
  assert (node != 0);
  static unsigned nrAndNodes = 0;
  static unsigned nrOrNodes  = 0;

  if (dynamic_cast<AndNode*>(node) != 0) {
    nrAndNodes ++;
    AndNode* casted = dynamic_cast<AndNode*>(node);
    os << escapeNode (node) << " [shape=box,label=\"" ;
    const Clauses& clauses = node->clauses();
    for (size_t i = 0; i < clauses.size(); i++) {
      os << clauses[i] << "\\n" ;
    }
    os << "\"]" ;
    os << endl;
    os << "and" << nrAndNodes << " [label=\"∧\"]" << endl;
    os << '"' << node << '"' << " -> " << "and" << nrAndNodes;
    os << " [label=\"" << node->explanation() << "\"]" << endl;
    os << "and" << nrAndNodes << " -> " ;
    os << escapeNode (*casted->leftFollow()) << endl;
    os << "and" << nrAndNodes << " -> " ;
    os << escapeNode (*casted->rightFollow()) << endl;
    printToDot (*casted->leftFollow(),  os);
    printToDot (*casted->rightFollow(), os);
  } else if (dynamic_cast<OrNode*>(node) != 0) {
    nrOrNodes ++;
    OrNode* casted = dynamic_cast<OrNode*>(node);
    os << escapeNode (node) << " [shape=box,label=\"" ;
    const Clauses& clauses = node->clauses();
    for (size_t i = 0; i < clauses.size(); i++) {
      os << clauses[i] << "\\n" ;
    }
    os << "\"]" ;
    os << endl;
    os << "or" << nrOrNodes << " [label=\"∨\"]" << endl;
    os << '"' << node << '"' << " -> " << "or" << nrOrNodes;
    os << " [label=\"" << node->explanation() << "\"]" << endl;
    os << "or" << nrOrNodes << " -> " ;
    os << escapeNode (*casted->leftFollow()) << endl;
    os << "or" << nrOrNodes << " -> " ;
    os << escapeNode (*casted->rightFollow()) << endl;
    printToDot (*casted->leftFollow(),  os);
    printToDot (*casted->rightFollow(), os);
  } else if (dynamic_cast<LeafNode*>(node) != 0) {
    os << escapeNode (node) << " [shape=box,label=\"" ;
    os << node->clauses()[0] << "\\n" ;
    os << "\"]" ;
    os << endl;
  } else if (dynamic_cast<TrueNode*>(node) != 0) {
    os << escapeNode (node);
    os << " [shape=box,label=\"⊤\"]" ;
    os << endl;
  } else if (dynamic_cast<FailNode*>(node) != 0) {
    os << escapeNode (node);
    os << " [shape=box,style=filled,fillcolor=red,label=\"" ;
    const Clauses& clauses = node->clauses();
    for (size_t i = 0; i < clauses.size(); i++) {
      os << clauses[i] << "\\n" ;
    }
    os << "\"]" ;
    os << endl;
  } else {
    cout << "something really failled" << endl;
  }  
}

