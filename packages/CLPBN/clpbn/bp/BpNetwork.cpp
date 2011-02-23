#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <sstream>

#include "BpNetwork.h"
#include "BpNode.h"
#include "CptEntry.h"

BpNetwork::BpNetwork (void)
{
  schedule_          = SEQUENTIAL_SCHEDULE;
  maxIter_           = 150;
  stableThreashold_  = 0.00000000000000000001;
}



BpNetwork::~BpNetwork (void)
{
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];
  }
  nodes_.clear();
}



void
BpNetwork::setSolverParameters (Schedule schedule, 
                                int maxIter, 
                                double stableThreashold)
{
  if (maxIter <= 0) {
    cerr << "error: maxIter must be greater or equal to 1" << endl;
    abort();
  }
  if (stableThreashold <= 0.0 || stableThreashold >= 1.0) {
    cerr << "error: stableThreashold must be greater than 0.0 " ;
    cerr << "and lesser than 1.0" << endl;
    abort();
  }
  schedule_         = schedule;
  maxIter_          = maxIter;
  stableThreashold_ = stableThreashold;
}



void
BpNetwork::runSolver (BayesianNode* queryVar)
{
  vector<BayesianNode*> queryVars;
  queryVars.push_back (queryVar);
  runSolver (queryVars);
}



void
BpNetwork::runSolver (vector<BayesianNode*> queryVars)
{
  if (queryVars.size() > 1) {
    addJunctionNode (queryVars);
  }
  else {
    string varName = queryVars[0]->getVariableName();
    queryNode_ = static_cast<BpNode*> (getNode (varName));
  }

  if (!isPolyTree()) {
    if (DL_ >= 1) {
      cout << "The graph is not single connected. " ;
      cout << "Iterative belief propagation will be used." ;
      cout << endl << endl;
    }
    schedule_  = PARALLEL_SCHEDULE;
  }

  if (schedule_ == SEQUENTIAL_SCHEDULE) {
    initializeSolver (queryVars);
    runNeapolitanSolver();
    for (unsigned int i = 0; i < nodes_.size(); i++) {
      if (nodes_[i]->hasEvidence()) {
        BpNode* v = static_cast<BpNode*> (nodes_[i]);
        addEvidence (v); 
        vector<BpNode*> parents = cast (v->getParents());
        for (unsigned int i = 0; i < parents.size(); i++) {
          if (!parents[i]->hasEvidence()) {
            sendLambdaMessage (v, parents[i]);
          }
        }
        vector<BpNode*> childs = cast (v->getChilds());
        for (unsigned int i = 0; i < childs.size(); i++) {
          sendPiMessage (v, childs[i]);
        }
      }
    }
  } else if (schedule_ == PARALLEL_SCHEDULE) {
    BpNode::enableParallelSchedule();
    initializeSolver (queryVars);
    for (unsigned int i = 0; i < nodes_.size(); i++) {
      if (nodes_[i]->hasEvidence()) {
        addEvidence (static_cast<BpNode*> (nodes_[i]));
      }
    }
    runIterativeBpSolver();
  }
}



void 
BpNetwork::printCurrentStatus (void)
{ 
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    printCurrentStatusOf (static_cast<BpNode*> (nodes_[i]));
  }
}



void 
BpNetwork::printCurrentStatusOf (BpNode* x)
{
  vector<BpNode*> childs = cast (x->getChilds());
  vector<string> domain  = x->getDomain();

  cout << left;
  cout << setw (10) << "domain" ;
  cout << setw (20) << "π(" + x->getVariableName() + ")" ;
  cout << setw (20) << "λ(" + x->getVariableName() + ")" ;
  cout << setw (16) << "belief" ;
  cout << endl;

  cout << "--------------------------------" ;
  cout << "--------------------------------" ;
  cout << endl;

  double* piValues     = x->getPiValues();
  double* lambdaValues = x->getLambdaValues();
  double* beliefs      = x->getBeliefs();
  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    cout << setw (10) << domain[xi];
    cout << setw (19) << piValues[xi];
    cout << setw (19) << lambdaValues[xi];
    cout.precision (PRECISION_);
    cout << setw (16) << beliefs[xi];
    cout << endl;
  }
  cout << endl;
  if (childs.size() > 0) {
    string s = "(" + x->getVariableName() + ")" ;
    for (unsigned int j = 0; j < childs.size(); j++) {
      cout << setw (10) << "domain" ;
      cout << setw (28) << "π" + childs[j]->getVariableName() + s;
      cout << setw (28) << "λ" + childs[j]->getVariableName() + s;
      cout << endl;
      cout << "--------------------------------" ;
      cout << "--------------------------------" ;
      cout << endl;
      for (int xi = 0; xi < x->getDomainSize(); xi++) {
        cout << setw (10) << domain[xi];
        cout.precision (PRECISION_);
        cout << setw (27) << x->getPiMessage(childs[j], xi);
        cout.precision (PRECISION_);
        cout << setw (27) << x->getLambdaMessage(childs[j], xi);
        cout << endl;
      }
      cout << endl;
    }
  }
}



void
BpNetwork::printBeliefs (void)
{
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    BpNode* x = static_cast<BpNode*> (nodes_[i]);
    vector<string> domain = x->getDomain();
    cout << setw (20) << left << x->getVariableName() ;
    cout << setw (26) << "belief" ;
    cout << endl;
    cout << "--------------------------------------" ;
    cout << endl;
    double* beliefs = x->getBeliefs();
    for (int xi = 0; xi < x->getDomainSize(); xi++) {
      cout << setw (20) << domain[xi];
      cout.precision (PRECISION_);
      cout << setw (26) << beliefs[xi];
      cout << endl;
    }
    cout << endl;
  }
}



vector<double>
BpNetwork::getBeliefs (void)
{
  return getBeliefs (queryNode_);
}



vector<double>
BpNetwork::getBeliefs (BpNode* x)
{
  double* beliefs = x->getBeliefs();
  vector<double> beliefsVec;
  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    beliefsVec.push_back (beliefs[xi]);
  }
  return beliefsVec;
}



void 
BpNetwork::initializeSolver (vector<BayesianNode*> queryVars)
{
  if (DL_ >= 1) {
    cout << "Initializing solver" << endl;
    if (schedule_ == SEQUENTIAL_SCHEDULE) {
      cout << "-> schedule           = sequential" << endl;
    } else {
      cout << "-> schedule           = parallel" << endl;
    }
    cout << "-> max iters          = " << maxIter_ << endl;
    cout << "-> stable threashold  = " << stableThreashold_ << endl;
    cout << "-> query vars         = " ;
    for (unsigned int i = 0; i < queryVars.size(); i++) {
      cout << queryVars[i]->getVariableName() << " " ;
    }
    cout << endl;
  }

  nIter_ = 0;

  for (unsigned int i = 0; i < nodes_.size(); i++) {
    BpNode* node = static_cast<BpNode*> (nodes_[i]);
    node->allocateMemory();
  }

  for (unsigned int i = 0; i < nodes_.size(); i++) {
    BpNode* x = static_cast<BpNode*> (nodes_[i]);

    double* piValues     = x->getPiValues();
    double* lambdaValues = x->getLambdaValues();
    for (int xi = 0; xi < x->getDomainSize(); xi++) {
      piValues[xi]     = 1.0;
      lambdaValues[xi] = 1.0;
    }

    vector<BpNode*> xChilds = cast (x->getChilds());
    for (unsigned int j = 0; j < xChilds.size(); j++) {
      double* piMessages = x->getPiMessages (xChilds[j]);
      for (int xi = 0; xi < x->getDomainSize(); xi++) {
        piMessages[xi] = 1.0;
        //x->setPiMessage (xChilds[j], xi, 1.0);
      }
    }

    vector<BpNode*> xParents = cast (x->getParents());
    for (unsigned int j = 0; j < xParents.size(); j++) {
      double* lambdaMessages = xParents[j]->getLambdaMessages (x);
      for (int xi = 0; xi < xParents[j]->getDomainSize(); xi++) {
        lambdaMessages[xi] = 1.0;
        //xParents[j]->setLambdaMessage (x, xi, 1.0);
      }
    }
  }

  for (unsigned int i = 0; i < nodes_.size(); i++) {
      BpNode* x = static_cast<BpNode*> (nodes_[i]);
      x->normalizeMessages();
  }
  printCurrentStatus();


  vector<BpNode*> roots = cast (getRootNodes());
  for (unsigned int i = 0; i < roots.size(); i++) {
    double* params = roots[i]->getParameters();
    double* piValues = roots[i]->getPiValues();
    for (int ri = 0; ri < roots[i]->getDomainSize(); ri++) {
      piValues[ri] = params[ri];
    }
  }
}



void
BpNetwork::addJunctionNode (vector<BayesianNode*> queryVars)
{
  const string VAR_NAME = "_Jn";
  int nStates = 1;
  vector<BayesianNode*> parents;
  vector<string> domain;
  for (unsigned int i = 0; i < queryVars.size(); i++) {
    parents.push_back (queryVars[i]);
    nStates *= queryVars[i]->getDomainSize();
  }

  for (int i = 0; i < nStates; i++) {
    stringstream ss;
    ss << "_jn" << i;
    domain.push_back (ss.str()); // FIXME make domain optional
  }

  int nParams = nStates * nStates;
  double* params = new double [nParams];
  for (int i = 0; i < nParams; i++) {
    int row = i / nStates;
    int col = i % nStates;
    if (row == col) {
      params[i] = 1;
    } else {
      params[i] = 0;
    }
  }
  addNode (VAR_NAME, parents, params, nParams, domain);
  queryNode_ = static_cast<BpNode*> (getNode (VAR_NAME));
  printNetwork();
}



void 
BpNetwork::addEvidence (BpNode* v)
{
  if (DL_ >= 1) {
    cout << "Adding evidence: node " ;
    cout << "`" << v->getVariableName() << "' was instantiated as " ;
    cout << "`" << v->getDomain()[v->getEvidence()] << "'" ;
    cout << endl;
  }
  double* piValues     = v->getPiValues();
  double* lambdaValues = v->getLambdaValues();
  for (int vi = 0; vi < v->getDomainSize(); vi++) {
    if (vi == v->getEvidence()) {
      piValues[vi]     = 1.0;
      lambdaValues[vi] = 1.0;
    } else {
      piValues[vi]     = 0.0;
      lambdaValues[vi] = 0.0;
    } 
  }
}



void
BpNetwork::runNeapolitanSolver (void)
{
  vector<BpNode*> roots = cast (getRootNodes());
  for (unsigned int i = 0; i < roots.size(); i++) {
    vector<BpNode*> childs = cast (roots[i]->getChilds());
    for (unsigned int j = 0; j < childs.size(); j++) {
      sendPiMessage (roots[i], static_cast<BpNode*> (childs[j]));
    }
  }
}



void 
BpNetwork::sendPiMessage (BpNode* z, BpNode* x)
{
  nIter_ ++;
  if (!(maxIter_ == -1 || nIter_ < maxIter_)) {
    cout << "the maximum number of iterations was achieved, terminating..." ;
    cout << endl;
    return;
  }

  if (DL_ >= 1) {
    cout << "π message " << z->getVariableName();
    cout << " --> " << x->getVariableName() << endl;
  }

  updatePiMessages(z, x);

  if (!x->hasEvidence()) {
    updatePiValues (x);
    vector<BpNode*> xChilds = cast (x->getChilds());
    for (unsigned int i = 0; i < xChilds.size(); i++) {
      sendPiMessage (x, xChilds[i]);
    }
  }

  bool isAllOnes = true;
  double* lambdaValues = x->getLambdaValues();
  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    if (lambdaValues[xi] != 1.0) {
      isAllOnes = false;
      break;
    }
  }

  if (!isAllOnes) {
    vector<BpNode*> xParents = cast (x->getParents());
    for (unsigned int i = 0; i < xParents.size(); i++) {
      if (xParents[i] != z && !xParents[i]->hasEvidence()) {
        sendLambdaMessage (x, xParents[i]);
      }
    }
  }
}



void 
BpNetwork::sendLambdaMessage (BpNode* y, BpNode* x)
{
  nIter_ ++; 
  if (!(maxIter_ == -1 || nIter_ < maxIter_)) {
    cout << "the maximum number of iterations was achieved, terminating..." ;
    cout << endl;
    return;
  }

  if (DL_ >= 1) {
    cout << "λ message " << y->getVariableName();
    cout << " --> " << x->getVariableName() << endl;
  }

  updateLambdaMessages (x, y);
  updateLambdaValues (x);

  vector<BpNode*> xParents = cast (x->getParents());
  for (unsigned int i = 0; i < xParents.size(); i++) {
    if (!xParents[i]->hasEvidence()) {
      sendLambdaMessage (x, xParents[i]);
    }
  }

  vector<BpNode*> xChilds = cast (x->getChilds());
  for (unsigned int i = 0; i < xChilds.size(); i++) {
    if (xChilds[i] != y) {
      sendPiMessage (x, xChilds[i]);
    }
  }
}



void
BpNetwork::updatePiValues (BpNode* x)
{
  // π(Xi)
  vector<BpNode*> parents = cast (x->getParents());
  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    stringstream calcs1;
    stringstream calcs2;
    if (DL_ >= 2) {
      calcs1 << "π("<< x->getDomain()[xi] << ")" << endl << "= " ;
    }
    double sum = 0.0;
    vector<pair<int, int> > constraints;
    vector<CptEntry> entries = x->getCptEntriesOfRow (xi);
    for (unsigned int k = 0; k < entries.size(); k++) {
      double prod = x->getProbability (entries[k]);
      if (DL_ >= 2) {
        if (k != 0) calcs1 << endl << "+ " ;
        calcs1 << x->entryToString (entries[k]);
        if (DL_ >= 3) {
          (k == 0) ? calcs2 << "(" << prod : calcs2 << endl << "+ (" << prod;
        }
      }
      vector<int> insts = entries[k].getDomainInstantiations();
      for (unsigned int i = 0; i < parents.size(); i++) {
        double value = parents[i]->getPiMessage (x, insts[i + 1]);
        prod *= value;
        if (DL_ >= 2) {
          calcs1 << ".π" << x->getVariableName();
          calcs1 << "(" << parents[i]->getDomain()[insts[i + 1]] << ")";
          if (DL_ >= 3) calcs2 << "x" << value;
        }
      }
      sum += prod;
      if (DL_ >= 3) calcs2 << ")";
    }
    x->setPiValue (xi, sum);
    if (DL_ >= 2) {
      cout << calcs1.str();
      if (DL_ >= 3) cout << endl << "= " << calcs2.str();
      cout << " = " << sum << endl;
    }
  }
}



void
BpNetwork::updatePiMessages (BpNode* z, BpNode* x)
{
  // πX(Zi)
  vector<BpNode*> zChilds  = cast (z->getChilds());
  for (int zi = 0; zi < z->getDomainSize(); zi++) {
    stringstream calcs1;
    stringstream calcs2;
    if (DL_ >= 2) {
      calcs1 << "π" << x->getVariableName();
      calcs1 << "(" << z->getDomain()[zi] << ") = " ;
    }
    double prod = z->getPiValue (zi);
    if (DL_ >= 2) {
      calcs1 << "π(" << z->getDomain()[zi] << ")" ;
      if (DL_ >= 3) calcs2 << prod;
    }
    for (unsigned int i = 0; i < zChilds.size(); i++) {
      if (zChilds[i] != x) {
        double value = z->getLambdaMessage (zChilds[i], zi);
        prod *= value;
        if (DL_ >= 2) {
          calcs1 << ".λ" << zChilds[i]->getVariableName();
          calcs1 << "(" << z->getDomain()[zi] + ")" ;
          if (DL_ >= 3) calcs2 << " x " << value;
        }
      }
    }
    z->setPiMessage (x, zi, prod);
    if (DL_ >= 2) {
      cout << calcs1.str();
      if (DL_ >= 3) cout << " = " << calcs2.str();
      cout << " = " << prod << endl;
    }
  }
}



void
BpNetwork::updateLambdaValues (BpNode* x)
{
  // λ(Xi)
  vector<BpNode*> childs = cast (x->getChilds());
  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    stringstream calcs1;
    stringstream calcs2;
    if (DL_ >= 2) {
      calcs1 << "λ" << "(" << x->getDomain()[xi] << ") = " ;
    }
    double prod = 1.0;
    for (unsigned int i = 0; i < childs.size(); i++) {
      double val = x->getLambdaMessage (childs[i], xi);
      prod *= val;
      if (DL_ >= 2) { 
        if (i != 0) calcs1 << "." ;
        calcs1 << "λ" << childs[i]->getVariableName();
        calcs1 << "(" << x->getDomain()[xi] + ")" ;
        if (DL_ >= 3) (i == 0) ? calcs2 << val : calcs2 << " x " << val;
      }
    }
    x->setLambdaValue (xi, prod);
    if (DL_ >= 2) {
      cout << calcs1.str();
      if (childs.size() == 0) {
        cout << 1 << endl;
      } else {
        if (DL_ >= 3) cout << " = " << calcs2.str();
        cout << " = " << prod << endl;
      }
    }
  }
}



void
BpNetwork::updateLambdaMessages (BpNode* x, BpNode* y)
{
  // λY(Xi)
  int parentIndex = y->getIndexOfParent (x) + 1;
  vector<BpNode*> yParents = cast (y->getParents());
  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    stringstream calcs1;
    stringstream calcs2;
    if (DL_ >= 2) {
      calcs1 << "λ" << y->getVariableName() ;
      calcs1 << "(" <<  x->getDomain()[xi] << ")" << endl << "= " ;
    }
    double outer_sum = 0.0;
    for (int yi = 0; yi < y->getDomainSize(); yi++) {
      if (DL_ >= 2) {
        (yi == 0) ? calcs1 << "[" : calcs1 << endl << "+ [" ;
        if (DL_ >= 3) {
          (yi == 0) ? calcs2 << "[" : calcs2 << endl << "+ [" ;
        }
      } 
      double inner_sum = 0.0;
      vector<pair<int, int> > constraints;
      constraints.push_back (make_pair (0, yi));
      constraints.push_back (make_pair (parentIndex, xi));
      vector<CptEntry> entries = y->getCptEntries (constraints);
      for (unsigned int k = 0; k < entries.size(); k++) {
        double prod = y->getProbability (entries[k]);
        if (DL_ >= 2) {
          if (k != 0) calcs1 << " + " ;
          calcs1 << y->entryToString (entries[k]);
          if (DL_ >= 3) {
            (k == 0) ? calcs2 << "(" << prod : calcs2 << " + (" << prod;
          }
        }
        vector<int> insts = entries[k].getDomainInstantiations();
        for (unsigned int i = 0; i < yParents.size(); i++) {
          if (yParents[i] != x) {
            double val = yParents[i]->getPiMessage (y, insts[i + 1]);
            prod *= val;
            if (DL_ >= 2) {
              calcs1 << ".π" << y->getVariableName();
              calcs1 << "(" << yParents[i]->getDomain()[insts[i + 1]] << ")" ;
              if (DL_ >= 3) calcs2 << "x" << val;
            }

          }
        }
        inner_sum += prod;
        if (DL_ >= 3) {
          calcs2 << ")" ;
        }
      }
      outer_sum += inner_sum * y->getLambdaValue (yi);
      if (DL_ >= 2) {
        calcs1 << "].λ(" << y->getDomain()[yi] << ")";
        if (DL_ >= 3) calcs2 << "]x" << y->getLambdaValue (yi);
      }
    }
    x->setLambdaMessage (y, xi, outer_sum);
    if (DL_ >= 2) {
      cout << calcs1.str();
      if (DL_ >= 3) cout << endl << "= " << calcs2.str();
      cout << " = " << outer_sum << endl;
    }
  }
}



void
BpNetwork::runIterativeBpSolver()
{
  int nIter = 0;
  maxIter_  = 100;
  bool converged = false;
  while (nIter < maxIter_ && !converged) {
    if (DL_ >= 1) {
      cout << endl << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
      cout << " Iteration " << nIter + 1 << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
    }

    for (unsigned int i = 0; i < nodes_.size(); i++) {
      BpNode* x = static_cast<BpNode*>(nodes_[i]);
      vector<BpNode*> xParents = cast (x->getParents());
      for (unsigned int j = 0; j < xParents.size(); j++) {
        //if (!xParents[j]->hasEvidence()) {
        if (DL_ >= 1) {
          cout << endl << "λ message " << x->getVariableName();
          cout << " --> " << xParents[j]->getVariableName() << endl;
        }
  	    updateLambdaMessages (xParents[j], x);
        //}
      }
    }

    for (unsigned int i = 0; i < nodes_.size(); i++) {
      BpNode* x = static_cast<BpNode*>(nodes_[i]);
      vector<BpNode*> xChilds = cast (x->getChilds());
      for (unsigned int j = 0; j < xChilds.size(); j++) {
        if (DL_ >= 1) {
          cout << endl << "π message " << x->getVariableName();
          cout << " --> " << xChilds[j]->getVariableName() << endl;
        }
        updatePiMessages (x, xChilds[j]);
      }
    }

    /*
    for (unsigned int i = 0; i < nodes_.size(); i++) {
      BpNode* x = static_cast<BpNode*>(nodes_[i]);
      vector<BpNode*> xChilds = cast (x->getChilds());
      for (unsigned int j = 0; j < xChilds.size(); j++) {
        if (DL_ >= 1) {
          cout << "π message " << x->getVariableName();
          cout << " --> " << xChilds[j]->getVariableName() << endl;
        }
        updatePiMessages (x, xChilds[j]);
      }
      vector<BpNode*> xParents = cast (x->getParents());
      for (unsigned int j = 0; j < xParents.size(); j++) {
        //if (!xParents[j]->hasEvidence()) {
        if (DL_ >= 1) {
          cout << "λ message " << x->getVariableName();
          cout << " --> " << xParents[j]->getVariableName() << endl;
        }
  	    updateLambdaMessages (xParents[j], x);
        //}
      }
    }
    */

    for (unsigned int i = 0; i < nodes_.size(); i++) {
      BpNode* x = static_cast<BpNode*> (nodes_[i]);
      //cout << endl << "SWAPING MESSAGES FOR " << x->getVariableName() << ":" ;
      //cout << endl << endl;
      //printCurrentStatusOf (x);
      x->swapMessages();
      x->normalizeMessages();
      //cout << endl << "messages swaped " << endl;
      //printCurrentStatusOf (x);
    }

    converged = true;
    for (unsigned int i = 0; i < nodes_.size(); i++) {
      BpNode* x = static_cast<BpNode*>(nodes_[i]);
      if (DL_ >= 1) {
        cout << endl << "var " << x->getVariableName() << ":" << endl;
      }
      //if (!x->hasEvidence()) {
        updatePiValues (x);
        updateLambdaValues (x);
        double change = x->getBeliefChange();
        if (DL_ >= 1) {
          cout << "belief change = " << change << endl;
        }
        if (change > stableThreashold_) {
          converged = false;
        }
      //}
    }

    if (converged) {
      // converged = false;
    }
    if (DL_ >= 2) {
      cout << endl;
      printCurrentStatus();
    }
    nIter++;
  }

  if (DL_ >= 1) {
    cout << endl;
    if (converged) {
      cout << "Iterative belief propagation converged in " ; 
      cout << nIter << " iterations" << endl;
    } else {
      cout << "Iterative belief propagation converged didn't converge" ;
      cout << endl;
    }
    if (DL_ == 1) {
      cout << endl;
      printBeliefs();
    }
    cout << endl;
  }
}



void
BpNetwork::addNode (string varName,
                    vector<BayesianNode*> parents,
                    int evidence,
                    int distId)
{
  for (unsigned int i = 0; i < dists_.size(); i++) {
    if (dists_[i]->id == distId) {
      BpNode* node = new BpNode (varName, parents, dists_[i], evidence);
      nodes_.push_back (node);
      break;
    }
  }
}



void
BpNetwork::addNode (string varName,
                    vector<BayesianNode*> parents, 
                    double* params,
                    int nParams,
                    vector<string> domain)
{
  Distribution* dist = new Distribution (params, nParams, domain);
  BpNode* node = new BpNode (varName, parents, dist);
  dists_.push_back (dist);
  nodes_.push_back (node);
}



vector<BpNode*> 
BpNetwork::cast (vector<BayesianNode*> nodes)
{
  vector<BpNode*> castedNodes (nodes.size());
  for (unsigned int i = 0; i < nodes.size(); i++) {
    castedNodes[i] = static_cast<BpNode*> (nodes[i]);
  }
  return castedNodes;
}

