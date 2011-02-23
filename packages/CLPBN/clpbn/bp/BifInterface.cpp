#include <iostream>
#include <sstream>
#include <map>

#include "xmlParser/xmlParser.h"

#include "BifInterface.h"
#include "BayesianNetwork.h"
#include "BayesianNode.h" 


void
BifInterface::createNetworkFromXML (BayesianNetwork* bn, const char* fileName)
{
  map<string, vector<string> > domains;
  XMLNode xMainNode = XMLNode::openFileHelper (fileName, "BIF");
  // only the first network is parsed, others are ignored
  XMLNode xNode = xMainNode.getChildNode ("NETWORK");
  int nVars = xNode.nChildNode ("VARIABLE");
  for (int i = 0; i < nVars; i++) {
    XMLNode var = xNode.getChildNode ("VARIABLE", i);
    string type = var.getAttribute ("TYPE");
    if (type != "nature") {
      cerr << "error: only \"nature\" variables are supported" << endl;
      abort();
    }
    vector<string> domain;
    string varName = var.getChildNode("NAME").getText();
    int domainSize = var.nChildNode ("OUTCOME");
    for (int j = 0; j < domainSize; j++) {
      domain.push_back (var.getChildNode("OUTCOME", j).getText());
    }
    domains.insert (make_pair (varName, domain));
  }

  int nDefs = xNode.nChildNode ("DEFINITION");
  if (nVars != nDefs) {
    cerr << "error: different number of variables and definitions";
    cerr << endl;
  }

  for (int i = 0; i < nDefs; i++) {
    XMLNode def = xNode.getChildNode ("DEFINITION", i);
    string nodeName = def.getChildNode("FOR").getText();
    map<string, vector<string> >::const_iterator iter = domains.find (nodeName);
    if (iter == domains.end()) {
      cerr << "error: unknow variable `" << nodeName << "'" << endl;
      abort();
    }
    vector<BayesianNode*> parents;
    int nParams = iter->second.size();
    for (int j = 0; j < def.nChildNode ("GIVEN"); j++) {
      string parentName = def.getChildNode("GIVEN", j).getText();
      BayesianNode* parentNode = bn->getNode (parentName);
      if (parentNode) {
        nParams *= parentNode->getDomainSize();
        parents.push_back (parentNode);
      }
      else {
        cerr << "error: unknow variable `" << parentName << "'" << endl;
        abort();
      }
    }

    int c = 0;
    double* params = new double [nParams];
    stringstream s (def.getChildNode("TABLE").getText());
    while (!s.eof() && c < nParams) {
      s >> params[c];
      c++;
    }
    if (c != nParams) {
      cerr << "error: invalid number of parameters " ;
      cerr << "for variable `" << nodeName << "'" << endl;
      abort();
    }
   
    params = reorderParameters (params, nParams, iter->second.size());
    bn->addNode (nodeName, parents, params, nParams, iter->second);
  }
}


double*
BifInterface::reorderParameters (double* params, 
                                 int nParams,
                                 int domainSize)
{
  // the interchange format for bayesian networks saves the probabilities 
  // in the following order:
  // p(a1|b1,c1) p(a2|b1,c1) p(a1|b1,c2) p(a2|b1,c2) p(a1|b2,c1) p(a2|b2,c1)
  // p(a1|b2,c2) p(a2|b2,c2).
  //
  // however, in clpbn we keep the probabilities in this order:
  // p(a1|b1,c1) p(a1|b1,c2) p(a1|b2,c1) p(a1|b2,c2) p(a2|b1,c1) p(a2|b1,c2)
  // p(a2|b2,c1) p(a2|b2,c2).

  int count         = 0;
  int index1        = 0;
  int index2        = 0;
  int rowSize       = nParams / domainSize;
  double* reordered = new double [nParams];

  while (index1 < nParams) {
    index2 = count;
    for (int i = 0; i < domainSize; i++) {
      reordered[index2] = params[index1];
      index1 += 1;
      index2 += rowSize;
    }
    count++;
  }

  delete [] params;
  return reordered;
}

