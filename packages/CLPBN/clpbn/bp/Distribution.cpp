#include <vector>
#include <string>

#include <Distribution.h>

Distribution::Distribution (int id,
                            double* params,
                            int nParams,
                            vector<string> domain)
{
  this->id      = id;
  this->params  = params;
  this->nParams = nParams;
  this->domain  = domain; 
}


Distribution::Distribution (double* params,
                            int nParams,
                            vector<string> domain)
{
  this->id      = -1;
  this->params  = params;
  this->nParams = nParams;
  this->domain  = domain; 
}



/*
Distribution::~Distribution()
{
  delete params;
  for (unsigned int i = 0; i < cptEntries.size(); i++) {
    delete cptEntries[i];
  }
}
*/


