#ifndef BP_FACTOR_GRAPH_H
#define BP_FACTOR_GRAPH_H

#include <vector>

#include "GraphicalModel.h"
#include "Shared.h"

using namespace std;

class FgVarNode;
class Factor;
class BayesNet;

class FactorGraph : public GraphicalModel
{
  public:
    FactorGraph (void) {};
    FactorGraph (const char*);
    FactorGraph (const BayesNet&);
   ~FactorGraph (void);
   
    void                addVariable (FgVarNode*);
    void                removeVariable (const FgVarNode*);
    void                addFactor (Factor*);
    void                removeFactor (const Factor*);
    VarSet              getVariables (void) const;
    Variable*           getVariable (unsigned) const;
    void                setIndexes (void);
    void                freeDistributions (void);
    void                printGraphicalModel (void) const;
    void                exportToDotFormat (const char*) const;
    void                exportToUaiFormat (const char*) const;
  
    const FgVarSet& getFgVarNodes (void) const { return varNodes_; }
    const FactorSet& getFactors (void) const   { return factors_; }

    FgVarNode* getFgVarNode (Vid vid) const
    {
      IndexMap::const_iterator it = indexMap_.find (vid);
      if (it == indexMap_.end()) {
        return 0;
      } else {
        return varNodes_[it->second];
      }
    }

  private:
    DISALLOW_COPY_AND_ASSIGN (FactorGraph);

    FgVarSet            varNodes_;
    FactorSet           factors_;
    IndexMap            indexMap_;
};

#endif // BP_FACTOR_GRAPH_H

