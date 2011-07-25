#ifndef BP_LIFTED_FG_H
#define BP_LIFTED_FG_H

#include <unordered_map>

#include "FactorGraph.h"
#include "FgVarNode.h"
#include "Factor.h"
#include "Shared.h"

class VarCluster;
class FactorCluster;
class Distribution;

typedef long                     Color;
typedef vector<Color>            Signature;
typedef vector<VarCluster*>      VarClusterSet;
typedef vector<FactorCluster*>   FactorClusterSet;

typedef map<string, FgVarSet>  VarSignMap;
typedef map<string, FactorSet> FactorSignMap;

typedef map<unsigned, vector<Color> > VarColorMap;
typedef map<Distribution*, Color> DistColorMap;

typedef map<Vid, VarCluster*> Vid2VarCluster;


class VarCluster
{
  public:
    VarCluster (CFgVarSet vs)
    {
      for (unsigned i = 0; i < vs.size(); i++) {
        groundVars_.push_back (vs[i]);
      }
    }

    void addFactorCluster (FactorCluster* fc)
    {
      factorClusters_.push_back (fc);
    }

    const FactorClusterSet& getFactorClusters (void) const
    {
      return factorClusters_;
    }

    FgVarNode* getRepresentativeVariable (void) const { return representVar_; }
    void setRepresentativeVariable (FgVarNode* v)     { representVar_ = v; }
    CFgVarSet getGroundFgVarNodes (void) const        { return groundVars_; }

  private:
    FgVarSet          groundVars_;
    FactorClusterSet  factorClusters_;
    FgVarNode*        representVar_;
};


class FactorCluster
{
  public:
    FactorCluster (CFactorSet groundFactors, const VarClusterSet& vcs)
    {
      groundFactors_ = groundFactors;
      varClusters_ = vcs;
      for (unsigned i = 0; i < varClusters_.size(); i++) {
        varClusters_[i]->addFactorCluster (this);
      }
    }
 
    const VarClusterSet& getVarClusters (void) const
    {
      return varClusters_;
    }
  
    bool containsGround (const Factor* f)
    {
      for (unsigned i = 0; i < groundFactors_.size(); i++) {
        if (groundFactors_[i] == f) {
          return true;
        }
      }
      return false;
    }

    Factor* getRepresentativeFactor (void) const  { return representFactor_; }
    void setRepresentativeFactor (Factor* f)      { representFactor_ = f; }
    CFactorSet getGroundFactors (void) const      { return groundFactors_; }

 
  private:
    FactorSet      groundFactors_;
    VarClusterSet  varClusters_;
    Factor*        representFactor_;
};


class LiftedFG
{
  public:
    LiftedFG (const FactorGraph&);
   ~LiftedFG (void);

    FactorGraph*    getCompressedFactorGraph (void);
    unsigned        getGroundEdgeCount (FactorCluster*, VarCluster*) const;
    void            printGroups (const VarSignMap& varGroups,
                                 const FactorSignMap& factorGroups) const;

    FgVarNode* getEquivalentVariable (Vid vid)
    {
      VarCluster* vc = vid2VarCluster_.find (vid)->second;
      return vc->getRepresentativeVariable();
    }
  
    const VarClusterSet&    getVariableClusters (void) { return varClusters_; }
    const FactorClusterSet& getFactorClusters (void) { return factorClusters_; }
 
  private:
    string  getSignatureId (FgVarNode*) const;
    string  getSignatureId (Factor*) const;

    Color getFreeColor (void)           { return ++freeColor_ -1; }
    Color getColor (FgVarNode* v) const { return varColors_[v->getIndex()]; }
    Color getColor (Factor* f) const    { return factorColors_[f->getIndex()]; }

    void setColor (FgVarNode* v, Color c)
    {
      varColors_[v->getIndex()] = c;
    }

    void setColor (Factor* f, Color  c)
    {
      factorColors_[f->getIndex()] = c;
    }

    VarCluster* getVariableCluster (Vid vid) const
    {
      return vid2VarCluster_.find (vid)->second;
    }

    Color               freeColor_;
    vector<Color>       varColors_;
    vector<Color>       factorColors_;
    VarClusterSet       varClusters_;
    FactorClusterSet    factorClusters_;
    Vid2VarCluster      vid2VarCluster_;
    const FactorGraph*  groundFg_;
};

#endif // BP_LIFTED_FG_H

