#ifndef HORUS_CFACTORGRAPH_H
#define HORUS_CFACTORGRAPH_H

#include <unordered_map>

#include "FactorGraph.h"
#include "Factor.h"
#include "Horus.h"

class VarCluster;
class FacCluster;
class Distribution;
class Signature;

class SignatureHash;


typedef long Color;

typedef unordered_map<unsigned, vector<Color>> VarColorMap;

typedef unordered_map<unsigned, Color>    DistColorMap;
typedef unordered_map<VarId, VarCluster*> VarId2VarCluster;

typedef vector<VarCluster*> VarClusters;
typedef vector<FacCluster*> FacClusters;

typedef unordered_map<Signature, VarNodes, SignatureHash> VarSignMap;
typedef unordered_map<Signature, FacNodes, SignatureHash> FacSignMap;



struct Signature
{
  Signature (unsigned size) : colors(size) {  }

  bool operator< (const Signature& sig) const
  {
    if (colors.size() < sig.colors.size()) {
      return true;
    } else if (colors.size() > sig.colors.size()) {
      return false;
    } else {
      for (unsigned i = 0; i < colors.size(); i++) {
        if (colors[i] < sig.colors[i]) {
          return true;
        } else if (colors[i] > sig.colors[i]) {
          return false;
        }
      }
    }
    return false;
  }

  bool operator== (const Signature& sig) const
  {
    if (colors.size() != sig.colors.size()) {
      return false;
    }
    for (unsigned i = 0; i < colors.size(); i++) {
      if (colors[i] != sig.colors[i]) {
        return false;
      }
    }
    return true;
  }

  vector<Color> colors;
};



struct SignatureHash
{
  size_t operator() (const Signature &sig) const
  {
    size_t val = hash<size_t>()(sig.colors.size());
    for (unsigned i = 0; i < sig.colors.size(); i++) {
      val ^= hash<size_t>()(sig.colors[i]);
    }
    return val;
  }
};



class VarCluster
{
  public:
    VarCluster (const VarNodes& vs)
    {
      for (unsigned i = 0; i < vs.size(); i++) {
        groundVars_.push_back (vs[i]);
      }
    }

    void addFacCluster (FacCluster* fc)
    {
      facClusters_.push_back (fc);
    }

    const FacClusters& getFacClusters (void) const
    {
      return facClusters_;
    }

    VarNode* getRepresentativeVariable (void) const { return representVar_; }

    void setRepresentativeVariable (VarNode* v) { representVar_ = v; }

    const VarNodes& getGroundVarNodes (void) const { return groundVars_; }

  private:
    VarNodes        groundVars_;
    FacClusters   facClusters_;
    VarNode*      representVar_;
};


class FacCluster
{
  public:
    FacCluster (const FacNodes& groundFactors, const VarClusters& vcs)
    {
      groundFactors_ = groundFactors;
      varClusters_ = vcs;
      for (unsigned i = 0; i < varClusters_.size(); i++) {
        varClusters_[i]->addFacCluster (this);
      }
    }
 
    const VarClusters& getVarClusters (void) const
    {
      return varClusters_;
    }
  
    bool containsGround (const FacNode* fn)
    {
      for (unsigned i = 0; i < groundFactors_.size(); i++) {
        if (groundFactors_[i] == fn) {
          return true;
        }
      }
      return false;
    }

    FacNode* getRepresentativeFactor (void) const
    {
      return representFactor_;
    }

    void setRepresentativeFactor (FacNode* fn) 
    { 
      representFactor_ = fn;
    }

    const FacNodes& getGroundFactors (void) const
    {
      return groundFactors_;
    }

 
  private:
    FacNodes        groundFactors_;
    VarClusters   varClusters_;
    FacNode*      representFactor_;
};


class CFactorGraph
{
  public:
    CFactorGraph (const FactorGraph&);

   ~CFactorGraph (void);

    const VarClusters& getVarClusters (void) { return varClusters_; }

    const FacClusters& getFacClusters (void) { return facClusters_; }

    VarNode* getEquivalentVariable (VarId vid)
    {
      VarCluster* vc = vid2VarCluster_.find (vid)->second;
      return vc->getRepresentativeVariable();
    }

    FactorGraph* getGroundFactorGraph (void) const;

    unsigned getEdgeCount (const FacCluster*, const VarCluster*) const;
 
    static bool checkForIdenticalFactors;
 
  private:
    Color getFreeColor (void)
    {
      ++ freeColor_;
      return freeColor_ - 1;
    }

    Color getColor (const VarNode* vn) const
    {
      return varColors_[vn->getIndex()];
    }
    Color getColor (const FacNode* fn) const  {
      return facColors_[fn->getIndex()];
    }

    void setColor (const VarNode* vn, Color c)
    {
      varColors_[vn->getIndex()] = c;
    }

    void setColor (const FacNode* fn, Color  c)
    {
      facColors_[fn->getIndex()] = c;
    }

    VarCluster* getVariableCluster (VarId vid) const
    {
      return vid2VarCluster_.find (vid)->second;
    }

    void setInitialColors (void);

    void createGroups (void);

    void createClusters (const VarSignMap&, const FacSignMap&);

    const Signature& getSignature (const VarNode*);

    const Signature& getSignature (const FacNode*);

    void printGroups (const VarSignMap&, const FacSignMap&) const;

    Color               freeColor_;
    vector<Color>       varColors_;
    vector<Color>       facColors_;
    vector<Signature>   varSignatures_;
    vector<Signature>   facSignatures_;
    VarClusters       varClusters_;
    FacClusters       facClusters_;
    VarId2VarCluster    vid2VarCluster_;
    const FactorGraph*  groundFg_;
};

#endif // HORUS_CFACTORGRAPH_H

