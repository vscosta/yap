#ifndef HORUS_CFACTORGRAPH_H
#define HORUS_CFACTORGRAPH_H

#include <unordered_map>

#include "FactorGraph.h"
#include "Factor.h"
#include "Util.h"
#include "Horus.h"

class VarCluster;
class FacCluster;
class Signature;
class SignatureHash;

typedef long Color;
typedef vector<Color> Colors;

typedef unordered_map<unsigned, Color>  DistColorMap;
typedef unordered_map<unsigned, Colors> VarColorMap;

typedef unordered_map<Signature, VarNodes, SignatureHash> VarSignMap;
typedef unordered_map<Signature, FacNodes, SignatureHash> FacSignMap;

typedef vector<VarCluster*> VarClusters;
typedef vector<FacCluster*> FacClusters;

typedef unordered_map<VarId, VarCluster*> VarId2VarCluster;


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

  Colors colors;
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
    VarCluster (const VarNodes& vs) : members_(vs) { }

    const VarNodes& members (void) const { return members_; }

    const FacClusters& facClusters (void) const { return facClusters_; }

    void addFacCluster (FacCluster* fc) { facClusters_.push_back (fc); }

    VarNode* getRepresentative (void) const { return repr_; }

    void setRepresentative (VarNode* vn) { repr_ = vn; }

  private:
    VarNodes     members_;
    FacClusters  facClusters_;
    VarNode*     repr_;
};


class FacCluster
{
  public:
    FacCluster (const FacNodes& fcs, const VarClusters& vcs)
        : members_(fcs), varClusters_(vcs) 
    {
      for (unsigned i = 0; i < varClusters_.size(); i++) {
        varClusters_[i]->addFacCluster (this);
      }
    }

    const FacNodes& members (void) const { return members_; }
 
    const VarClusters& varClusters (void) const { return varClusters_; }
  
    FacNode* getRepresentative (void) const { return repr_; }

    void setRepresentative (FacNode* fn) { repr_ = fn; }

    bool containsGround (const FacNode* fn) const
    {
      return std::find (members_.begin(), members_.end(), fn) 
          != members_.end();
    }
 
  private:
    FacNodes     members_;
    VarClusters  varClusters_;
    FacNode*     repr_;
};


class CFactorGraph
{
  public:
    CFactorGraph (const FactorGraph&);

   ~CFactorGraph (void);

    const VarClusters& varClusters (void) { return varClusters_; }

    const FacClusters& facClusters (void) { return facClusters_; }

    VarNode* getEquivalentVariable (VarId vid)
    {
      VarCluster* vc = vid2VarCluster_.find (vid)->second;
      return vc->getRepresentative();
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

    void findIdenticalFactors (void);

    void setInitialColors (void);

    void createGroups (void);

    void createClusters (const VarSignMap&, const FacSignMap&);

    const Signature& getSignature (const VarNode*);

    const Signature& getSignature (const FacNode*);

    void printGroups (const VarSignMap&, const FacSignMap&) const;

    Color               freeColor_;
    Colors              varColors_;
    Colors              facColors_;
    vector<Signature>   varSignatures_;
    vector<Signature>   facSignatures_;
    VarClusters         varClusters_;
    FacClusters         facClusters_;
    VarId2VarCluster    vid2VarCluster_;
    const FactorGraph*  groundFg_;
};

#endif // HORUS_CFACTORGRAPH_H

