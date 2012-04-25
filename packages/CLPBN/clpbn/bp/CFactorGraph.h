#ifndef HORUS_CFACTORGRAPH_H
#define HORUS_CFACTORGRAPH_H

#include <unordered_map>

#include "FactorGraph.h"
#include "Factor.h"
#include "Util.h"
#include "Horus.h"

class VarCluster;
class FacCluster;
class VarSignatureHash;
class FacSignatureHash;

typedef long Color;
typedef vector<Color> Colors;
typedef vector<std::pair<Color,unsigned>> VarSignature;
typedef vector<Color> FacSignature;

typedef unordered_map<unsigned, Color>  DistColorMap;
typedef unordered_map<unsigned, Colors> VarColorMap;

typedef unordered_map<VarSignature, VarNodes, VarSignatureHash> VarSignMap;
typedef unordered_map<FacSignature, FacNodes, FacSignatureHash> FacSignMap;

typedef vector<VarCluster*> VarClusters;
typedef vector<FacCluster*> FacClusters;

typedef unordered_map<VarId, VarCluster*> VarId2VarCluster;


struct VarSignatureHash
{
  size_t operator() (const VarSignature &sig) const
  {
    size_t val = hash<size_t>()(sig.size());
    for (unsigned i = 0; i < sig.size(); i++) {
      val ^= hash<size_t>()(sig[i].first);
      val ^= hash<size_t>()(sig[i].second);
    }
    return val;
  }
};


struct FacSignatureHash
{
  size_t operator() (const FacSignature &sig) const
  {
    size_t val = hash<size_t>()(sig.size());
    for (unsigned i = 0; i < sig.size(); i++) {
      val ^= hash<size_t>()(sig[i]);
    }
    return val;
  }
};



class VarCluster
{
  public:
    VarCluster (const VarNodes& vs) : members_(vs) { }

    const VarNode* first (void) const { return members_.front(); }

    const VarNodes& members (void) const { return members_; }

    VarNode* representative (void) const { return repr_; }

    void setRepresentative (VarNode* vn) { repr_ = vn; }

  private:
    VarNodes     members_;
    VarNode*     repr_;
};


class FacCluster
{
  public:
    FacCluster (const FacNodes& fcs, const VarClusters& vcs)
        : members_(fcs), varClusters_(vcs) { }

    const FacNode* first (void) const { return members_.front(); }

    const FacNodes& members (void) const { return members_; }
 
    VarClusters& varClusters (void) { return varClusters_; }
  
    FacNode* representative (void) const { return repr_; }

    void setRepresentative (FacNode* fn) { repr_ = fn; }
 
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

    VarNode* getEquivalent (VarId vid)
    {
      VarCluster* vc = vid2VarCluster_.find (vid)->second;
      return vc->representative();
    }

    FactorGraph* getGroundFactorGraph (void);

    unsigned getEdgeCount (const FacCluster*,
        const VarCluster*, unsigned index) const;
 
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

    void findIdenticalFactors (void);

    void setInitialColors (void);

    void createGroups (void);

    void createClusters (const VarSignMap&, const FacSignMap&);

    VarSignature getSignature (const VarNode*);

    FacSignature getSignature (const FacNode*);

    void printGroups (const VarSignMap&, const FacSignMap&) const;

    Color                 freeColor_;
    Colors                varColors_;
    Colors                facColors_;
    VarClusters           varClusters_;
    FacClusters           facClusters_;
    VarId2VarCluster      vid2VarCluster_;
    const FactorGraph*    groundFg_;
};

#endif // HORUS_CFACTORGRAPH_H

