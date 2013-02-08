#ifndef YAP_PACKAGES_CLPBN_HORUS_COUNTINGBP_H_
#define YAP_PACKAGES_CLPBN_HORUS_COUNTINGBP_H_

#include <vector>
#include <unordered_map>

#include "GroundSolver.h"
#include "FactorGraph.h"
#include "Horus.h"


namespace Horus {

class VarCluster;
class FacCluster;
class WeightedBp;

typedef long Color;
typedef std::vector<Color> Colors;
typedef std::vector<std::pair<Color,unsigned>> VarSignature;
typedef std::vector<Color> FacSignature;

typedef std::unordered_map<unsigned, Color>  DistColorMap;
typedef std::unordered_map<unsigned, Colors> VarColorMap;

typedef std::unordered_map<VarSignature, VarNodes> VarSignMap;
typedef std::unordered_map<FacSignature, FacNodes> FacSignMap;

typedef std::unordered_map<VarId, VarCluster*> VarClusterMap;

typedef std::vector<VarCluster*> VarClusters;
typedef std::vector<FacCluster*> FacClusters;


template <class T>
inline size_t hash_combine (size_t seed, const T& v)
{
  return seed ^ (std::hash<T>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
}

}  // namespace Horus


namespace std {

template <typename T1, typename T2> struct hash<std::pair<T1,T2>> {
  size_t operator() (const std::pair<T1,T2>& p) const {
    return Horus::hash_combine (std::hash<T1>()(p.first), p.second);
}};

template <typename T> struct hash<std::vector<T>>
{
  size_t operator() (const std::vector<T>& vec) const
  {
    size_t h = 0;
    typename std::vector<T>::const_iterator first = vec.begin();
    typename std::vector<T>::const_iterator last  = vec.end();
    for (; first != last; ++first) {
      h = Horus::hash_combine (h, *first);
    }
    return h;
  }
};

}  // namespace std


namespace Horus {

class VarCluster
{
  public:
    VarCluster (const VarNodes& vs) : members_(vs) { }

    const VarNode* first (void) const { return members_.front(); }

    const VarNodes& members (void) const { return members_; }

    VarNode* representative (void) const { return repr_; }

    void setRepresentative (VarNode* vn) { repr_ = vn; }

  private:
    VarNodes  members_;
    VarNode*  repr_;

    DISALLOW_COPY_AND_ASSIGN (VarCluster);
};


class FacCluster
{
  public:
    FacCluster (const FacNodes& fcs, const VarClusters& vcs)
        : members_(fcs), varClusters_(vcs) { }

    const FacNode* first (void) const { return members_.front(); }

    const FacNodes& members (void) const { return members_; }

    FacNode* representative (void) const { return repr_; }

    void setRepresentative (FacNode* fn) { repr_ = fn; }

    VarClusters& varClusters (void) { return varClusters_; }

  private:
    FacNodes     members_;
    FacNode*     repr_;
    VarClusters  varClusters_;

    DISALLOW_COPY_AND_ASSIGN (FacCluster);
};


class CountingBp : public GroundSolver
{
  public:
    CountingBp (const FactorGraph& fg);

   ~CountingBp (void);

    void printSolverFlags (void) const;

    Params solveQuery (VarIds);

    static void setFindIdenticalFactorsFlag (bool fif) { fif_ = fif; }

  private:
    Color getNewColor (void);

    Color getColor (const VarNode* vn) const;

    Color getColor (const FacNode* fn) const;

    void setColor (const VarNode* vn, Color c);

    void setColor (const FacNode* fn, Color c);

    void findIdenticalFactors (void);

    void setInitialColors (void);

    void createGroups (void);

    void createClusters (const VarSignMap&, const FacSignMap&);

    VarSignature getSignature (const VarNode*);

    FacSignature getSignature (const FacNode*);

    void printGroups (const VarSignMap&, const FacSignMap&) const;

    VarId getRepresentative (VarId vid);

    FacNode* getRepresentative (FacNode*);

    FactorGraph* getCompressedFactorGraph (void);

    std::vector<std::vector<unsigned>> getWeights (void) const;

    unsigned getWeight (const FacCluster*,
        const VarCluster*, size_t index) const;

    Color               freeColor_;
    Colors              varColors_;
    Colors              facColors_;
    VarClusters         varClusters_;
    FacClusters         facClusters_;
    VarClusterMap       varClusterMap_;
    const FactorGraph*  compressedFg_;
    WeightedBp*         solver_;

    static bool fif_;

    DISALLOW_COPY_AND_ASSIGN (CountingBp);
};



inline Color
CountingBp::getNewColor (void)
{
  ++ freeColor_;
  return freeColor_ - 1;
}



inline Color
CountingBp::getColor (const VarNode* vn) const
{
  return varColors_[vn->getIndex()];
}



inline Color
CountingBp::getColor (const FacNode* fn) const
{
  return facColors_[fn->getIndex()];
}



inline void
CountingBp::setColor (const VarNode* vn, Color c)
{
  varColors_[vn->getIndex()] = c;
}



inline void
CountingBp::setColor (const FacNode* fn, Color  c)
{
  facColors_[fn->getIndex()] = c;
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_COUNTINGBP_H_

