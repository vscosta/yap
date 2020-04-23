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


template <class T> inline size_t
hash_combine (size_t seed, const T& v)
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

class CountingBp : public GroundSolver {
  public:
    CountingBp (const FactorGraph& fg);

   ~CountingBp();

    void printSolverFlags() const;

    Params solveQuery (VarIds);

    static void setFindIdenticalFactorsFlag (bool fif) { fif_ = fif; }

  private:
    typedef long                                        Color;
    typedef std::vector<Color>                          Colors;

    typedef std::vector<std::pair<Color,unsigned>>      VarSignature;
    typedef std::vector<Color>                          FacSignature;

    typedef std::vector<VarCluster*>                    VarClusters;
    typedef std::vector<FacCluster*>                    FacClusters;

    typedef std::unordered_map<unsigned, Color>         DistColorMap;
    typedef std::unordered_map<unsigned, Colors>        VarColorMap;
    typedef std::unordered_map<VarSignature, VarNodes>  VarSignMap;
    typedef std::unordered_map<FacSignature, FacNodes>  FacSignMap;
    typedef std::unordered_map<VarId, VarCluster*>      VarClusterMap;

    Color getNewColor();

    Color getColor (const VarNode* vn) const;

    Color getColor (const FacNode* fn) const;

    void setColor (const VarNode* vn, Color c);

    void setColor (const FacNode* fn, Color c);

    void findIdenticalFactors();

    void setInitialColors();

    void createGroups();

    void createClusters (const VarSignMap&, const FacSignMap&);

    VarSignature getSignature (const VarNode*);

    FacSignature getSignature (const FacNode*);

    void printGroups (const VarSignMap&, const FacSignMap&) const;

    VarId getRepresentative (VarId vid);

    FacNode* getRepresentative (FacNode*);

    FactorGraph* getCompressedFactorGraph();

    std::vector<std::vector<unsigned>> getWeights() const;

    unsigned getWeight (const FacCluster*, const VarCluster*,
        size_t index) const;

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



inline CountingBp::Color
CountingBp::getNewColor()
{
  ++ freeColor_;
  return freeColor_ - 1;
}



inline CountingBp::Color
CountingBp::getColor (const VarNode* vn) const
{
  return varColors_[vn->getIndex()];
}



inline CountingBp::Color
CountingBp::getColor (const FacNode* fn) const
{
  return facColors_[fn->getIndex()];
}



inline void
CountingBp::setColor (const VarNode* vn, CountingBp::Color c)
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

