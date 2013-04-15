#ifndef YAP_PACKAGES_CLPBN_HORUS_FACTORGRAPH_H_
#define YAP_PACKAGES_CLPBN_HORUS_FACTORGRAPH_H_

#include <vector>
#include <unordered_map>
#include <string>
#include <fstream>

#include "Factor.h"
#include "BayesBallGraph.h"
#include "Horus.h"


namespace Horus {

class FacNode;


class VarNode : public Var {
  public:
    VarNode (VarId varId, unsigned nrStates,
        int evidence = Constants::unobserved)
        : Var (varId, nrStates, evidence) { }

    VarNode (const Var* v) : Var (v) { }

    void addNeighbor (FacNode* fn) { neighs_.push_back (fn); }

    const FacNodes& neighbors() const { return neighs_; }

  private:
    FacNodes neighs_;

    DISALLOW_COPY_AND_ASSIGN (VarNode);
};



class FacNode {
  public:
    FacNode (const Factor& f) : factor_(f), index_(-1) { }

    const Factor& factor() const { return factor_; }

    Factor& factor() { return factor_; }

    void addNeighbor (VarNode* vn) { neighs_.push_back (vn); }

    const VarNodes& neighbors() const { return neighs_; }

    size_t getIndex() const { return index_; }

    void setIndex (size_t index) { index_ = index; }

    std::string getLabel() { return factor_.getLabel(); }

  private:
    VarNodes  neighs_;
    Factor    factor_;
    size_t    index_;

    DISALLOW_COPY_AND_ASSIGN (FacNode);
};



class FactorGraph {
  public:
    FactorGraph() : bayesFactors_(false) { }

    FactorGraph (const FactorGraph&);

   ~FactorGraph();

    const VarNodes& varNodes() const { return varNodes_; }

    const FacNodes& facNodes() const { return facNodes_; }

    void setFactorsAsBayesian() { bayesFactors_ = true; }

    bool bayesianFactors() const { return bayesFactors_; }

    size_t nrVarNodes() const { return varNodes_.size(); }

    size_t nrFacNodes() const { return facNodes_.size(); }

    VarNode* getVarNode (VarId vid) const;

    void addFactor (const Factor& factor);

    void addVarNode (VarNode*);

    void addFacNode (FacNode*);

    void addEdge (VarNode*, FacNode*);

    bool isTree() const;

    BayesBallGraph& getStructure();

    void print() const;

    void exportToLibDai (const char*) const;

    void exportToUai (const char*) const;

    void exportToGraphViz (const char*) const;

    FactorGraph& operator= (const FactorGraph&);

    static FactorGraph readFromUaiFormat (const char*);

    static FactorGraph readFromLibDaiFormat (const char*);

    static bool exportToLibDai() { return exportLd_; }

    static bool exportToUai() { return exportUai_; }

    static bool exportGraphViz() { return exportGv_; }

    static bool printFactorGraph() { return printFg_; }

    static void enableExportToLibDai() { exportLd_ = true; }

    static void disableExportToLibDai() { exportLd_ = false; }

    static void enableExportToUai() { exportUai_ = true; }

    static void disableExportToUai() { exportUai_ = false; }

    static void enableExportToGraphViz() { exportGv_ = true; }

    static void disableExportToGraphViz() { exportGv_ = false; }

    static void enablePrintFactorGraph() { printFg_ = true; }

    static void disablePrintFactorGraph() { printFg_ = false; }

  private:
    typedef std::unordered_map<unsigned, VarNode*> VarMap;

    void clone (const FactorGraph& fg);

    bool containsCycle() const;

    bool containsCycle (const VarNode*, const FacNode*,
        std::vector<bool>&, std::vector<bool>&) const;

    bool containsCycle (const FacNode*, const VarNode*,
        std::vector<bool>&, std::vector<bool>&) const;

    static void ignoreLines (std::ifstream&);

    VarNodes        varNodes_;
    FacNodes        facNodes_;
    VarMap          varMap_;
    BayesBallGraph  structure_;
    bool            bayesFactors_;

    static bool     exportLd_;
    static bool     exportUai_;
    static bool     exportGv_;
    static bool     printFg_;
};



inline VarNode*
FactorGraph::getVarNode (VarId vid) const
{
  VarMap::const_iterator it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



struct sortByVarId {
  bool operator()(VarNode* vn1, VarNode* vn2) {
    return vn1->varId() < vn2->varId();
}};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_FACTORGRAPH_H_

