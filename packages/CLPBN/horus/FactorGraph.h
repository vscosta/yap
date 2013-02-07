#ifndef YAP_PACKAGES_CLPBN_HORUS_FACTORGRAPH_H_
#define YAP_PACKAGES_CLPBN_HORUS_FACTORGRAPH_H_

#include <vector>

#include "Factor.h"
#include "BayesBallGraph.h"
#include "Horus.h"

class FacNode;


class VarNode : public Var
{
  public:
    VarNode (VarId varId, unsigned nrStates,
        int evidence = Constants::NO_EVIDENCE)
        : Var (varId, nrStates, evidence) { }

    VarNode (const Var* v) : Var (v) { }

    void addNeighbor (FacNode* fn) { neighs_.push_back (fn); }

    const FacNodes& neighbors (void) const { return neighs_; }

  private:
    FacNodes neighs_;

    DISALLOW_COPY_AND_ASSIGN (VarNode);
};



class FacNode
{
  public:
    FacNode (const Factor& f) : factor_(f), index_(-1) { }

    const Factor& factor (void) const { return factor_; }

    Factor& factor (void) { return factor_; }

    void addNeighbor (VarNode* vn) { neighs_.push_back (vn); }

    const VarNodes& neighbors (void) const { return neighs_; }

    size_t getIndex (void) const { return index_; }

    void setIndex (size_t index) { index_ = index; }

    std::string getLabel (void) { return factor_.getLabel(); }

  private:
    VarNodes  neighs_;
    Factor    factor_;
    size_t    index_;

    DISALLOW_COPY_AND_ASSIGN (FacNode);
};



class FactorGraph
{
  public:
    FactorGraph (void) : bayesFactors_(false) { }

    FactorGraph (const FactorGraph&);

   ~FactorGraph (void);

    const VarNodes& varNodes (void) const { return varNodes_; }

    const FacNodes& facNodes (void) const { return facNodes_; }

    void setFactorsAsBayesian (void) { bayesFactors_ = true; }

    bool bayesianFactors (void) const { return bayesFactors_; }

    size_t nrVarNodes (void) const { return varNodes_.size(); }

    size_t nrFacNodes (void) const { return facNodes_.size(); }

    VarNode* getVarNode (VarId vid) const;

    void readFromUaiFormat (const char*);

    void readFromLibDaiFormat (const char*);

    void addFactor (const Factor& factor);

    void addVarNode (VarNode*);

    void addFacNode (FacNode*);

    void addEdge (VarNode*, FacNode*);

    bool isTree (void) const;

    BayesBallGraph& getStructure (void);

    void print (void) const;

    void exportToLibDai (const char*) const;

    void exportToUai (const char*) const;

    void exportToGraphViz (const char*) const;

    static bool exportToLibDai (void) { return exportLd_; }

    static bool exportToUai (void) { return exportUai_; }

    static bool exportGraphViz (void) { return exportGv_; }

    static bool printFactorGraph (void) { return printFg_; }

    static void enableExportToLibDai (void) { exportLd_ = true; }

    static void disableExportToLibDai (void) { exportLd_ = false; }

    static void enableExportToUai (void) { exportUai_ = true; }

    static void disableExportToUai (void) { exportUai_ = false; }

    static void enableExportToGraphViz (void) { exportGv_ = true; }

    static void disableExportToGraphViz (void) { exportGv_ = false; }

    static void enablePrintFactorGraph (void) { printFg_ = true; }

    static void disablePrintFactorGraph (void) { printFg_ = false; }

  private:
    void ignoreLines (std::ifstream&) const;

    bool containsCycle (void) const;

    bool containsCycle (const VarNode*, const FacNode*,
        std::vector<bool>&, std::vector<bool>&) const;

    bool containsCycle (const FacNode*, const VarNode*,
        std::vector<bool>&, std::vector<bool>&) const;

    VarNodes  varNodes_;
    FacNodes  facNodes_;

    BayesBallGraph  structure_;
    bool            bayesFactors_;

    typedef std::unordered_map<unsigned, VarNode*> VarMap;
    VarMap varMap_;

    static bool exportLd_;
    static bool exportUai_;
    static bool exportGv_;
    static bool printFg_;

    DISALLOW_ASSIGN (FactorGraph);
};



inline VarNode*
FactorGraph::getVarNode (VarId vid) const
{
  VarMap::const_iterator it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



struct sortByVarId
{
  bool operator()(VarNode* vn1, VarNode* vn2) {
    return vn1->varId() < vn2->varId();
  }
};


#endif // YAP_PACKAGES_CLPBN_HORUS_FACTORGRAPH_H_

