#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_

#include <vector>
#include <unordered_map>
#include <string>
#include <fstream>

#include "LiftedSolver.h"
#include "LiftedWCNF.h"
#include "ParfactorList.h"


namespace horus {

enum CircuitNodeType {
  OR_NODE,
  AND_NODE,
  SET_OR_NODE,
  SET_AND_NODE,
  INC_EXC_NODE,
  LEAF_NODE,
  SMOOTH_NODE,
  TRUE_NODE,
  COMPILATION_FAILED_NODE
};



class CircuitNode
{
  public:
    CircuitNode (void) { }

    virtual ~CircuitNode (void) { }

    virtual double weight (void) const = 0;
};



class OrNode : public CircuitNode
{
  public:
    OrNode (void) : CircuitNode(), leftBranch_(0), rightBranch_(0) { }

   ~OrNode (void);

    CircuitNode** leftBranch  (void) { return &leftBranch_; }
    CircuitNode** rightBranch (void) { return &rightBranch_; }

    double weight (void) const;

  private:
    CircuitNode*  leftBranch_;
    CircuitNode*  rightBranch_;
};



class AndNode : public CircuitNode
{
  public:
    AndNode (void) : CircuitNode(), leftBranch_(0), rightBranch_(0) { }

    AndNode (CircuitNode* leftBranch, CircuitNode* rightBranch)
        : CircuitNode(), leftBranch_(leftBranch), rightBranch_(rightBranch) { }

   ~AndNode (void);

    CircuitNode** leftBranch  (void) { return &leftBranch_;  }
    CircuitNode** rightBranch (void) { return &rightBranch_; }

    double weight (void) const;

  private:
    CircuitNode*  leftBranch_;
    CircuitNode*  rightBranch_;
};



class SetOrNode	: public CircuitNode
{
  public:
    SetOrNode (unsigned nrGroundings)
        : CircuitNode(), follow_(0), nrGroundings_(nrGroundings) { }

   ~SetOrNode (void);

    CircuitNode** follow (void) { return &follow_; }

    static unsigned nrPositives (void) { return nrPos_; }

    static unsigned nrNegatives (void) { return nrNeg_; }

    static bool isSet (void) { return nrPos_ >= 0; }

    double weight (void) const;

  private:
    CircuitNode*  follow_;
    unsigned      nrGroundings_;
    static int    nrPos_;
    static int    nrNeg_;
};



class SetAndNode : public CircuitNode
{
  public:
    SetAndNode (unsigned nrGroundings)
        : CircuitNode(), follow_(0), nrGroundings_(nrGroundings) { }

   ~SetAndNode (void);

    CircuitNode** follow (void) { return &follow_; }

    double weight (void) const;

  private:
    CircuitNode*  follow_;
    unsigned      nrGroundings_;
};



class IncExcNode : public CircuitNode
{
  public:
    IncExcNode (void)
        : CircuitNode(), plus1Branch_(0), plus2Branch_(0), minusBranch_(0) { }

   ~IncExcNode (void);

    CircuitNode** plus1Branch (void) { return &plus1Branch_; }
    CircuitNode** plus2Branch (void) { return &plus2Branch_; }
    CircuitNode** minusBranch (void) { return &minusBranch_; }

    double weight (void) const;

  private:
    CircuitNode*  plus1Branch_;
    CircuitNode*  plus2Branch_;
    CircuitNode*  minusBranch_;
};



class LeafNode : public CircuitNode
{
  public:
    LeafNode (Clause* clause, const LiftedWCNF& lwcnf)
        : CircuitNode(), clause_(clause), lwcnf_(lwcnf) { }

   ~LeafNode (void);

    const Clause* clause (void) const { return clause_; }

    Clause* clause (void) { return clause_; }

    double weight (void) const;

  private:
    Clause*            clause_;
    const LiftedWCNF&  lwcnf_;
};



class SmoothNode : public CircuitNode
{
  public:
    SmoothNode (const Clauses& clauses, const LiftedWCNF& lwcnf)
        : CircuitNode(), clauses_(clauses), lwcnf_(lwcnf) { }

   ~SmoothNode (void);

    const Clauses& clauses (void) const { return clauses_; }

    Clauses clauses (void) { return clauses_; }

    double weight (void) const;

  private:
    Clauses            clauses_;
    const LiftedWCNF&  lwcnf_;
};



class TrueNode : public CircuitNode
{
  public:
    TrueNode (void) : CircuitNode() { }

    double weight (void) const;
};



class CompilationFailedNode : public CircuitNode
{
  public:
    CompilationFailedNode (void) : CircuitNode() { }

    double weight (void) const;
};



class LiftedCircuit
{
  public:
    LiftedCircuit (const LiftedWCNF* lwcnf);

   ~LiftedCircuit (void);

    bool isCompilationSucceeded (void) const;

    double getWeightedModelCount (void) const;

    void exportToGraphViz (const char*);

  private:
    void compile (CircuitNode** follow, Clauses& clauses);

    bool tryUnitPropagation (CircuitNode** follow, Clauses& clauses);

    bool tryIndependence (CircuitNode** follow, Clauses& clauses);

    bool tryShannonDecomp (CircuitNode** follow, Clauses& clauses);

    bool tryInclusionExclusion (CircuitNode** follow, Clauses& clauses);

    bool tryIndepPartialGrounding (CircuitNode** follow, Clauses& clauses);

    bool tryIndepPartialGroundingAux (Clauses& clauses, ConstraintTree& ct,
        LogVars& rootLogVars);

    bool tryAtomCounting (CircuitNode** follow, Clauses& clauses);

    void shatterCountedLogVars (Clauses& clauses);

    bool shatterCountedLogVarsAux (Clauses& clauses);

    bool shatterCountedLogVarsAux (Clauses& clauses, size_t idx1, size_t idx2);

    bool independentClause (Clause& clause, Clauses& otherClauses) const;

    bool independentLiteral (const Literal& lit,
        const Literals& otherLits) const;

    LitLvTypesSet smoothCircuit (CircuitNode* node);

    void createSmoothNode (const LitLvTypesSet& lids,
        CircuitNode** prev);

    std::vector<LogVarTypes> getAllPossibleTypes (unsigned nrLogVars) const;

    bool containsTypes (const LogVarTypes& typesA,
        const LogVarTypes& typesB) const;

    CircuitNodeType getCircuitNodeType (const CircuitNode* node) const;

    void exportToGraphViz (CircuitNode* node, std::ofstream&);

    void printClauses (CircuitNode* node, std::ofstream&,
        std::string extraOptions = "");

    std::string escapeNode (const CircuitNode* node) const;

    std::string getExplanationString (CircuitNode* node);

    CircuitNode*       root_;
    const LiftedWCNF*  lwcnf_;
    bool compilationSucceeded_;
    Clauses backupClauses_;
    std::unordered_map<CircuitNode*, Clauses>      originClausesMap_;
    std::unordered_map<CircuitNode*, std::string>  explanationMap_;

    DISALLOW_COPY_AND_ASSIGN (LiftedCircuit);
};



class LiftedKc : public LiftedSolver
{
  public:
   LiftedKc (const ParfactorList& pfList)
       : LiftedSolver(pfList) { }

  ~LiftedKc (void);

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    LiftedWCNF*     lwcnf_;
    LiftedCircuit*  circuit_;
    ParfactorList   pfList_;

    DISALLOW_COPY_AND_ASSIGN (LiftedKc);
};

}  // namespace horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_

