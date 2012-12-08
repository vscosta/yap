#ifndef HORUS_LIFTEDCIRCUIT_H
#define HORUS_LIFTEDCIRCUIT_H

#include <stack>

#include "LiftedWCNF.h"


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

    virtual double weight (void) const = 0;
};



class OrNode : public CircuitNode
{
  public:
    OrNode (void) : CircuitNode(), leftBranch_(0), rightBranch_(0) { }          

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

    CircuitNode** follow (void) { return &follow_; }

    static unsigned nrPositives (void) { return nrGrsStack.top().first;  }

    static unsigned nrNegatives (void) { return nrGrsStack.top().second; }

    double weight (void) const;

  private:
    CircuitNode*  follow_;
    unsigned      nrGroundings_;

    static stack<pair<unsigned, unsigned>> nrGrsStack;
};



class SetAndNode : public CircuitNode
{
  public:
    SetAndNode (unsigned nrGroundings)
        : CircuitNode(), follow_(0), nrGroundings_(nrGroundings) { }

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

    vector<LogVarTypes> getAllPossibleTypes (unsigned nrLogVars) const;

    bool containsTypes (const LogVarTypes& typesA,
        const LogVarTypes& typesB) const;

    CircuitNodeType getCircuitNodeType (const CircuitNode* node) const;

    void exportToGraphViz (CircuitNode* node, ofstream&);

    void printClauses (CircuitNode* node, ofstream&,
        string extraOptions = "");

    string escapeNode (const CircuitNode* node) const;
    
    string getExplanationString (CircuitNode* node);

    CircuitNode*       root_;
    const LiftedWCNF*  lwcnf_;
    
    Clauses backupClauses_;
    unordered_map<CircuitNode*, Clauses> originClausesMap_;
    unordered_map<CircuitNode*, string>  explanationMap_;
    bool               compilationSucceeded_;
};

#endif // HORUS_LIFTEDCIRCUIT_H

