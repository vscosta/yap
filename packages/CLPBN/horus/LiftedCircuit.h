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
    CircuitNode (const Clauses& clauses, string explanation = "")
        : clauses_(clauses), explanation_(explanation) { }
    
    const Clauses& clauses (void) const { return clauses_; }
    
    Clauses clauses (void) { return clauses_; }
    
    virtual double weight (void) const = 0;
    
    string explanation (void) const { return explanation_; }
        
  private:    
    Clauses  clauses_;
    string   explanation_;
};



class OrNode : public CircuitNode
{
  public:
    OrNode (const Clauses& clauses, string explanation = "")
        : CircuitNode (clauses, explanation),
          leftBranch_(0), rightBranch_(0) { }

    CircuitNode** leftBranch  (void) { return &leftBranch_; }
    CircuitNode** rightBranch (void) { return &rightBranch_; }
    
    double weight (void) const;
  private:
    CircuitNode* leftBranch_;
    CircuitNode* rightBranch_;
};



class AndNode : public CircuitNode
{
  public:
    AndNode (const Clauses& clauses, string explanation = "")
        : CircuitNode (clauses, explanation),
          leftBranch_(0), rightBranch_(0) { }
          
    AndNode (
        const Clauses& clauses,
        CircuitNode* leftBranch,
        CircuitNode* rightBranch,
        string explanation = "")
        : CircuitNode (clauses, explanation),
          leftBranch_(leftBranch), rightBranch_(rightBranch) { }
          
   AndNode (
        CircuitNode* leftBranch,
        CircuitNode* rightBranch,
        string explanation = "")
        : CircuitNode ({}, explanation),
          leftBranch_(leftBranch), rightBranch_(rightBranch) { }

    CircuitNode** leftBranch  (void) { return &leftBranch_;  }
    CircuitNode** rightBranch (void) { return &rightBranch_; }
    
    double weight (void) const;
    
  private:
    CircuitNode* leftBranch_;
    CircuitNode* rightBranch_;
};



class SetOrNode	: public CircuitNode
{
  public:
    SetOrNode (unsigned nrGroundings, const Clauses& clauses)
        : CircuitNode (clauses, " AC"), follow_(0),
          nrGroundings_(nrGroundings) { }

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
    SetAndNode (unsigned nrGroundings, const Clauses& clauses)
        : CircuitNode (clauses, " IPG"), follow_(0),
          nrGroundings_(nrGroundings) { }

    CircuitNode** follow (void) { return &follow_; }
        
    double weight (void) const;

  private:
    CircuitNode*  follow_;
    unsigned      nrGroundings_;
};



class IncExcNode : public CircuitNode
{
  public:
    IncExcNode (const Clauses& clauses)
        : CircuitNode (clauses), plus1Branch_(0),
        plus2Branch_(0), minusBranch_(0) { }

    CircuitNode** plus1Branch (void) { return &plus1Branch_; }
    CircuitNode** plus2Branch (void) { return &plus2Branch_; }
    CircuitNode** minusBranch (void) { return &minusBranch_; }
    
    double weight (void) const;    

  private:
    CircuitNode* plus1Branch_;
    CircuitNode* plus2Branch_;
    CircuitNode* minusBranch_;
};



class LeafNode : public CircuitNode
{
  public:
    LeafNode (const Clause& clause, const LiftedWCNF& lwcnf)
        : CircuitNode (Clauses() = {clause}), lwcnf_(lwcnf) { }

    double weight (void) const;
    
  private:
    const LiftedWCNF&  lwcnf_;
};



class SmoothNode : public CircuitNode
{
  public:
    SmoothNode (const Clauses& clauses, const LiftedWCNF& lwcnf)
        : CircuitNode (clauses), lwcnf_(lwcnf) { }

    double weight (void) const;

  private:
    const LiftedWCNF&  lwcnf_;
};



class TrueNode : public CircuitNode
{
  public:
    TrueNode (void) : CircuitNode ({}) { }
    
    double weight (void) const;
};



class CompilationFailedNode : public CircuitNode
{
  public:
    CompilationFailedNode (const Clauses& clauses) : CircuitNode (clauses) { }
    
    double weight (void) const;
};



class LiftedCircuit
{
  public:
    LiftedCircuit (const LiftedWCNF* lwcnf);    
    
    void smoothCircuit (void);
    
    double getWeightedModelCount (void) const;
    
    void exportToGraphViz (const char*);
    
  private:

    void compile (CircuitNode** follow, Clauses& clauses);

    bool tryUnitPropagation (CircuitNode** follow, Clauses& clauses);
    
    bool tryIndependence    (CircuitNode** follow, Clauses& clauses);
    
    bool tryShannonDecomp   (CircuitNode** follow, Clauses& clauses);
    
    bool tryInclusionExclusion (CircuitNode** follow, Clauses& clauses);
    
    bool tryIndepPartialGrounding (CircuitNode** follow, Clauses& clauses);
    
    bool tryIndepPartialGroundingAux (Clauses& clauses, ConstraintTree& ct,
        vector<unsigned>& indices);
        
    bool tryAtomCounting (CircuitNode** follow, Clauses& clauses);
    
    bool tryGrounding (CircuitNode** follow, Clauses& clauses);
    
    void shatterCountedLogVars (Clauses& clauses);
    
    bool shatterCountedLogVarsAux (Clauses& clauses);

    bool shatterCountedLogVarsAux (Clauses& clauses, size_t idx1, size_t idx2);

    LitLvTypesSet smoothCircuit (CircuitNode* node);
    
    void createSmoothNode (const LitLvTypesSet& lids,
        CircuitNode** prev);
    
    CircuitNodeType getCircuitNodeType (const CircuitNode* node) const;
         
    void exportToGraphViz (CircuitNode* node, ofstream&);
    
    void printClauses (const CircuitNode* node, ofstream&,
        string extraOptions = "");
        
    string escapeNode (const CircuitNode* node) const;

    CircuitNode*       root_;
    const LiftedWCNF*  lwcnf_;
};

#endif // HORUS_LIFTEDCIRCUIT_H

