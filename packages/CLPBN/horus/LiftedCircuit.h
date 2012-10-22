#ifndef HORUS_LIFTEDCIRCUIT_H
#define HORUS_LIFTEDCIRCUIT_H

#include "LiftedWCNF.h"


class CircuitNode
{
  public:
    CircuitNode (const Clauses& clauses, string explanation = "")
        : clauses_(clauses), explanation_(explanation) { }
    
    const Clauses& clauses (void) { return clauses_; }
    
    virtual double weight (void) const { return 0; }
    
    string explanation (void) const { return explanation_; }
        
  private:    
    Clauses  clauses_;
    string   explanation_;
};



class AndNode : public CircuitNode
{
  public:
    AndNode (const Clauses& clauses, string explanation = "")
        : CircuitNode (clauses, explanation),
          leftFollow_(0), rightFollow_(0) { }

    CircuitNode** leftFollow  (void) { return &leftFollow_; }
    CircuitNode** rightFollow (void) { return &rightFollow_; }
  private:
    CircuitNode* leftFollow_;
    CircuitNode* rightFollow_;
};



class OrNode : public CircuitNode
{
  public:
    OrNode (const Clauses& clauses, string explanation = "")
        : CircuitNode (clauses, explanation),
          leftFollow_(0), rightFollow_(0) { }

    CircuitNode** leftFollow  (void) { return &leftFollow_; }
    CircuitNode** rightFollow (void) { return &rightFollow_; }
  private:
    CircuitNode* leftFollow_;
    CircuitNode* rightFollow_;
};



class SetAndNode : public CircuitNode
{
  public:
  private:
    CircuitNode* follow_;
};



class SetOrNode	: public CircuitNode
{
  public:
  private:
    CircuitNode* follow_;
};



class IncExclNode : public CircuitNode
{
  public:
  private:
    CircuitNode* xFollow_;
    CircuitNode* yFollow_;
    CircuitNode* zFollow_;
};



class LeafNode : public CircuitNode
{
  public:
    LeafNode (const Clause& clause) : CircuitNode ({clause}) { }
  private:
};



class TrueNode : public CircuitNode
{
  public:
    TrueNode () : CircuitNode ({}) { }
  private:
};




class FailNode : public CircuitNode
{
  public:
    FailNode (const Clauses& clauses) : CircuitNode (clauses) { }
  private:
};



class LiftedCircuit
{
  public:
    LiftedCircuit (const LiftedWCNF* lwcnf);
    
    void printToDot (void);

  private:

    void compile (CircuitNode** follow, const Clauses& clauses);

    bool tryUnitPropagation (CircuitNode** follow, const Clauses& clauses);
    bool tryIndependence (CircuitNode** follow, const Clauses& clauses);
    bool tryShannonDecomposition (CircuitNode** follow, const Clauses& clauses);

    string escapeNode (const CircuitNode*) const;
    void printToDot (CircuitNode* node, ofstream&);

    CircuitNode*       root_;
    const LiftedWCNF*  lwcnf_;
};

#endif // HORUS_LIFTEDCIRCUIT_H
