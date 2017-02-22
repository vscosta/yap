#include <cassert>

#include <vector>
#include <unordered_map>
#include <string>
#include <fstream>
#include <iostream>

#include "LiftedKc.h"
#include "LiftedWCNF.h"
#include "LiftedOperations.h"
#include "Indexer.h"


namespace Horus {

enum class CircuitNodeType {
  orCnt,
  andCnt,
  setOrCnt,
  setAndCnt,
  incExcCnt,
  leafCnt,
  smoothCnt,
  trueCnt,
  compilationFailedCnt
};



class CircuitNode {
  public:
    CircuitNode() { }

    virtual ~CircuitNode() { }

    virtual double weight() const = 0;
};



class OrNode : public CircuitNode {
  public:
    OrNode() : CircuitNode(), leftBranch_(0), rightBranch_(0) { }

   ~OrNode();

    CircuitNode** leftBranch () { return &leftBranch_; }
    CircuitNode** rightBranch() { return &rightBranch_; }

    double weight() const;

  private:
    CircuitNode*  leftBranch_;
    CircuitNode*  rightBranch_;
};



class AndNode : public CircuitNode {
  public:
    AndNode() : CircuitNode(), leftBranch_(0), rightBranch_(0) { }

    AndNode (CircuitNode* leftBranch, CircuitNode* rightBranch)
        : CircuitNode(), leftBranch_(leftBranch),
          rightBranch_(rightBranch) { }

   ~AndNode();

    CircuitNode** leftBranch () { return &leftBranch_;  }
    CircuitNode** rightBranch() { return &rightBranch_; }

    double weight() const;

  private:
    CircuitNode*  leftBranch_;
    CircuitNode*  rightBranch_;
};



class SetOrNode	: public CircuitNode {
  public:
    SetOrNode (unsigned nrGroundings)
        : CircuitNode(), follow_(0), nrGroundings_(nrGroundings) { }

   ~SetOrNode();

    CircuitNode** follow() { return &follow_; }

    static unsigned nrPositives() { return nrPos_; }

    static unsigned nrNegatives() { return nrNeg_; }

    static bool isSet() { return nrPos_ >= 0; }

    double weight() const;

  private:
    CircuitNode*  follow_;
    unsigned      nrGroundings_;
    static int    nrPos_;
    static int    nrNeg_;
};



class SetAndNode : public CircuitNode {
  public:
    SetAndNode (unsigned nrGroundings)
        : CircuitNode(), follow_(0), nrGroundings_(nrGroundings) { }

   ~SetAndNode();

    CircuitNode** follow() { return &follow_; }

    double weight() const;

  private:
    CircuitNode*  follow_;
    unsigned      nrGroundings_;
};



class IncExcNode : public CircuitNode {
  public:
    IncExcNode()
        : CircuitNode(), plus1Branch_(0), plus2Branch_(0), minusBranch_(0) { }

   ~IncExcNode();

    CircuitNode** plus1Branch() { return &plus1Branch_; }
    CircuitNode** plus2Branch() { return &plus2Branch_; }
    CircuitNode** minusBranch() { return &minusBranch_; }

    double weight() const;

  private:
    CircuitNode*  plus1Branch_;
    CircuitNode*  plus2Branch_;
    CircuitNode*  minusBranch_;
};



class LeafNode : public CircuitNode {
  public:
    LeafNode (Clause* clause, const LiftedWCNF& lwcnf)
        : CircuitNode(), clause_(clause), lwcnf_(lwcnf) { }

   ~LeafNode();

    const Clause* clause() const { return clause_; }

    Clause* clause() { return clause_; }

    double weight() const;

  private:
    Clause*            clause_;
    const LiftedWCNF&  lwcnf_;
};



class SmoothNode : public CircuitNode {
  public:
    SmoothNode (const Clauses& clauses, const LiftedWCNF& lwcnf)
        : CircuitNode(), clauses_(clauses), lwcnf_(lwcnf) { }

   ~SmoothNode();

    const Clauses& clauses() const { return clauses_; }

    Clauses clauses() { return clauses_; }

    double weight() const;

  private:
    Clauses            clauses_;
    const LiftedWCNF&  lwcnf_;
};



class TrueNode : public CircuitNode {
  public:
    TrueNode() : CircuitNode() { }

    double weight() const;
};



class CompilationFailedNode : public CircuitNode {
  public:
    CompilationFailedNode() : CircuitNode() { }

    double weight() const;
};



class LiftedCircuit {
  public:
    LiftedCircuit (const LiftedWCNF* lwcnf);

   ~LiftedCircuit();

    bool isCompilationSucceeded() const;

    double getWeightedModelCount() const;

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

    bool shatterCountedLogVarsAux (Clauses& clauses,
        size_t idx1, size_t idx2);

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



OrNode::~OrNode()
{
  delete leftBranch_;
  delete rightBranch_;
}



double
OrNode::weight() const
{
  double lw = leftBranch_->weight();
  double rw = rightBranch_->weight();
  return Globals::logDomain ? Util::logSum (lw, rw) : lw + rw;
}



AndNode::~AndNode()
{
  delete leftBranch_;
  delete rightBranch_;
}



double
AndNode::weight() const
{
  double lw = leftBranch_->weight();
  double rw = rightBranch_->weight();
  return Globals::logDomain ? lw + rw : lw * rw;
}



int SetOrNode::nrPos_ = -1;
int SetOrNode::nrNeg_ = -1;



SetOrNode::~SetOrNode()
{
  delete follow_;
}



double
SetOrNode::weight() const
{
  double weightSum = LogAware::addIdenty();
  for (unsigned i = 0; i < nrGroundings_ + 1; i++) {
    nrPos_ = nrGroundings_ - i;
    nrNeg_ = i;
    if (Globals::logDomain) {
      double nrCombs = Util::nrCombinations (nrGroundings_, i);
      double w = follow_->weight();
      weightSum = Util::logSum (weightSum, std::log (nrCombs) + w);
    } else {
      double w = follow_->weight();
      weightSum += Util::nrCombinations (nrGroundings_, i) * w;
    }
  }
  nrPos_ = -1;
  nrNeg_ = -1;
  return weightSum;
}



SetAndNode::~SetAndNode()
{
  delete follow_;
}



double
SetAndNode::weight() const
{
  return LogAware::pow (follow_->weight(), nrGroundings_);
}



IncExcNode::~IncExcNode()
{
  delete plus1Branch_;
  delete plus2Branch_;
  delete minusBranch_;
}



double
IncExcNode::weight() const
{
  double w = 0.0;
  if (Globals::logDomain) {
    w = Util::logSum (plus1Branch_->weight(), plus2Branch_->weight());
    w = std::log (std::exp (w) - std::exp (minusBranch_->weight()));
  } else {
    w = plus1Branch_->weight() + plus2Branch_->weight();
    w -= minusBranch_->weight();
  }
  return w;
}



LeafNode::~LeafNode()
{
  delete clause_;
}



double
LeafNode::weight() const
{
  assert (clause_->isUnit());
  if (clause_->posCountedLogVars().empty() == false
    || clause_->negCountedLogVars().empty() == false) {
    if (SetOrNode::isSet() == false) {
      // return a NaN if we have a SetOrNode
      // ancester that is not set. This can only
      // happen when calculating the weights
      // for the edge labels in graphviz
      return nan("SetOrNode::isSet() == false");
    }
  }
  double weight = clause_->literals()[0].isPositive()
      ? lwcnf_.posWeight (clause_->literals().front().lid())
      : lwcnf_.negWeight (clause_->literals().front().lid());
  LogVarSet lvs = clause_->constr().logVarSet();
  lvs -= clause_->ipgLogVars();
  lvs -= clause_->posCountedLogVars();
  lvs -= clause_->negCountedLogVars();
  unsigned nrGroundings = 1;
  if (lvs.empty() == false) {
    nrGroundings = clause_->constr().projectedCopy (lvs).size();
  }
  if (clause_->posCountedLogVars().empty() == false) {
    nrGroundings *= std::pow (SetOrNode::nrPositives(),
        clause_->nrPosCountedLogVars());
  }
  if (clause_->negCountedLogVars().empty() == false) {
    nrGroundings *= std::pow (SetOrNode::nrNegatives(),
        clause_->nrNegCountedLogVars());
  }
  return LogAware::pow (weight, nrGroundings);
}



SmoothNode::~SmoothNode()
{
  Clause::deleteClauses (clauses_);
}



double
SmoothNode::weight() const
{
  Clauses cs = clauses();
  double totalWeight = LogAware::multIdenty();
  for (size_t i = 0; i < cs.size(); i++) {
    double posWeight = lwcnf_.posWeight (cs[i]->literals()[0].lid());
    double negWeight = lwcnf_.negWeight (cs[i]->literals()[0].lid());
    LogVarSet lvs = cs[i]->constr().logVarSet();
    lvs -= cs[i]->ipgLogVars();
    lvs -= cs[i]->posCountedLogVars();
    lvs -= cs[i]->negCountedLogVars();
    unsigned nrGroundings = 1;
    if (lvs.empty() == false) {
      nrGroundings = cs[i]->constr().projectedCopy (lvs).size();
    }
    if (cs[i]->posCountedLogVars().empty() == false) {
      nrGroundings *= std::pow (SetOrNode::nrPositives(),
          cs[i]->nrPosCountedLogVars());
    }
    if (cs[i]->negCountedLogVars().empty() == false) {
      nrGroundings *= std::pow (SetOrNode::nrNegatives(),
          cs[i]->nrNegCountedLogVars());
    }
    if (Globals::logDomain) {
      totalWeight += Util::logSum (posWeight, negWeight) * nrGroundings;
    } else {
      totalWeight *= std::pow (posWeight + negWeight, nrGroundings);
    }
  }
  return totalWeight;
}



double
TrueNode::weight() const
{
  return LogAware::multIdenty();
}



double
CompilationFailedNode::weight() const
{
  // weighted model counting in compilation
  // failed nodes should give NaN
  return 0.0 / 0.0;
}



LiftedCircuit::LiftedCircuit (const LiftedWCNF* lwcnf)
    : lwcnf_(lwcnf)
{
  root_ = 0;
  compilationSucceeded_ = true;
  Clauses clauses = Clause::copyClauses (lwcnf->clauses());
  compile (&root_, clauses);
  if (compilationSucceeded_) {
    smoothCircuit (root_);
  }
  if (Globals::verbosity > 1) {
    if (compilationSucceeded_) {
      double wmc = LogAware::exp (getWeightedModelCount());
      std::cout << "Weighted model count = " << wmc;
      std::cout << std::endl << std::endl;
    }
    std::cout << "Exporting circuit to graphviz (circuit.dot)..." ;
    std::cout << std::endl << std::endl;
    exportToGraphViz ("circuit.dot");
  }
}



LiftedCircuit::~LiftedCircuit()
{
  delete root_;
  std::unordered_map<CircuitNode*, Clauses>::iterator it
      = originClausesMap_.begin();
  while (it != originClausesMap_.end()) {
    Clause::deleteClauses (it->second);
    ++ it;
  }
}



bool
LiftedCircuit::isCompilationSucceeded() const
{
  return compilationSucceeded_;
}



double
LiftedCircuit::getWeightedModelCount() const
{
  assert (compilationSucceeded_);
  return root_->weight();
}



void
LiftedCircuit::exportToGraphViz (const char* fileName)
{
  std::ofstream out (fileName);
  if (!out.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    return;
  }
  out << "digraph {" << std::endl;
  out << "ranksep=1" << std::endl;
  exportToGraphViz (root_, out);
  out << "}" << std::endl;
  out.close();
}



void
LiftedCircuit::compile (
    CircuitNode** follow,
    Clauses& clauses)
{
  if (compilationSucceeded_ == false
      && Globals::verbosity <= 1) {
    return;
  }

  if (clauses.empty()) {
    *follow = new TrueNode();
    return;
  }

  if (clauses.size() == 1 && clauses[0]->isUnit()) {
    *follow = new LeafNode (clauses[0], *lwcnf_);
    return;
  }

  if (tryUnitPropagation (follow, clauses)) {
    return;
  }

  if (tryIndependence (follow, clauses)) {
    return;
  }

  if (tryShannonDecomp (follow, clauses)) {
    return;
  }

  if (tryInclusionExclusion (follow, clauses)) {
    return;
  }

  if (tryIndepPartialGrounding (follow, clauses)) {
    return;
  }

  if (tryAtomCounting (follow, clauses)) {
    return;
  }

  *follow = new CompilationFailedNode();
  if (Globals::verbosity > 1) {
    originClausesMap_[*follow] = clauses;
    explanationMap_[*follow]   = "" ;
  }
  compilationSucceeded_ = false;
}



bool
LiftedCircuit::tryUnitPropagation (
    CircuitNode** follow,
    Clauses& clauses)
{
  if (Globals::verbosity > 1) {
    backupClauses_ = Clause::copyClauses (clauses);
  }
  for (size_t i = 0; i < clauses.size(); i++) {
    if (clauses[i]->isUnit()) {
      Clauses propagClauses;
      for (size_t j = 0; j < clauses.size(); j++) {
        if (i != j) {
          LiteralId lid = clauses[i]->literals()[0].lid();
          LogVarTypes types = clauses[i]->logVarTypes (0);
          if (clauses[i]->literals()[0].isPositive()) {
            if (clauses[j]->containsPositiveLiteral (lid, types) == false) {
              clauses[j]->removeNegativeLiterals (lid, types);
              if (clauses[j]->nrLiterals() > 0) {
                propagClauses.push_back (clauses[j]);
              } else {
                delete clauses[j];
              }
            } else {
              delete clauses[j];
            }
          } else if (clauses[i]->literals()[0].isNegative()) {
            if (clauses[j]->containsNegativeLiteral (lid, types) == false) {
              clauses[j]->removePositiveLiterals (lid, types);
              if (clauses[j]->nrLiterals() > 0) {
                propagClauses.push_back (clauses[j]);
              } else {
                delete clauses[j];
              }
            } else {
              delete clauses[j];
            }
          }
        }
      }

      AndNode* andNode = new AndNode();
      if (Globals::verbosity > 1) {
        originClausesMap_[andNode] = backupClauses_;
        std::stringstream explanation;
        explanation << " UP on " << clauses[i]->literals()[0];
        explanationMap_[andNode] = explanation.str();
      }

      Clauses unitClause = { clauses[i] };
      compile (andNode->leftBranch(),  unitClause);
      compile (andNode->rightBranch(), propagClauses);
      (*follow) = andNode;
      return true;
    }
  }
  if (Globals::verbosity > 1) {
    Clause::deleteClauses (backupClauses_);
  }
  return false;
}



bool
LiftedCircuit::tryIndependence (
    CircuitNode** follow,
    Clauses& clauses)
{
  if (clauses.size() == 1) {
    return false;
  }
  if (Globals::verbosity > 1) {
    backupClauses_ = Clause::copyClauses (clauses);
  }
  Clauses depClauses = { clauses[0] };
  Clauses indepClauses (clauses.begin() + 1, clauses.end());
  bool finish = false;
  while (finish == false) {
    finish = true;
    for (size_t i = 0; i < indepClauses.size(); i++) {
      if (independentClause (*indepClauses[i], depClauses) == false) {
        depClauses.push_back (indepClauses[i]);
        indepClauses.erase (indepClauses.begin() + i);
        finish = false;
        break;
      }
    }
  }
  if (indepClauses.empty() == false) {
    AndNode* andNode = new AndNode ();
    if (Globals::verbosity > 1) {
      originClausesMap_[andNode] = backupClauses_;
      explanationMap_[andNode]   = " Independence" ;
    }
    compile (andNode->leftBranch(), depClauses);
    compile (andNode->rightBranch(), indepClauses);
    (*follow) = andNode;
    return true;
  }
  if (Globals::verbosity > 1) {
    Clause::deleteClauses (backupClauses_);
  }
  return false;
}



bool
LiftedCircuit::tryShannonDecomp (
    CircuitNode** follow,
    Clauses& clauses)
{
  if (Globals::verbosity > 1) {
    backupClauses_ = Clause::copyClauses (clauses);
  }
  for (size_t i = 0; i < clauses.size(); i++) {
    const Literals& literals = clauses[i]->literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].isGround (
          clauses[i]->constr(), clauses[i]->ipgLogVars())) {

        Clause* c1 = lwcnf_->createClause (literals[j].lid());
        Clause* c2 = new Clause (*c1);
        c2->literals().front().complement();

        Clauses otherClauses = Clause::copyClauses (clauses);
        clauses.push_back (c1);
        otherClauses.push_back (c2);

        OrNode* orNode = new OrNode();
        if (Globals::verbosity > 1) {
          originClausesMap_[orNode] = backupClauses_;
          std::stringstream explanation;
          explanation << " SD on " << literals[j];
          explanationMap_[orNode] = explanation.str();
        }

        compile (orNode->leftBranch(),  clauses);
        compile (orNode->rightBranch(), otherClauses);
        (*follow) = orNode;
        return true;
      }
    }
  }
  if (Globals::verbosity > 1) {
    Clause::deleteClauses (backupClauses_);
  }
  return false;
}



bool
LiftedCircuit::tryInclusionExclusion (
    CircuitNode** follow,
    Clauses& clauses)
{
  if (Globals::verbosity > 1) {
    backupClauses_ = Clause::copyClauses (clauses);
  }
  for (size_t i = 0; i < clauses.size(); i++) {
    Literals depLits = { clauses[i]->literals().front() };
    Literals indepLits (clauses[i]->literals().begin() + 1,
        clauses[i]->literals().end());
    bool finish = false;
    while (finish == false) {
      finish = true;
      for (size_t j = 0; j < indepLits.size(); j++) {
        if (independentLiteral (indepLits[j], depLits) == false) {
          depLits.push_back (indepLits[j]);
          indepLits.erase (indepLits.begin() + j);
          finish = false;
          break;
        }
      }
    }
    if (indepLits.empty() == false) {
      LogVarSet lvs1;
      for (size_t j = 0; j < depLits.size(); j++) {
        lvs1 |= depLits[j].logVarSet();
      }
      if (clauses[i]->constr().isCountNormalized (lvs1) == false) {
        break;
      }
      LogVarSet lvs2;
      for (size_t j = 0; j < indepLits.size(); j++) {
        lvs2 |= indepLits[j].logVarSet();
      }
      if (clauses[i]->constr().isCountNormalized (lvs2) == false) {
        break;
      }
      Clause* c1 = new Clause (clauses[i]->constr().projectedCopy (lvs1));
      for (size_t j = 0; j < depLits.size(); j++) {
        c1->addLiteral (depLits[j]);
      }
      Clause* c2 = new Clause (clauses[i]->constr().projectedCopy (lvs2));
      for (size_t j = 0; j < indepLits.size(); j++) {
        c2->addLiteral (indepLits[j]);
      }

      clauses.erase (clauses.begin() + i);
      Clauses plus1Clauses = Clause::copyClauses (clauses);
      Clauses plus2Clauses = Clause::copyClauses (clauses);

      plus1Clauses.push_back (c1);
      plus2Clauses.push_back (c2);
      clauses.push_back (c1);
      clauses.push_back (c2);

      IncExcNode* ieNode = new IncExcNode();
      if (Globals::verbosity > 1) {
        originClausesMap_[ieNode] = backupClauses_;
        std::stringstream explanation;
        explanation << " IncExc on clause nº " << i + 1;
        explanationMap_[ieNode] = explanation.str();
      }
      compile (ieNode->plus1Branch(), plus1Clauses);
      compile (ieNode->plus2Branch(), plus2Clauses);
      compile (ieNode->minusBranch(), clauses);
      *follow = ieNode;
      return true;
    }
  }
  if (Globals::verbosity > 1) {
    Clause::deleteClauses (backupClauses_);
  }
  return false;
}



bool
LiftedCircuit::tryIndepPartialGrounding (
    CircuitNode** follow,
    Clauses& clauses)
{
  // assumes that all literals have logical variables
  // else, shannon decomp was possible
  if (Globals::verbosity > 1) {
    backupClauses_ = Clause::copyClauses (clauses);
  }
  LogVars rootLogVars;
  LogVarSet lvs = clauses[0]->ipgCandidates();
  for (size_t i = 0; i < lvs.size(); i++) {
    rootLogVars.clear();
    rootLogVars.push_back (lvs[i]);
    ConstraintTree ct = clauses[0]->constr().projectedCopy ({lvs[i]});
    if (tryIndepPartialGroundingAux (clauses, ct, rootLogVars)) {
      for (size_t j = 0; j < clauses.size(); j++) {
        clauses[j]->addIpgLogVar (rootLogVars[j]);
      }
      SetAndNode* setAndNode = new SetAndNode (ct.size());
      if (Globals::verbosity > 1) {
        originClausesMap_[setAndNode] = backupClauses_;
        explanationMap_[setAndNode]   = " IPG" ;
      }
      *follow = setAndNode;
      compile (setAndNode->follow(), clauses);
      return true;
    }
  }
  if (Globals::verbosity > 1) {
    Clause::deleteClauses (backupClauses_);
  }
  return false;
}



bool
LiftedCircuit::tryIndepPartialGroundingAux (
    Clauses& clauses,
    ConstraintTree& ct,
    LogVars& rootLogVars)
{
  for (size_t i = 1; i < clauses.size(); i++) {
    LogVarSet lvs = clauses[i]->ipgCandidates();
    for (size_t j = 0; j < lvs.size(); j++) {
      ConstraintTree ct2 = clauses[i]->constr().projectedCopy ({lvs[j]});
      if (ct.tupleSet() == ct2.tupleSet()) {
        rootLogVars.push_back (lvs[j]);
        break;
      }
    }
    if (rootLogVars.size() != i + 1) {
      return false;
    }
  }
  // verifies if the IPG logical vars appear in the same positions
  std::unordered_map<LiteralId, size_t> positions;
  for (size_t i = 0; i < clauses.size(); i++) {
    const Literals& literals = clauses[i]->literals();
    for (size_t j = 0; j < literals.size(); j++) {
      size_t idx = literals[j].indexOfLogVar (rootLogVars[i]);
      assert (idx != literals[j].nrLogVars());
      std::unordered_map<LiteralId, size_t>::iterator it;
      it = positions.find (literals[j].lid());
      if (it != positions.end()) {
        if (it->second != idx) {
          return false;
        }
      } else {
        positions[literals[j].lid()] = idx;
      }
    }
  }
  return true;
}



bool
LiftedCircuit::tryAtomCounting (
    CircuitNode** follow,
    Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size(); i++) {
    if (clauses[i]->nrPosCountedLogVars() > 0
        || clauses[i]->nrNegCountedLogVars() > 0) {
      // only allow one atom counting node per branch
      return false;
    }
  }
  if (Globals::verbosity > 1) {
    backupClauses_ = Clause::copyClauses (clauses);
  }
  for (size_t i = 0; i < clauses.size(); i++) {
    Literals literals = clauses[i]->literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].nrLogVars() == 1
          && ! clauses[i]->isIpgLogVar (literals[j].logVars().front())
          && ! clauses[i]->isCountedLogVar (literals[j].logVars().front())) {
        unsigned nrGroundings = clauses[i]->constr().projectedCopy (
            literals[j].logVars()).size();
        SetOrNode* setOrNode = new SetOrNode (nrGroundings);
        if (Globals::verbosity > 1) {
          originClausesMap_[setOrNode] = backupClauses_;
          explanationMap_[setOrNode]   = " AC" ;
        }
        Clause* c1 = new Clause (
            clauses[i]->constr().projectedCopy (literals[j].logVars()));
        Clause* c2 = new Clause (
            clauses[i]->constr().projectedCopy (literals[j].logVars()));
        c1->addLiteral (literals[j]);
        c2->addLiteralComplemented (literals[j]);
        c1->addPosCountedLogVar (literals[j].logVars().front());
        c2->addNegCountedLogVar (literals[j].logVars().front());
        clauses.push_back (c1);
        clauses.push_back (c2);
        shatterCountedLogVars (clauses);
        compile (setOrNode->follow(), clauses);
        *follow = setOrNode;
        return true;
      }
    }
  }
  if (Globals::verbosity > 1) {
    Clause::deleteClauses (backupClauses_);
  }
  return false;
}



void
LiftedCircuit::shatterCountedLogVars (Clauses& clauses)
{
  while (shatterCountedLogVarsAux (clauses)) ;
}



bool
LiftedCircuit::shatterCountedLogVarsAux (Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size() - 1; i++) {
    for (size_t j = i + 1; j < clauses.size(); j++) {
      bool splitedSome = shatterCountedLogVarsAux (clauses, i, j);
      if (splitedSome) {
        return true;
      }
    }
  }
  return false;
}



bool
LiftedCircuit::shatterCountedLogVarsAux (
    Clauses& clauses,
    size_t idx1,
    size_t idx2)
{
  Literals lits1 = clauses[idx1]->literals();
  Literals lits2 = clauses[idx2]->literals();
  for (size_t i = 0; i < lits1.size(); i++) {
    for (size_t j = 0; j < lits2.size(); j++) {
      if (lits1[i].lid() == lits2[j].lid()) {
        LogVars lvs1 = lits1[i].logVars();
        LogVars lvs2 = lits2[j].logVars();
        for (size_t k = 0; k < lvs1.size(); k++) {
          if (clauses[idx1]->isCountedLogVar (lvs1[k])
              && clauses[idx2]->isCountedLogVar (lvs2[k]) == false) {
            clauses.push_back (new Clause (*clauses[idx2]));
            clauses[idx2]->addPosCountedLogVar (lvs2[k]);
            clauses.back()->addNegCountedLogVar (lvs2[k]);
            return true;
          }
          if (clauses[idx2]->isCountedLogVar (lvs2[k])
              && clauses[idx1]->isCountedLogVar (lvs1[k]) == false) {
            clauses.push_back (new Clause (*clauses[idx1]));
            clauses[idx1]->addPosCountedLogVar (lvs1[k]);
            clauses.back()->addNegCountedLogVar (lvs1[k]);
            return true;
          }
        }
      }
    }
  }
  return false;
}



bool
LiftedCircuit::independentClause (
    Clause& clause,
    Clauses& otherClauses) const
{
  for (size_t i = 0; i < otherClauses.size(); i++) {
    if (Clause::independentClauses (clause, *otherClauses[i]) == false) {
      return false;
    }
  }
  return true;
}



bool
LiftedCircuit::independentLiteral (
    const Literal& lit,
    const Literals& otherLits) const
{
  for (size_t i = 0; i < otherLits.size(); i++) {
    if (lit.lid() == otherLits[i].lid()
        || (lit.logVarSet() & otherLits[i].logVarSet()).empty() == false) {
      return false;
    }
  }
  return true;
}



LitLvTypesSet
LiftedCircuit::smoothCircuit (CircuitNode* node)
{
  assert (node);
  LitLvTypesSet propagLits;

  switch (getCircuitNodeType (node)) {

    case CircuitNodeType::orCnt: {
      OrNode* casted = dynamic_cast<OrNode*>(node);
      LitLvTypesSet lids1 = smoothCircuit (*casted->leftBranch());
      LitLvTypesSet lids2 = smoothCircuit (*casted->rightBranch());
      LitLvTypesSet missingLeft  = lids2 - lids1;
      LitLvTypesSet missingRight = lids1 - lids2;
      createSmoothNode (missingLeft,  casted->leftBranch());
      createSmoothNode (missingRight, casted->rightBranch());
      propagLits |= lids1;
      propagLits |= lids2;
      break;
    }

    case CircuitNodeType::andCnt: {
      AndNode* casted = dynamic_cast<AndNode*>(node);
      LitLvTypesSet lids1 = smoothCircuit (*casted->leftBranch());
      LitLvTypesSet lids2 = smoothCircuit (*casted->rightBranch());
      propagLits |= lids1;
      propagLits |= lids2;
      break;
    }

    case CircuitNodeType::setOrCnt: {
      SetOrNode* casted = dynamic_cast<SetOrNode*>(node);
      propagLits = smoothCircuit (*casted->follow());
      TinySet<std::pair<LiteralId,unsigned>> litSet;
      for (size_t i = 0; i < propagLits.size(); i++) {
        litSet.insert (std::make_pair (propagLits[i].lid(),
            propagLits[i].logVarTypes().size()));
      }
      LitLvTypesSet missingLids;
      for (size_t i = 0; i < litSet.size(); i++) {
        std::vector<LogVarTypes> allTypes
            = getAllPossibleTypes (litSet[i].second);
        for (size_t j = 0; j < allTypes.size(); j++) {
          bool typeFound = false;
          for (size_t k = 0; k < propagLits.size(); k++) {
            if (litSet[i].first == propagLits[k].lid()
                && containsTypes (propagLits[k].logVarTypes(), allTypes[j])) {
              typeFound = true;
              break;
            }
          }
          if (typeFound == false) {
            missingLids.insert (LitLvTypes (litSet[i].first, allTypes[j]));
          }
        }
      }
      createSmoothNode (missingLids, casted->follow());
      // setAllFullLogVars() can cause repeated elements in
      // the set. Fix this by reconstructing the set again
      LitLvTypesSet copy = propagLits;
      propagLits.clear();
      for (size_t i = 0; i < copy.size(); i++) {
        copy[i].setAllFullLogVars();
        propagLits.insert (copy[i]);
      }
      break;
    }

    case CircuitNodeType::setAndCnt: {
      SetAndNode* casted = dynamic_cast<SetAndNode*>(node);
      propagLits = smoothCircuit (*casted->follow());
      break;
    }

    case CircuitNodeType::incExcCnt: {
      IncExcNode* casted = dynamic_cast<IncExcNode*>(node);
      LitLvTypesSet lids1 = smoothCircuit (*casted->plus1Branch());
      LitLvTypesSet lids2 = smoothCircuit (*casted->plus2Branch());
      LitLvTypesSet missingPlus1 = lids2 - lids1;
      LitLvTypesSet missingPlus2 = lids1 - lids2;
      createSmoothNode (missingPlus1, casted->plus1Branch());
      createSmoothNode (missingPlus2, casted->plus2Branch());
      propagLits |= lids1;
      propagLits |= lids2;
      break;
    }

    case CircuitNodeType::leafCnt: {
      LeafNode* casted = dynamic_cast<LeafNode*>(node);
      propagLits.insert (LitLvTypes (
          casted->clause()->literals()[0].lid(),
          casted->clause()->logVarTypes(0)));
    }

    default:
      break;
  }

  return propagLits;
}



void
LiftedCircuit::createSmoothNode (
    const LitLvTypesSet& missingLits,
    CircuitNode** prev)
{
  if (missingLits.empty() == false) {
    if (Globals::verbosity > 1) {
      std::unordered_map<CircuitNode*, Clauses>::iterator it
          = originClausesMap_.find (*prev);
      if (it != originClausesMap_.end()) {
        backupClauses_ = it->second;
      } else {
        backupClauses_ =  Clause::copyClauses (
            {((dynamic_cast<LeafNode*>(*prev))->clause())});
      }
    }
    Clauses clauses;
    for (size_t i = 0; i < missingLits.size(); i++) {
      LiteralId lid = missingLits[i].lid();
      const LogVarTypes& types = missingLits[i].logVarTypes();
      Clause* c = lwcnf_->createClause (lid);
      for (size_t j = 0; j < types.size(); j++) {
        LogVar X = c->literals().front().logVars()[j];
        if (types[j] == LogVarType::posLvt) {
          c->addPosCountedLogVar (X);
        } else if (types[j] == LogVarType::negLvt) {
          c->addNegCountedLogVar (X);
        }
      }
      c->addLiteralComplemented (c->literals()[0]);
      clauses.push_back (c);
    }
    SmoothNode* smoothNode = new SmoothNode (clauses, *lwcnf_);
    *prev = new AndNode (smoothNode, *prev);
    if (Globals::verbosity > 1) {
      originClausesMap_[*prev] = backupClauses_;
      explanationMap_[*prev]   = " Smoothing" ;
    }
  }
}



std::vector<LogVarTypes>
LiftedCircuit::getAllPossibleTypes (unsigned nrLogVars) const
{
  std::vector<LogVarTypes> res;
  if (nrLogVars == 0) {
    // do nothing
  } else if (nrLogVars == 1) {
    res.push_back ({ LogVarType::posLvt });
    res.push_back ({ LogVarType::negLvt });
  } else {
    Ranges ranges (nrLogVars, 2);
    Indexer indexer (ranges);
    while (indexer.valid()) {
      LogVarTypes types;
      for (size_t i = 0; i < nrLogVars; i++) {
        if (indexer[i] == 0) {
          types.push_back (LogVarType::posLvt);
        } else {
          types.push_back (LogVarType::negLvt);
        }
      }
      res.push_back (types);
      ++ indexer;
    }
  }
  return res;
}



bool
LiftedCircuit::containsTypes (
    const LogVarTypes& typesA,
    const LogVarTypes& typesB) const
{
  for (size_t i = 0; i < typesA.size(); i++) {
    if (typesA[i] == LogVarType::fullLvt) {

    } else if (typesA[i] == LogVarType::posLvt
        && typesB[i] == LogVarType::posLvt) {

    } else if (typesA[i] == LogVarType::negLvt
        && typesB[i] == LogVarType::negLvt) {

    } else {
      return false;
    }
  }
  return true;
}



CircuitNodeType
LiftedCircuit::getCircuitNodeType (const CircuitNode* node) const
{
  CircuitNodeType type = CircuitNodeType::orCnt;
  if (dynamic_cast<const OrNode*>(node)) {
    type = CircuitNodeType::orCnt;
  } else if (dynamic_cast<const AndNode*>(node)) {
    type = CircuitNodeType::andCnt;
  } else if (dynamic_cast<const SetOrNode*>(node)) {
    type = CircuitNodeType::setOrCnt;
  } else if (dynamic_cast<const SetAndNode*>(node)) {
    type = CircuitNodeType::setAndCnt;
  } else if (dynamic_cast<const IncExcNode*>(node)) {
    type = CircuitNodeType::incExcCnt;
  } else if (dynamic_cast<const LeafNode*>(node)) {
    type = CircuitNodeType::leafCnt;
  } else if (dynamic_cast<const SmoothNode*>(node)) {
    type = CircuitNodeType::smoothCnt;
  } else if (dynamic_cast<const TrueNode*>(node)) {
    type = CircuitNodeType::trueCnt;
  } else if (dynamic_cast<const CompilationFailedNode*>(node)) {
    type = CircuitNodeType::compilationFailedCnt;
  } else {
    assert (false);
  }
  return type;
}



void
LiftedCircuit::exportToGraphViz (CircuitNode* node, std::ofstream& os)
{
  assert (node);

  static unsigned nrAuxNodes = 0;
  std::stringstream ss;
  ss << "n" << nrAuxNodes;
  std::string auxNode = ss.str();
  nrAuxNodes ++;
  std::string opStyle = "shape=circle,width=0.7,margin=\"0.0,0.0\"," ;

  switch (getCircuitNodeType (node)) {

    case CircuitNodeType::orCnt: {
      OrNode* casted = dynamic_cast<OrNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [" << opStyle << "label=\"∨\"]" ;
      os << std::endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << getExplanationString (node) << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->leftBranch());
      os << " [label=\" " << (*casted->leftBranch())->weight() << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->rightBranch());
      os << " [label=\" " << (*casted->rightBranch())->weight() << "\"]" ;
      os << std::endl;

      exportToGraphViz (*casted->leftBranch(),  os);
      exportToGraphViz (*casted->rightBranch(), os);
      break;
    }

    case CircuitNodeType::andCnt: {
      AndNode* casted = dynamic_cast<AndNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [" << opStyle << "label=\"∧\"]" ;
      os << std::endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << getExplanationString (node) << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->leftBranch());
      os << " [label=\" " << (*casted->leftBranch())->weight() << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->rightBranch());
      os << " [label=\" " << (*casted->rightBranch())->weight() << "\"]" ;
      os << std::endl;

      exportToGraphViz (*casted->leftBranch(),  os);
      exportToGraphViz (*casted->rightBranch(), os);
      break;
    }

    case CircuitNodeType::setOrCnt: {
      SetOrNode* casted = dynamic_cast<SetOrNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [" << opStyle << "label=\"∨(X)\"]" ;
      os << std::endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << getExplanationString (node) << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->follow());
      os << " [label=\" " << (*casted->follow())->weight() << "\"]" ;
      os << std::endl;

      exportToGraphViz (*casted->follow(),  os);
      break;
    }

    case CircuitNodeType::setAndCnt: {
      SetAndNode* casted = dynamic_cast<SetAndNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [" << opStyle << "label=\"∧(X)\"]" ;
      os << std::endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << getExplanationString (node) << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->follow());
      os << " [label=\" " << (*casted->follow())->weight() << "\"]" ;
      os << std::endl;

      exportToGraphViz (*casted->follow(),  os);
      break;
    }

    case CircuitNodeType::incExcCnt: {
      IncExcNode* casted = dynamic_cast<IncExcNode*>(node);
      printClauses (casted, os);

      os << auxNode << " [" << opStyle << "label=\"+ - +\"]" ;
      os << std::endl;
      os << escapeNode (node) << " -> " << auxNode;
      os << " [label=\"" << getExplanationString (node) << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->plus1Branch());
      os << " [label=\" " << (*casted->plus1Branch())->weight() << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->minusBranch()) << std::endl;
      os << " [label=\" " << (*casted->minusBranch())->weight() << "\"]" ;
      os << std::endl;

      os << auxNode << " -> " ;
      os << escapeNode (*casted->plus2Branch());
      os << " [label=\" " << (*casted->plus2Branch())->weight() << "\"]" ;
      os << std::endl;

      exportToGraphViz (*casted->plus1Branch(), os);
      exportToGraphViz (*casted->plus2Branch(), os);
      exportToGraphViz (*casted->minusBranch(), os);
      break;
    }

    case CircuitNodeType::leafCnt: {
      printClauses (node, os, "style=filled,fillcolor=palegreen,");
      break;
    }

    case CircuitNodeType::smoothCnt: {
      printClauses (node, os, "style=filled,fillcolor=lightblue,");
      break;
    }

    case CircuitNodeType::trueCnt: {
      os << escapeNode (node);
      os << " [shape=box,label=\"⊤\"]" ;
      os << std::endl;
      break;
    }

    case CircuitNodeType::compilationFailedCnt: {
      printClauses (node, os, "style=filled,fillcolor=salmon,");
      break;
    }

    default:
      assert (false);
  }
}



std::string
LiftedCircuit::escapeNode (const CircuitNode* node) const
{
  std::stringstream ss;
  ss << "\"" << node << "\"" ;
  return ss.str();
}



std::string
LiftedCircuit::getExplanationString (CircuitNode* node)
{
  return Util::contains (explanationMap_, node)
      ? explanationMap_[node]
      : "" ;
}



void
LiftedCircuit::printClauses (
    CircuitNode* node,
    std::ofstream& os,
    std::string extraOptions)
{
  Clauses clauses;
  if (Util::contains (originClausesMap_, node)) {
    clauses = originClausesMap_[node];
  } else if (getCircuitNodeType (node) == CircuitNodeType::leafCnt) {
    clauses = { (dynamic_cast<LeafNode*>(node))->clause() } ;
  } else if (getCircuitNodeType (node) == CircuitNodeType::smoothCnt) {
    clauses = (dynamic_cast<SmoothNode*>(node))->clauses();
  }
  assert (clauses.empty() == false);
  os << escapeNode (node);
  os << " [shape=box," << extraOptions << "label=\"" ;
  for (size_t i = 0; i < clauses.size(); i++) {
    if (i != 0) os << "\\n" ;
    os << *clauses[i];
  }
  os << "\"]" ;
  os << std::endl;
}



Params
LiftedKc::solveQuery (const Grounds& query)
{
  ParfactorList pfList (parfactorList);
  LiftedOperations::shatterAgainstQuery (pfList, query);
  LiftedOperations::runWeakBayesBall (pfList, query);
  LiftedWCNF lwcnf (pfList);
  LiftedCircuit circuit (&lwcnf);
  if (circuit.isCompilationSucceeded() == false) {
    std::cerr << "Error: the circuit compilation has failed." ;
    std::cerr << std::endl;
    exit (EXIT_FAILURE);
  }
  std::vector<PrvGroup> groups;
  Ranges ranges;
  for (size_t i = 0; i < query.size(); i++) {
    ParfactorList::const_iterator it = pfList.begin();
    while (it != pfList.end()) {
      size_t idx = (*it)->indexOfGround (query[i]);
      if (idx != (*it)->nrArguments()) {
        groups.push_back ((*it)->argument (idx).group());
        ranges.push_back ((*it)->range (idx));
        break;
      }
      ++ it;
    }
  }
  assert (groups.size() == query.size());
  Params params;
  Indexer indexer (ranges);
  while (indexer.valid()) {
    for (size_t i = 0; i < groups.size(); i++) {
      std::vector<LiteralId> litIds = lwcnf.prvGroupLiterals (groups[i]);
      for (size_t j = 0; j < litIds.size(); j++) {
        if (indexer[i] == j) {
          lwcnf.addWeight (litIds[j], LogAware::one(),
              LogAware::one());
        } else {
          lwcnf.addWeight (litIds[j], LogAware::zero(),
              LogAware::one());
        }
      }
    }
    params.push_back (circuit.getWeightedModelCount());
    ++ indexer;
  }
  LogAware::normalize (params);
  if (Globals::logDomain) {
    Util::exp (params);
  }
  return params;
}



void
LiftedKc::printSolverFlags() const
{
  std::stringstream ss;
  ss << "lifted kc [" ;
  ss << "log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  std::cout << ss.str() << std::endl;
}

}  // namespace Horus
