#include <queue>

#include <iostream>
#include <ostream>
#include <fstream>

#include "ConstraintTree.h"
#include "Util.h"


namespace Horus {

class CTNode {
  public:
    CTNode (const CTNode& n, const CTChilds& chs = CTChilds()) 
        : symbol_(n.symbol()), childs_(chs), level_(n.level()) { }

    CTNode (Symbol s, unsigned l, const CTChilds& chs = CTChilds())
        : symbol_(s), childs_(chs), level_(l) { }

    unsigned level() const { return level_; }

    void setLevel (unsigned level) { level_ = level; }

    Symbol symbol() const { return symbol_; }

    void setSymbol (Symbol s) { symbol_ = s; }

    CTChilds& childs() { return childs_; }

    const CTChilds& childs() const { return childs_; }

    size_t nrChilds() const { return childs_.size(); }

    bool isRoot() const { return level_ == 0; }

    bool isLeaf() const { return childs_.empty(); }

    CTChilds::iterator findSymbol (Symbol symb);

    void mergeSubtree (CTNode*, bool = true);

    void removeChild (CTNode*);

    void removeChilds();

    void removeAndDeleteChild (CTNode*);

    void removeAndDeleteAllChilds();

    SymbolSet childSymbols() const;

    static CTNode* copySubtree (const CTNode*);

    static void deleteSubtree (CTNode*);

  private:
    void updateChildLevels (CTNode*, unsigned);

    Symbol     symbol_;
    CTChilds   childs_;
    unsigned   level_;

    DISALLOW_ASSIGN (CTNode);
};



inline CTChilds::iterator
CTNode::findSymbol (Symbol symb)
{
  CTNode tmp (symb, 0);
  return childs_.find (&tmp);
}



inline bool
CmpSymbol::operator() (const CTNode* n1, const CTNode* n2) const
{
  return n1->symbol() < n2->symbol();
}



void
CTNode::mergeSubtree (CTNode* n, bool updateLevels)
{
  if (updateLevels) {
    updateChildLevels (n, level_ + 1);
  }
  CTChilds::iterator chIt = childs_.find (n);
  if (chIt != childs_.end()) {
    assert ((*chIt)->symbol() == n->symbol());
    const CTChilds& childsToAdd = n->childs();
    for (CTChilds::const_iterator it = childsToAdd.begin();
         it != childsToAdd.end(); ++ it) {
      (*chIt)->mergeSubtree (*it, false);
    }
    delete n;
  } else {
    childs_.insert (n);
  }
}



void
CTNode::removeChild (CTNode* child)
{
  assert (childs_.contains (child));
  childs_.remove (child);
}



void
CTNode::removeChilds()
{
  childs_.clear();
}



void
CTNode::removeAndDeleteChild (CTNode* child)
{
  removeChild (child);
  CTNode::deleteSubtree (child);
}



void
CTNode::removeAndDeleteAllChilds()
{
  for (CTChilds::const_iterator chIt = childs_.begin();
       chIt != childs_.end(); ++ chIt) {
    deleteSubtree (*chIt);
  }
  childs_.clear();
}



SymbolSet
CTNode::childSymbols() const
{
  SymbolSet symbols;
  for (CTChilds::const_iterator chIt = childs_.begin();
       chIt != childs_.end(); ++ chIt) {
    symbols.insert ((*chIt)->symbol());
  }
  return symbols;
}



void
CTNode::updateChildLevels (CTNode* n, unsigned level)
{
  CTNodes stack;
  stack.push_back (n);
  n->setLevel (level);
  while (stack.empty() == false) {
    CTNode* node = stack.back();
    stack.pop_back();
    for (CTChilds::const_iterator chIt = node->childs().begin();
         chIt != node->childs().end(); ++ chIt) {
      (*chIt)->setLevel (node->level() + 1);
    }
    stack.insert (stack.end(), node->childs().begin(),
        node->childs().end());
  }
}



CTNode*
CTNode::copySubtree (const CTNode* root1)
{
  if (root1->childs().empty()) {
    return new CTNode (*root1);
  }
  CTNode* root2 = new CTNode (*root1);
  typedef std::pair<const CTNode*, CTNode*> StackPair;
  std::vector<StackPair> stack = { StackPair (root1, root2) };
  while (stack.empty() == false) {
    const CTNode* n1 = stack.back().first;
    CTNode* n2 = stack.back().second;
    stack.pop_back();
    // std::cout << "n2 childs: " << n2->childs();
    // std::cout << "n1 childs: " << n1->childs();
    n2->childs().reserve (n1->nrChilds());
    stack.reserve (n1->nrChilds());
    for (CTChilds::const_iterator chIt = n1->childs().begin();
         chIt != n1->childs().end(); ++ chIt) {
      CTNode* chCopy = new CTNode (**chIt);
      n2->childs().insert_sorted (chCopy);
      if ((*chIt)->nrChilds() > 0) {
        stack.push_back (StackPair (*chIt, chCopy));
      }
    }
  }
  return root2;
}



void
CTNode::deleteSubtree (CTNode* n)
{
  assert (n);
  const CTChilds& childs = n->childs();
  for (CTChilds::const_iterator chIt = childs.begin();
       chIt != childs.end(); ++ chIt) {
    deleteSubtree (*chIt);
  }
  delete n;
}



std::ostream&
operator<< (std::ostream& out, const CTNode& n)
{
  out << "(" << n.level() << ") " ;
  out << n.symbol();
  return out;
}



ConstraintTree::ConstraintTree (unsigned nrLvs)
{
  for (unsigned i = 0; i < nrLvs; i++) {
    logVars_.push_back (LogVar (i));
  }
  root_      = new CTNode (0, 0);
  logVarSet_ = LogVarSet (logVars_);
}



ConstraintTree::ConstraintTree (const LogVars& logVars)
{
  root_      = new CTNode (0, 0);
  logVars_   = logVars;
  logVarSet_ = LogVarSet (logVars);
}



ConstraintTree::ConstraintTree (
    const LogVars& logVars,
    const Tuples&  tuples)
{
  root_      = new CTNode (0, 0);
  logVars_   = logVars;
  logVarSet_ = LogVarSet (logVars);
  for (size_t i = 0; i < tuples.size(); i++) {
    addTuple (tuples[i]);
  }
}



ConstraintTree::ConstraintTree (
     std::vector<std::vector<std::string>> names)
{
  assert (names.empty() == false);
  assert (names.front().empty() == false);
  unsigned nrLvs = names[0].size();
  for (size_t i = 0; i < nrLvs; i++) {
    logVars_.push_back (LogVar (i));
  }
  root_      = new CTNode (0, 0);
  logVarSet_ = LogVarSet (logVars_);
  for (size_t i = 0; i < names.size(); i++) {
    Tuple t;
    for (size_t j = 0; j < names[i].size(); j++) {
      assert (names[i].size() == nrLvs);
      t.push_back (LiftedUtils::getSymbol (names[i][j]));
    }
    addTuple (t);
  }
}



ConstraintTree::ConstraintTree (const ConstraintTree& ct)
{
  *this = ct;
}



ConstraintTree::ConstraintTree (
    const CTChilds& rootChilds,
    const LogVars& logVars)
        : root_(new CTNode (Symbol (0), unsigned (0), rootChilds)),
          logVars_(logVars),
          logVarSet_(logVars)
{

}



ConstraintTree::~ConstraintTree()
{
  CTNode::deleteSubtree (root_);
}



bool
ConstraintTree::empty() const
{
  return root_->childs().empty();
}



void
ConstraintTree::addTuple (const Tuple& tuple)
{
  CTNode* prevNode = root_;
  for (size_t i = 0; i < tuple.size(); i++) {
    CTChilds::const_iterator it = prevNode->findSymbol (tuple[i]);
    if (it == prevNode->childs().end()) {
      CTNode* newNode = new CTNode (tuple[i], i + 1);
      prevNode->mergeSubtree (newNode, false);
      prevNode = newNode;
    } else {
      prevNode  = *it;
    }
  }
}



bool
ConstraintTree::containsTuple (const Tuple& tuple)
{
	CTNode* prevNode = root_;
  for (size_t i = 0; i < tuple.size(); i++) {
    CTChilds::const_iterator it = prevNode->findSymbol (tuple[i]);
    if (it == prevNode->childs().end()) {
      return false;
    } else {
      prevNode = *it;
    }
  }
  return true;
}



void
ConstraintTree::moveToTop (const LogVars& lvs)
{
  for (size_t i = 0; i < lvs.size(); i++) {
    size_t pos = Util::indexOf (logVars_, lvs[i]);
    assert (pos != logVars_.size());
    for (size_t j = pos; j-- > i; ) {
      swapLogVar (logVars_[j]);
    }
  }
}



void
ConstraintTree::moveToBottom (const LogVars& lvs)
{
  for (size_t i = lvs.size(); i-- > 0; ) {
    size_t pos = Util::indexOf (logVars_, lvs[i]);
    assert (pos != logVars_.size());
    size_t stop = logVars_.size() - (lvs.size() - i - 1);
    for (size_t j = pos; j < stop - 1; j++) {
      swapLogVar (logVars_[j]);
    }
  }
}



void
ConstraintTree::join (ConstraintTree* ct, bool oneTwoOne)
{
  if (logVarSet_.empty()) {
    CTNode::deleteSubtree (root_);
    root_      = CTNode::copySubtree (ct->root());
    logVars_   = ct->logVars();
    logVarSet_ = ct->logVarSet();
    return;
  }
  if (oneTwoOne) {
    if (logVarSet_.contains (ct->logVarSet())) {
      return;
    }
    if (ct->logVarSet().contains (logVarSet_)) {
      CTNode::deleteSubtree (root_);
      root_      = CTNode::copySubtree (ct->root());
      logVars_   = ct->logVars();
      logVarSet_ = ct->logVarSet();
      return;
    }
  }
  LogVarSet intersect = logVarSet_ & ct->logVarSet_;
  if (intersect.empty()) {
    // cartesian product
    appendOnBottom (root_, ct->root()->childs());
    Util::addToVector (logVars_, ct->logVars_);
    logVarSet_ |= ct->logVarSet_;
  } else {
    moveToTop (intersect.elements());
    ct->moveToTop (intersect.elements());

    Tuples tuples;
    CTNodes appendNodes;
    getTuples (ct->root(), Tuples(), intersect.size(),
        tuples, appendNodes);

    CTNodes::const_iterator appendIt = appendNodes.begin();
    for (size_t i = 0; i < tuples.size(); ++ i, ++ appendIt) {
      bool tupleFounded = join (root_, tuples[i], 0, *appendIt);
      if (oneTwoOne && tupleFounded == false) {
        assert (false);
      }
    }

    LogVars newLvs (ct->logVars().begin() + intersect.size(), 
                    ct->logVars().end());
    Util::addToVector (logVars_, newLvs);
    logVarSet_ |= LogVarSet (newLvs);
  }
}



unsigned
ConstraintTree::getLevel (LogVar X) const
{
  unsigned level = Util::indexOf (logVars_, X);
  level += 1; // root is in level 0, first logVar is in level 1
  return level;
}



void
ConstraintTree::rename (LogVar X_old, LogVar X_new)
{
  assert (logVarSet_.contains (X_old));
  assert (logVarSet_.contains (X_new) == false);
  logVarSet_ -= X_old;
  logVarSet_ |= X_new;
  for (size_t i = 0; i < logVars_.size(); i++) {
    if (logVars_[i] == X_old) {
      logVars_[i] = X_new;
      return;
    }
  }
  assert (false);
}



void
ConstraintTree::applySubstitution (const Substitution& theta)
{
  for (size_t i = 0; i < logVars_.size(); i++) {
    logVars_[i] = theta.newNameFor (logVars_[i]);
  }
  logVarSet_ = LogVarSet (logVars_);
}



void
ConstraintTree::project (const LogVarSet& X)
{
  assert (logVarSet_.contains (X));
  remove ((logVarSet_ - X));
}



ConstraintTree
ConstraintTree::projectedCopy (const LogVarSet& X)
{
  ConstraintTree copy = *this;
  copy.project (X);
  return copy;
}



void
ConstraintTree::remove (const LogVarSet& X)
{
  assert (logVarSet_.contains (X));
  if (X.empty()) {
    return;
  }
  moveToBottom (X.elements());
  unsigned level = getLevel (X.front()) - 1;
  CTNodes nodes = getNodesAtLevel (level);
  for (CTNodes::const_iterator it = nodes.begin();
       it != nodes.end(); ++ it) {
    (*it)->removeAndDeleteAllChilds();
  }
  logVars_.resize (logVars_.size() - X.size());
  logVarSet_ -= X;
}



bool
ConstraintTree::ConstraintTree::isSingleton (LogVar X)
{
  Symbol symb;
  unsigned level = getLevel (X);
  CTNodes stack;
  stack.push_back (root_);
  while (stack.empty() == false) {
    CTNode* node = stack.back();
    stack.pop_back();
    if (node->level() == level) {
      if (symb.valid()) {
        if (node->symbol() != symb) {
          return false;
        }
      } else {
        symb = node->symbol();
      }
    } else {
      stack.insert (stack.end(), node->childs().begin(),
          node->childs().end());
    }
  }
  return true;
}



LogVarSet
ConstraintTree::singletons()
{
  LogVarSet singletons;
  for (size_t i = 0; i < logVars_.size(); i++) {
    if (isSingleton (logVars_[i])) {
      singletons.insert (logVars_[i]);
    }
  }
  return singletons;
}



TupleSet
ConstraintTree::tupleSet (unsigned stopLevel) const
{
  assert (root_->isRoot());
  Tuples tuples;
  if (stopLevel == 0) {
    stopLevel = logVars_.size();
  }
  getTuples (root_, Tuples(), stopLevel, tuples, CTNodes() = {});
  return TupleSet (tuples);
}



TupleSet
ConstraintTree::tupleSet (const LogVars& originalLvs)
{
  LogVars uniqueLvs;
  for (size_t i = 0; i < originalLvs.size(); i++) {
    if (Util::contains (uniqueLvs, originalLvs[i]) == false) {
      uniqueLvs.push_back (originalLvs[i]);
    }
  }

  Tuples tuples;
  moveToTop (uniqueLvs);
  unsigned stopLevel = uniqueLvs.size();
  getTuples (root_, Tuples(), stopLevel, tuples, CTNodes() = {});

  if (originalLvs.size() != uniqueLvs.size()) {
    std::vector<size_t> indexes;
    indexes.reserve (originalLvs.size());
    for (size_t i = 0; i < originalLvs.size(); i++) {
      indexes.push_back (Util::indexOf (uniqueLvs, originalLvs[i]));
    }
    Tuples tuples2;
    tuples2.reserve (tuples.size());
    for (size_t i = 0; i < tuples.size(); i++) {
      Tuple t;
      t.reserve (originalLvs.size());
      for (size_t j = 0; j < originalLvs.size(); j++) {
        t.push_back (tuples[i][indexes[j]]);
      }
      tuples2.push_back (t);
    }
    return TupleSet (tuples2);
  }

  return TupleSet (tuples);
}



void
ConstraintTree::exportToGraphViz (
    const char* fileName,
    bool showLogVars) const
{
  std::ofstream out (fileName);
  if (!out.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    return;
  }
  out << "digraph {" << std::endl;
  ConstraintTree copy (*this);
  copy.moveToTop (copy.logVarSet_.elements());
  CTNodes nodes = getNodesBelow (copy.root_);
  out << "\"" << copy.root_ << "\"" << " [label=\"R\"]" << std::endl;
  for (CTNodes::const_iterator it = ++ nodes.begin();
       it != nodes.end(); ++ it) {
    out << "\"" << *it << "\"";
    out << " [label=\"" << **it << "\"]" ;
    out << std::endl;
  }
  for (CTNodes::const_iterator it = nodes.begin();
       it != nodes.end(); ++ it) {
    const CTChilds& childs = (*it)->childs();
    for (CTChilds::const_iterator chIt = childs.begin();
         chIt != childs.end(); ++ chIt) {
      out << "\"" << *it << "\"" ;
      out << " -> " ;
      out << "\"" << *chIt << "\"" << std::endl ;
    }
  }
  if (showLogVars) {
    out << "Root [label=\"\", shape=plaintext]" << std::endl;
    for (size_t i = 0; i < copy.logVars_.size(); i++) {
      out << copy.logVars_[i] << " [label=" ;
      out << copy.logVars_[i] << ", " ;
      out << "shape=plaintext, fontsize=14]" << std::endl;
    }
   out << "Root -> " << copy.logVars_[0];
   out << " [style=invis]" << std::endl;
    for (size_t i = 0; i < copy.logVars_.size() - 1; i++) {
      out << copy.logVars_[i] << " -> " << copy.logVars_[i + 1];
      out << " [style=invis]" << std::endl;
    }
  }
  out << "}" <<std::endl;
  out.close();
}



bool
ConstraintTree::isCountNormalized (const LogVarSet& Ys)
{
  assert (logVarSet_.contains (Ys));
  if (Ys.empty()) {
    return true;
  }
  if (Ys.size() == logVars_.size()) {
    assert (LogVarSet (logVars_) == LogVarSet (Ys));
    return true;
  }
  LogVarSet Zs = logVarSet_ - LogVarSet (Ys);
  moveToTop (Zs.elements());
  CTNodes nodes = getNodesAtLevel (Zs.size());
  unsigned count = countTuples (*nodes.begin());
  for (CTNodes::const_iterator it = nodes.begin();
       it != nodes.end(); ++ it) {
    if (countTuples (*it) != count) {
      return false;
    }
  }
  return true;
}



unsigned
ConstraintTree::getConditionalCount (const LogVarSet& Ys)
{
  assert (isCountNormalized (Ys));
  if (Ys.empty()) {
    return 1;
  }
  if (Ys.size() == logVars_.size()) {
    assert (LogVarSet (Ys) == LogVarSet (logVars_));
    return countTuples (root_);
  }
  LogVarSet Zs = logVarSet_ - Ys;
  moveToTop (Zs.elements());
  CTNode* n = root_;
  unsigned l = 0;
  while (l != Zs.size()) {
    n = *(n->childs().begin());
    l ++;
  }
  return countTuples (n);
}



TinySet<unsigned>
ConstraintTree::getConditionalCounts (const LogVarSet& Ys)
{
  TinySet<unsigned> counts;
  assert (logVarSet_.contains (Ys));
  if (Ys.empty()) {
    counts.insert (1);
  } else if (Ys.size() == logVars_.size()) {
    assert (LogVarSet (logVars_) == LogVarSet (Ys));
    counts.insert (countTuples (root_));
  } else {
    LogVarSet Zs = logVarSet_ - LogVarSet (Ys);
    moveToTop (Zs.elements());
    CTNodes nodes = getNodesAtLevel (Zs.size());
    for (CTNodes::const_iterator it = nodes.begin();
         it != nodes.end(); ++ it) {
      counts.insert (countTuples (*it));
    }
  }
  return counts;
}



bool
ConstraintTree::isCartesianProduct (const LogVarSet& Xs)
{
  assert (logVarSet_.contains (Xs));
  if (Xs.size() <= 1) {
    return true;
  }
  moveToTop (Xs.elements());
  for (size_t i = 1; i < Xs.size(); i++) {
    CTNodes nodes = getNodesAtLevel (i);
    for (size_t j = 1; j < nodes.size(); j++) {
      if (nodes[j-1]->nrChilds() != nodes[ j ]->nrChilds()) {
        return false;
      }
      if (nodes[j-1]->childSymbols() != nodes[ j ]->childSymbols()) {
        return false;
      }
    }
  }
  return true;
}



std::pair<ConstraintTree*,ConstraintTree*>
ConstraintTree::split (const Tuple& tuple)
{
  // assumes that my log vars are already on top
  LogVars lvs (logVars_.begin(), logVars_.begin() + tuple.size());
  ConstraintTree tempCt (logVars_, {tuple});
  return split (lvs, &tempCt, lvs);
}



std::pair<ConstraintTree*, ConstraintTree*>
ConstraintTree::split (
    const LogVars& lvs1,
    ConstraintTree* ct,
    const LogVars& lvs2)
{
  assert (lvs1.size() == lvs2.size());
  assert (lvs1.size() == LogVarSet (lvs1).size());
  assert (lvs2.size() == LogVarSet (lvs2).size());
  assert (logVarSet_.contains (lvs1));
  assert (ct->logVarSet().contains (lvs2));
  CTChilds commChilds, exclChilds;
  unsigned stopLevel = lvs1.size();
  split (root_, ct->root(), commChilds, exclChilds, stopLevel);
  ConstraintTree* commCt = new ConstraintTree (commChilds, logVars_);
  ConstraintTree* exclCt = new ConstraintTree (exclChilds, logVars_);
  // std::cout << commCt->tupleSet() << " + " ;
  // std::cout << exclCt->tupleSet() << " = " ;
  // std::cout << tupleSet() << std::endl;
  assert ((commCt->tupleSet() | exclCt->tupleSet()) == tupleSet());
  assert ((exclCt->tupleSet (stopLevel) & ct->tupleSet (stopLevel)).empty());
  return {commCt, exclCt};
}



ConstraintTrees
ConstraintTree::countNormalize (const LogVarSet& Ys)
{
  assert (logVarSet_.contains (Ys));
  LogVarSet Zs = logVarSet_ - Ys;
  if (Ys.empty() || Zs.empty()) {
    return { new ConstraintTree (*this) };
  }
  moveToTop (Zs.elements());
  ConstraintTrees cts;
  std::unordered_map<unsigned, ConstraintTree*> countMap;
  unsigned stopLevel = getLevel (Zs.back());
  const CTChilds& childs = root_->childs();

  for (CTChilds::const_iterator chIt = childs.begin();
       chIt != childs.end(); ++ chIt) {
    const std::vector<std::pair<CTNode*, unsigned>>& res =
        countNormalize (*chIt, stopLevel);
    for (size_t j = 0; j < res.size(); j++) {
      std::unordered_map<unsigned, ConstraintTree*>::iterator it
          = countMap.find (res[j].second);
      if (it == countMap.end()) {
        ConstraintTree* newCt = new ConstraintTree (logVars_);
        it = countMap.insert (std::make_pair (res[j].second, newCt)).first;
        cts.push_back (newCt);
      }
      it->second->root_->mergeSubtree (res[j].first);
    }
  }
  return cts;
}



ConstraintTrees
ConstraintTree::jointCountNormalize (
    ConstraintTree* commCt,
    ConstraintTree* exclCt,
    LogVar X,
    LogVar X_new1,
    LogVar X_new2)
{
  unsigned N = getConditionalCount (X);
  // std::cout << "My tuples:     " << tupleSet() << std::endl;
  // std::cout << "CommCt tuples: " << commCt->tupleSet() << std::endl;
  // std::cout << "ExclCt tuples: " << exclCt->tupleSet() << std::endl;
  // std::cout << "Counted Lv:    " << X << std::endl;
  // std::cout << "X_new1:        " << X_new1 << std::endl;
  // std::cout << "X_new2:        " << X_new2 << std::endl;
  // std::cout << "Original N:    " << N << std::endl;
  // std::cout << endl;

  ConstraintTrees normCts1 = commCt->countNormalize (X);
  std::vector<unsigned> counts1 (normCts1.size());
  for (size_t i = 0; i < normCts1.size(); i++) {
    counts1[i] = normCts1[i]->getConditionalCount (X);
    // std::cout << "normCts1[" << i << "] #" << counts1[i] ;
    // std::cout << " " << normCts1[i]->tupleSet() << std::endl;
  }

  ConstraintTrees normCts2 = exclCt->countNormalize (X);
  std::vector<unsigned> counts2 (normCts2.size());
  for (size_t i = 0; i < normCts2.size(); i++) {
    counts2[i] = normCts2[i]->getConditionalCount (X);
    // std::cout << "normCts2[" << i << "] #" << counts2[i] ;
    // std::cout << " " << normCts2[i]->tupleSet() << std::endl;
  }
  // std::cout << std::endl;

  ConstraintTree* excl1 = 0;
  for (size_t i = 0; i < normCts1.size(); i++) {
    if (counts1[i] == N) {
      excl1 = normCts1[i];
      normCts1.erase (normCts1.begin() + i);
      counts1.erase (counts1.begin() + i);
      // std::cout << "joint-count(" << N << ",0)" << std::endl;
      break;
    }
  }

  ConstraintTree* excl2 = 0;
  for (size_t i = 0; i < normCts2.size(); i++) {
    if (counts2[i] == N) {
      excl2 = normCts2[i];
      normCts2.erase (normCts2.begin() + i);
      counts2.erase (counts2.begin() + i);
      // std::cout << "joint-count(0," << N << ")" << std::endl;
      break;
    }
  }

  for (size_t i = 0; i < normCts1.size(); i++) {
    unsigned j;
    for (j = 0; counts1[i] + counts2[j] != N; j++) ;
    // std::cout << "joint-count(" << counts1[i] ;
    // std::cout <<  "," << counts2[j] << ")" << std::endl;
    const CTChilds& childs = normCts2[j]->root_->childs();
    for (CTChilds::const_iterator chIt = childs.begin();
         chIt != childs.end(); ++ chIt) {
      normCts1[i]->root_->mergeSubtree (CTNode::copySubtree (*chIt));
    }
    delete normCts2[j];
  }

  ConstraintTrees cts = normCts1;
  commCt->rename (X, X_new1);
  exclCt->rename (X, X_new2);
  for (size_t i = 0; i < cts.size(); i++) {
    cts[i]->remove (X);
    cts[i]->join (commCt);
    cts[i]->join (exclCt);
  }

  if (excl1) {
    cts.push_back (excl1);
  }
  if (excl2) {
    cts.push_back (excl2);
  }

  return cts;
}



LogVars
ConstraintTree::expand (LogVar X)
{
  moveToBottom ({X});
  assert (isCountNormalized (X));
  CTNodes nodes = getNodesAtLevel (logVars_.size() - 1);
  unsigned nrSymbols = getConditionalCount (X);
  for (CTNodes::const_iterator it = nodes.begin();
       it != nodes.end(); ++ it) {
    Symbols symbols;
    const CTChilds& childs = (*it)->childs();
    for (CTChilds::const_iterator chIt = childs.begin();
         chIt != childs.end(); ++ chIt) {
      symbols.push_back ((*chIt)->symbol());
    }
    (*it)->removeAndDeleteAllChilds();
    CTNode* prev = *it;
    assert (symbols.size() == nrSymbols);
    for (size_t j = 0; j < nrSymbols; j++) {
      CTNode* newNode = new CTNode (symbols[j], (*it)->level() + j);
      prev->mergeSubtree (newNode);
      prev = newNode;
    }
  }
  LogVars newLvs;
  logVars_.pop_back();
  for (size_t i = 0; i < nrSymbols; i++) {
    logVars_.push_back (LogVar (logVarSet_.back() + 1));
    newLvs.push_back   (LogVar (logVarSet_.back() + 1));
    logVarSet_.insert  (LogVar (logVarSet_.back() + 1));
  }
  logVarSet_ -= X;
  return newLvs;
}



ConstraintTrees
ConstraintTree::ground (LogVar X)
{
  moveToTop ({X});
  ConstraintTrees cts;
  const CTChilds& nodes = root_->childs();
  for (CTChilds::const_iterator it = nodes.begin();
       it != nodes.end(); ++ it) {
    CTNode* copy = CTNode::copySubtree (*it);
    copy->setSymbol ((*it)->symbol());
    ConstraintTree* newCt = new ConstraintTree (logVars_);
    newCt->root()->mergeSubtree (copy);
    cts.push_back (newCt);
  }
  return cts;
}



void
ConstraintTree::cloneLogVar (LogVar X_1, LogVar X_2)
{
  moveToBottom ({X_1});
  CTNodes leafs = getNodesAtLevel (logVars_.size());
  for (size_t i = 0; i < leafs.size(); i++) {
    leafs[i]->childs().insert_sorted (
        new CTNode (leafs[i]->symbol(), leafs[i]->level() + 1));
  }
  logVars_.push_back (X_2);
  logVarSet_.insert (X_2);
}



ConstraintTree&
ConstraintTree::operator= (const ConstraintTree& ct)
{
  if (this != &ct) {
    root_      = CTNode::copySubtree (ct.root_);
    logVars_   = ct.logVars_;
    logVarSet_ = ct.logVarSet_;
  }
  return *this;
}



unsigned
ConstraintTree::countTuples (const CTNode* n) const
{
  if (n->isLeaf()) {
    return 1;
  }
  unsigned sum = 0;
  const CTChilds& childs = n->childs();
  for (CTChilds::const_iterator chIt = childs.begin();
      chIt != childs.end(); ++ chIt) {
    sum += countTuples (*chIt);
  }
  return sum;
}



CTNodes
ConstraintTree::getNodesBelow (CTNode* fromHere) const
{
  CTNodes nodes;
  std::queue<CTNode*> queue;
  queue.push (fromHere);
  while (queue.empty() == false) {
    CTNode* node = queue.front();
    nodes.push_back (node);
    for (CTChilds::const_iterator chIt = node->childs().begin();
         chIt != node->childs().end(); ++ chIt) {
      queue.push (*chIt);
    }
    queue.pop();
  }
  return nodes;
}



CTNodes
ConstraintTree::getNodesAtLevel (unsigned level) const
{
  assert (level <= logVars_.size());
  if (level == 0) {
    return { root_ };
  }
  CTNodes stack;
  CTNodes nodes;
  stack.push_back (root_);
  while (stack.empty() == false) {
    CTNode* node = stack.back();
    stack.pop_back();
    if (node->level() + 1 == level) {
      nodes.insert (nodes.end(), node->childs().begin(),
          node->childs().end());
    } else {
      stack.insert (stack.end(), node->childs().begin(),
          node->childs().end());
    }
  }
  return nodes;
}



unsigned
ConstraintTree::nrNodes (const CTNode* n) const
{
  unsigned nr = 0;
  if (n->isLeaf() == false) {
    for (CTChilds::const_iterator chIt = n->childs().begin();
         chIt != n->childs().end(); ++ chIt) {
      nr += nrNodes (*chIt);
    }
  }
  return nr;
}



void
ConstraintTree::appendOnBottom (CTNode* n, const CTChilds& childs)
{
  if (childs.empty()) {
    return;
  }
  CTNodes stack { n };
  while (stack.empty() == false) {
    CTNode* node = stack.back();
    stack.pop_back();
    if (node->isLeaf()) {
      for (CTChilds::const_iterator chIt = childs.begin();
           chIt != childs.end(); ++ chIt) {
        node->mergeSubtree (CTNode::copySubtree (*chIt));
      }
    } else {
      stack.insert (stack.end(), node->childs().begin(),
          node->childs().end());
    }
  }
}



void
ConstraintTree::swapLogVar (LogVar X)
{
  size_t pos = Util::indexOf (logVars_, X);
  assert (pos != logVars_.size());
  CTNodes nodes = getNodesAtLevel (pos);
  for (CTNodes::const_iterator nodeIt = nodes.begin();
       nodeIt != nodes.end(); ++ nodeIt) {
    CTChilds childsCopy = (*nodeIt)->childs();
    (*nodeIt)->removeChilds();
    for (CTChilds::const_iterator ccIt = childsCopy.begin();
         ccIt != childsCopy.end(); ++ ccIt) {
       const CTChilds& grandsons = (*ccIt)->childs();
       for (CTChilds::const_iterator gsIt = grandsons.begin();
            gsIt != grandsons.end(); ++ gsIt) {
         CTNode* childCopy = new CTNode (
             (*ccIt)->symbol(), (*ccIt)->level() + 1, (*gsIt)->childs());
         (*gsIt)->removeChilds();
         (*gsIt)->childs().insert_sorted (childCopy);
         (*gsIt)->setLevel ((*gsIt)->level() - 1);
         (*nodeIt)->mergeSubtree ((*gsIt), false);
       }
       delete (*ccIt);
    }
  }
  std::swap (logVars_[pos], logVars_[pos + 1]);
}



bool
ConstraintTree::join (
    CTNode* currNode,
    const Tuple& tuple,
    size_t currIdx,
    CTNode* appendNode)
{
  bool tupleFounded = false;
  CTChilds::const_iterator it = currNode->findSymbol (tuple[currIdx]);
  if (it != currNode->childs().end()) {
    if (currIdx == tuple.size() - 1) {
      appendOnBottom (*it, appendNode->childs());
      return true;
    } else {
      tupleFounded = join (*it, tuple, currIdx + 1, appendNode);
    }
  }
  return tupleFounded;
}



void
ConstraintTree::getTuples (
    CTNode* n,
    Tuples currTuples,
    unsigned stopLevel,
    Tuples& tuplesCollected,
    CTNodes& continuationNodes) const
{
  if (n->isRoot() == false) {
    if (currTuples.empty()) {
        currTuples.push_back ({ n->symbol()});
    } else {
      for (size_t i = 0; i < currTuples.size(); i++) {
        currTuples[i].push_back (n->symbol());
      }
    }
    if (n->level() == stopLevel) {
      for (size_t i = 0; i < currTuples.size(); i++) {
        tuplesCollected.push_back (currTuples[i]);
        continuationNodes.push_back (n);
      }
      return;
    }
  }
  const CTChilds& childs = n->childs();
  for (CTChilds::const_iterator chIt = childs.begin();
      chIt != childs.end(); ++ chIt) {
    getTuples (*chIt, currTuples, stopLevel, tuplesCollected,
        continuationNodes);
  }
}



unsigned
ConstraintTree::size() const
{
  return countTuples (root_);
}



unsigned
ConstraintTree::nrSymbols (LogVar X)
{
  moveToTop ({X});
  return root_->childs().size();
}



std::vector<std::pair<CTNode*, unsigned>>
ConstraintTree::countNormalize (
     const CTNode* n,
     unsigned stopLevel)
{
  if (n->level() == stopLevel) {
    return std::vector<std::pair<CTNode*, unsigned>>() = {
      std::make_pair (CTNode::copySubtree (n), countTuples (n))
    };
  }
  std::vector<std::pair<CTNode*, unsigned>> res;
  const CTChilds& childs = n->childs();
  for (CTChilds::const_iterator chIt = childs.begin();
       chIt != childs.end(); ++ chIt) {
    const std::vector<std::pair<CTNode*, unsigned>>& lowerRes =
        countNormalize (*chIt, stopLevel);
    for (size_t j = 0; j < lowerRes.size(); j++) {
      CTNode* newNode = new CTNode (*n);
      newNode->mergeSubtree (lowerRes[j].first);
      res.push_back (std::make_pair (newNode, lowerRes[j].second));
    }
  }
  return res;
}



void
ConstraintTree::split (
    CTNode* n1,
    CTNode* n2,
    CTChilds& commChilds,
    CTChilds& exclChilds,
    unsigned stopLevel)
{
  CTChilds& childs1 = n1->childs();
  for (CTChilds::const_iterator chIt1 = childs1.begin();
       chIt1 != childs1.end(); ++ chIt1) {
    CTChilds::iterator chIt2 = n2->findSymbol ((*chIt1)->symbol());
    if (chIt2 == n2->childs().end()) {
      exclChilds.insert_sorted (CTNode::copySubtree (*chIt1));
    } else {
      if ((*chIt1)->level() == stopLevel) {
        commChilds.insert_sorted (CTNode::copySubtree (*chIt1));
      } else {
        CTChilds lowerCommChilds, lowerExclChilds;
        split (*chIt1, *chIt2, lowerCommChilds, lowerExclChilds, stopLevel);
        if (lowerCommChilds.empty() == false) {
          commChilds.insert_sorted (new CTNode (**chIt1, lowerCommChilds));
        }
        if (lowerExclChilds.empty() == false) {
          exclChilds.insert_sorted (new CTNode (**chIt1, lowerExclChilds));
        }
      }
    }
  }
}

}  // namespace Horus

