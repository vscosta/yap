#include <queue>

#include <fstream>

#include "ConstraintTree.h"
#include "Util.h"


void
CTNode::mergeSubtree (CTNode* n, bool updateLevels)
{
  if (updateLevels) {
    updateChildLevels (n, level_ + 1);
  }
  CTChilds::iterator chIt = findChild (n);
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
  CTChilds::iterator it;
  it = findChild (child);
  assert (it != childs_.end());
  childs_.erase (it);
}



void
CTNode::removeChilds (void)
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
CTNode::removeAndDeleteAllChilds (void)
{
  for (CTChilds::const_iterator chIt = childs_.begin();
       chIt != childs_.end(); ++ chIt) {
    deleteSubtree (*chIt);
  }
  childs_.clear();
}



SymbolSet
CTNode::childSymbols (void) const
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
  /*
  n->setLevel (level);
  const CTChilds& childs = n->childs();
  for (CTChilds::const_iterator chIt = childs.begin();
       chIt != childs.end(); ++ chIt) {
    updateChildLevels (*chIt, level + 1);
  }
  */
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
  typedef pair<const CTNode*, CTNode*> StackPair;
  vector<StackPair> stack = { StackPair (root1, root2) };
  while (stack.empty() == false) {
    const CTNode* n1 = stack.back().first;
    CTNode* n2 = stack.back().second;
    stack.pop_back();
    n2->childs().reserve (n1->nrChilds());
    stack.reserve (n1->nrChilds());
    for (CTChilds::const_iterator chIt = n1->childs().begin();
         chIt != n1->childs().end(); ++ chIt) {
      CTNode* chCopy = new CTNode (**chIt);
      n2->childs().push_back (chCopy);
      if ((*chIt)->nrChilds() != 0) {
        stack.push_back (StackPair (*chIt, chCopy));
      }
    }
  }
  return root2;
  /*
  CTNode* newNode = new CTNode (*root1);
  const CTChilds& childs = root1->childs();
  newNode->childs().reserve (childs.size());
  for (CTChilds::const_iterator chIt = childs.begin();
       chIt != childs.end(); ++ chIt) {
    newNode->childs().push_back ((copySubtree (*chIt)));
  }
  return newNode;
  */
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



ostream& operator<< (ostream &out, const CTNode& n)
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
  for (unsigned i = 0; i < tuples.size(); i++) {
    addTuple (tuples[i]);
  }
}



ConstraintTree::ConstraintTree (const ConstraintTree& ct)
{
  root_       = CTNode::copySubtree (ct.root_);
  logVars_    = ct.logVars_;
  logVarSet_  = ct.logVarSet_;
}



ConstraintTree::~ConstraintTree (void)
{
  CTNode::deleteSubtree (root_);
}



void
ConstraintTree::addTuple (const Tuple& tuple)
{
  CTNode* prevNode = root_;
  for (unsigned i = 0; i < tuple.size(); i++) {
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
  for (unsigned i = 0; i < tuple.size(); i++) {
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
  for (unsigned i = 0; i < lvs.size(); i++) {
    LogVars::iterator it = 
        std::find (logVars_.begin(), logVars_.end(), lvs[i]);
    assert (it != logVars_.end());
    unsigned pos = std::distance (logVars_.begin(), it);
    for (unsigned j = pos; j > i; j--) {
      swapLogVar (logVars_[j-1]);
    }                                 
  }
}



void
ConstraintTree::moveToBottom (const LogVars& lvs)
{
  for (int i = lvs.size() - 1; i >= 0; i--) {
    LogVars::iterator it = 
        std::find (logVars_.begin(), logVars_.end(), lvs[i]);
    assert (it != logVars_.end());
    unsigned pos = std::distance (logVars_.begin(), it);
    unsigned stop = logVars_.size() - (lvs.size() - i - 1);
    for (unsigned j = pos; j < stop - 1; j++) {
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
    // carteesian product
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
    for (unsigned i = 0; i < tuples.size(); ++ i, ++ appendIt) {
      bool tupleFounded = join (root_, tuples[i], 0, *appendIt);
      if (oneTwoOne) {
        assert (tupleFounded);
        tupleFounded = true; // hack to avoid gcc warning 
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
  LogVars::const_iterator it = 
      std::find (logVars_.begin(), logVars_.end(), X);
  assert (it != logVars_.end());
  unsigned level = std::distance (logVars_.begin(), it);
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
  for (unsigned i = 0; i < logVars_.size(); i++) {
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
  LogVars discardedLvs = theta.getDiscardedLogVars();
  for (unsigned i = 0; i < discardedLvs.size(); i++) {
    remove(discardedLvs[i]);
  }
  for (unsigned i = 0; i < logVars_.size(); i++) {
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
  /*
  const CTNodes& nodes = getNodesAtLevel (getLevel (X));
  Symbol symb = nodes.front()->symbol();
  for (CTNodes::const_iterator it = nodes.begin();
       it != nodes.end(); ++ it) {
    if ((*it)->symbol() != symb) {
      return false;
    }
  }
  return true;
  */
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
ConstraintTree::singletons (void)
{
  LogVarSet singletons;
  for (unsigned i = 0; i < logVars_.size(); i++) {
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
ConstraintTree::tupleSet (const LogVars& lvs)
{  
  Tuples tuples;
  moveToTop (lvs);
  unsigned stopLevel = lvs.size();
  getTuples (root_, Tuples(), stopLevel, tuples, CTNodes() = {});
  return TupleSet (tuples);
}



void
ConstraintTree::exportToGraphViz (
    const char* fileName,
    bool showLogVars) const
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "ConstraintTree::exportToDotFile()" << endl;
    abort();
  }
  out << "digraph {" << endl;
  ConstraintTree copy (*this);
  // copy.moveToTop (copy.logVarSet_.elements());
  CTNodes nodes = getNodesBelow (copy.root_);
  out << "\"" << copy.root_ << "\"" << " [label=\"R\"]" << endl;
  for (CTNodes::const_iterator it = ++ nodes.begin();
       it != nodes.end(); ++ it) {
    out << "\"" << *it << "\"";
    out << " [label=\"" << **it << "\"]" ;
    out << endl;
  }
  for (CTNodes::const_iterator it = nodes.begin();
       it != nodes.end(); ++ it) {
    const CTChilds& childs = (*it)->childs();
    for (CTChilds::const_iterator chIt = childs.begin();
         chIt != childs.end(); ++ chIt) {
      out << "\"" << *it << "\"" ;
      out << " -> " ;
      out << "\"" << *chIt << "\"" << endl ;
    }
  }
  if (showLogVars) {
    out << "Root [label=\"\", shape=plaintext]" << endl;
    for (unsigned i = 0; i < copy.logVars_.size(); i++) {
      out << copy.logVars_[i] << " [label=" ;
      out << copy.logVars_[i] << ", " ;
      out << "shape=plaintext, fontsize=14]" << endl;
    }
   out << "Root -> " << copy.logVars_[0];
   out << " [style=invis]" << endl;
    for (unsigned i = 0; i < copy.logVars_.size() - 1; i++) {
      out << copy.logVars_[i] << " -> " << copy.logVars_[i + 1];
      out << " [style=invis]" << endl;
    }
  }
  out << "}" << endl;
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
ConstraintTree::isCarteesianProduct (const LogVarSet& Xs) const
{
  assert (logVarSet_.contains (Xs));
  if (Xs.size() <= 1) {
    return true;
  }
  for (unsigned i = 1; i < Xs.size(); i++) {
    CTNodes nodes = getNodesAtLevel (i);
    for (unsigned j = 1; j < nodes.size(); j++) {
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
ConstraintTree::split (
    const Tuple& tuple,
    unsigned stopLevel)
{
  ConstraintTree tempCt (logVars_, {tuple});
  return split (&tempCt, stopLevel);
}



std::pair<ConstraintTree*, ConstraintTree*>
ConstraintTree::split (
    const ConstraintTree* ct,
    unsigned stopLevel) const
{
  assert (stopLevel <= logVars_.size());
  assert (stopLevel <= ct->logVars_.size());
  CTChilds commChilds, exclChilds;
  split (root_, ct->root(), commChilds, exclChilds, stopLevel);
  ConstraintTree* commCt = new ConstraintTree (commChilds, logVars_);
  ConstraintTree* exclCt = new ConstraintTree (exclChilds, logVars_);
  // cout << commCt->tupleSet() << " + " ;
  // cout << exclCt->tupleSet() << " = " ;
  // cout << tupleSet() << endl;
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
  unordered_map<unsigned, ConstraintTree*> countMap;
  unsigned stopLevel = getLevel (Zs.back());
  const CTChilds& childs = root_->childs();

  for (CTChilds::const_iterator chIt = childs.begin();
       chIt != childs.end(); ++ chIt) {
    const vector<pair<CTNode*, unsigned>>& res =
        countNormalize (*chIt, stopLevel);
    for (unsigned j = 0; j < res.size(); j++) {
      unordered_map<unsigned, ConstraintTree*>::iterator it
          = countMap.find (res[j].second);
      if (it == countMap.end()) {
        ConstraintTree* newCt = new ConstraintTree (logVars_);
        it = countMap.insert (make_pair (res[j].second, newCt)).first;
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
  // cout << "My tuples:     " << tupleSet() << endl;
  // cout << "CommCt tuples: " << commCt->tupleSet() << endl;
  // cout << "ExclCt tuples: " << exclCt->tupleSet() << endl;
  // cout << "Counted Lv:    " << X << endl;
  // cout << "X_new1:        " << X_new1 << endl;
  // cout << "X_new2:        " << X_new2 << endl;
  // cout << "Original N:    " << N << endl;
  // cout << endl;

  ConstraintTrees normCts1 = commCt->countNormalize (X);
  vector<unsigned> counts1 (normCts1.size());
  for (unsigned i = 0; i < normCts1.size(); i++) {
    counts1[i] = normCts1[i]->getConditionalCount (X);
    // cout << "normCts1[" << i << "] #" << counts1[i] ;
    // cout << " " << normCts1[i]->tupleSet() << endl;
  }

  ConstraintTrees normCts2 = exclCt->countNormalize (X);
  vector<unsigned> counts2 (normCts2.size());
  for (unsigned i = 0; i < normCts2.size(); i++) {
    counts2[i] = normCts2[i]->getConditionalCount (X);
    // cout << "normCts2[" << i << "] #" << counts2[i] ;
    // cout << " " << normCts2[i]->tupleSet() << endl;
  }
  // cout << endl;

  ConstraintTree* excl1 = 0;
  for (unsigned i = 0; i < normCts1.size(); i++) {
    if (counts1[i] == N) {
      excl1 = normCts1[i];
      normCts1.erase (normCts1.begin() + i);
      counts1.erase (counts1.begin() + i);
      // cout << "joint-count(" << N << ",0)" << endl;
      break;
    }
  }

  ConstraintTree* excl2 = 0;
  for (unsigned i = 0; i < normCts2.size(); i++) {
    if (counts2[i] == N) {
      excl2 = normCts2[i];
      normCts2.erase (normCts2.begin() + i);
      counts2.erase (counts2.begin() + i);
      // cout << "joint-count(0," << N << ")" << endl;
      break;
    }
  }

  for (unsigned i = 0; i < normCts1.size(); i++) {
    unsigned j; 
    for (j = 0; counts1[i] + counts2[j] != N; j++) ;
    // cout << "joint-count(" << counts1[i] ;
    // cout <<  "," << counts2[j] << ")" << endl;
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
  for (unsigned i = 0; i < cts.size(); i++) {
    cts[i]->remove (X);
    cts[i]->join (commCt);
    cts[i]->join (exclCt);
  }

  if (excl1 != 0) {
    cts.push_back (excl1);
  }
  if (excl2 != 0) {
    cts.push_back (excl2);
  }

  return cts;
}



bool
ConstraintTree::identical (
  const ConstraintTree* ct1,
  const ConstraintTree* ct2,
  unsigned stopLevel)
{
  TupleSet ts1 = ct1->tupleSet (stopLevel);
  TupleSet ts2 = ct2->tupleSet (stopLevel);
  return ts1 == ts2;
}



bool
ConstraintTree::disjoint (
  const ConstraintTree* ct1,
  const ConstraintTree* ct2,
  unsigned stopLevel)
{
  TupleSet ts1 = ct1->tupleSet (stopLevel);
  TupleSet ts2 = ct2->tupleSet (stopLevel);
  return (ts1 & ts2).empty();
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
    for (unsigned j = 0; j < nrSymbols; j++) {
      CTNode* newNode = new CTNode (symbols[j], (*it)->level() + j);
      prev->mergeSubtree (newNode);
      prev = newNode;
    }
  }
  LogVars newLvs;
  logVars_.pop_back();
  for (unsigned i = 0; i < nrSymbols; i++) {
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
  queue<CTNode*> queue;
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
  /*
  CTNodes nodes;
  queue<CTNode*> queue;
  queue.push (root_);
  while (queue.empty() == false) {
    CTNode* node = queue.front();
    if (node->level() == level) {
      nodes.push_back (node);
    } else {
      for (CTChilds::const_iterator chIt = node->childs().begin();
           chIt != node->childs().end(); ++ chIt) {
        queue.push (*chIt);
      }
    }
    queue.pop();
  }
  */
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
  LogVars::iterator it;
  it = std::find (logVars_.begin(),logVars_.end(), X);
  assert (it != logVars_.end());
  unsigned pos = std::distance (logVars_.begin(), it);
  const CTNodes& nodes = getNodesAtLevel (pos);
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
         (*gsIt)->childs().push_back (childCopy);
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
    unsigned currIdx,
    CTNode* appendNode)
{
  bool tupleFounded = false;
  CTChilds::const_iterator it = currNode->findSymbol (tuple[currIdx]);
  if (it != currNode->childs().end()) {
    if (currIdx == tuple.size() - 1) {
      appendOnBottom (currNode, appendNode->childs());
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
    if (currTuples.size() == 0) {
        currTuples.push_back ({ n->symbol()});
    } else {
      for (unsigned i = 0; i < currTuples.size(); i++) {
        currTuples[i].push_back (n->symbol());
      }
    }
    if (n->level() == stopLevel) {
      for (unsigned i = 0; i < currTuples.size(); i++) {
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
ConstraintTree::size (void) const
{
  return countTuples (root_);
}



unsigned
ConstraintTree::nrSymbols (LogVar X)
{
  moveToTop ({X});
  return root_->childs().size();
}



vector<pair<CTNode*, unsigned>>
ConstraintTree::countNormalize (
     const CTNode* n,
     unsigned stopLevel)
{
  if (n->level() == stopLevel) {
    return vector<pair<CTNode*, unsigned>>() = {
      make_pair (CTNode::copySubtree (n), countTuples (n))
    };
  }
  vector<pair<CTNode*, unsigned>> res;
  const CTChilds& childs = n->childs();
  for (CTChilds::const_iterator chIt = childs.begin();
       chIt != childs.end(); ++ chIt) {
    const vector<pair<CTNode*, unsigned>>& lowerRes =
        countNormalize (*chIt, stopLevel);
    for (unsigned j = 0; j < lowerRes.size(); j++) {
      CTNode* newNode = new CTNode (*n);
      newNode->mergeSubtree (lowerRes[j].first);
      res.push_back (make_pair (newNode, lowerRes[j].second));
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
      exclChilds.push_back (CTNode::copySubtree (*chIt1));
    } else {
      if ((*chIt1)->level() == stopLevel) {
        commChilds.push_back (CTNode::copySubtree (*chIt1));
      } else {
        CTChilds lowerCommChilds, lowerExclChilds;
        split (*chIt1, *chIt2, lowerCommChilds, lowerExclChilds, stopLevel);
        if (lowerCommChilds.empty() == false) {
          commChilds.push_back (new CTNode (**chIt1, lowerCommChilds));
        }
        if (lowerExclChilds.empty() == false) {
          exclChilds.push_back (new CTNode (**chIt1, lowerExclChilds));
        }
      }
    }
  }
}

