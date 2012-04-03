#include <queue>

#include <fstream>

#include "ConstraintTree.h"
#include "Util.h"


void
CTNode::addChild (CTNode* child, bool updateLevels)
{
  if (updateLevels) {
    updateChildLevels (child, level_ + 1);
  }
  bool found = false;
  for (unsigned i = 0; i < childs_.size(); i++) {
    if (child->symbol() == childs_[i]->symbol()) {
      CTNodes childChilds = child->childs();
      for (unsigned j = 0; j < childChilds.size(); j++) {
        childs_[i]->addChild (childChilds[j], false);
      }
      found = true;
      break;
    }
  }
  if (found) {
    delete child;
  } else {
    childs_.push_back (child);
  }
}



void
CTNode::removeChild (CTNode* child)
{
  CTNodes::iterator it = 
      std::find (childs_.begin(), childs_.end(), child);
  assert (it != childs_.end());
  childs_.erase (it);
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
  for (unsigned i = 0; i < childs_.size(); i++) {
    deleteSubtree (childs_[i]);
  }
  childs_.clear();
}



SymbolSet
CTNode::childSymbols (void) const
{
  SymbolSet symbols;
  for (unsigned i = 0; i < childs_.size(); i++) {
    symbols.insert (childs_[i]->symbol());
  }
  return symbols;
}



void
CTNode::updateChildLevels (CTNode* n, unsigned level)
{
  n->setLevel (level);
  const CTNodes& childs = n->childs();
  for (unsigned i = 0; i < childs.size(); i++) {
    updateChildLevels (childs[i], level + 1);
  }
}



CTNode*
CTNode::copySubtree (const CTNode* n)
{
  CTNode* newNode = new CTNode (*n);
  const CTNodes& childs = n->childs();
  for (unsigned i = 0; i < childs.size(); i++) {
    newNode->addChild (copySubtree (childs[i]));
  }
  return newNode;
}



void
CTNode::deleteSubtree (CTNode* n)
{
  assert (n);
  const CTNodes& childs = n->childs();
  for (unsigned i = 0; i < childs.size(); i++) {
    deleteSubtree (childs[i]);
  }
  delete n;
}



ostream& operator<< (ostream &out, const CTNode& n)
{
  // out << "(" << n.level() << ") " ;
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



ConstraintTree::ConstraintTree (const LogVars& logVars,
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
  CTNode* prevNode  = root_;
  CTNodes currNodes = root_->childs();
  for (unsigned i = 0; i < tuple.size(); i++) {
    int idx = -1;
    for (unsigned j = 0; j < currNodes.size(); j++) {
      if (currNodes[j]->symbol() == tuple[i]) {
        idx = j;
        break;
      }
    }
    if (idx == -1) {
      CTNode* newNode = new CTNode (tuple[i], i + 1);
      prevNode->addChild (newNode);
      prevNode = newNode;
      currNodes.clear();
    } else {
      prevNode  = currNodes[idx];
      currNodes = currNodes[idx]->childs();
    }
  }
}



bool
ConstraintTree::containsTuple (const Tuple& tuple)
{
  queue<CTNode*> queue;
  queue.push (root_);
	
  while (queue.empty() == false) {
    CTNode* n = queue.front();
    if (n == root_ || n->symbol() == (tuple[n->level() - 1])) {
      if (n->level() == tuple.size()) {
        return true;
      } else {
         CTNodes childs = n->childs();
         for (unsigned i = 0; i < childs.size(); i++) {
           queue.push (childs[i]);
         }
      }
    }
    queue.pop();
  }
  return false;
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
ConstraintTree::join (ConstraintTree* ct, bool assertWhenNotFound)
{  
  if (logVarSet_.empty()) {
    delete root_;
    root_      = CTNode::copySubtree (ct->root());
    logVars_   = ct->logVars();
    logVarSet_ = ct->logVarSet();
    return;
  }

  LogVarSet intersect = logVarSet_ & ct->logVarSet_;
  if (intersect.empty()) {
    const CTNodes& childs = ct->root()->childs();
    CTNodes leafs = getNodesAtLevel (getLevel (logVars_.back()));
    for (unsigned i = 0; i < leafs.size(); i++) {
      for (unsigned j = 0; j < childs.size(); j++) {
        leafs[i]->addChild (CTNode::copySubtree (childs[j]));
      }
    }
    Util::addToVector (logVars_, ct->logVars_);
    logVarSet_ |= ct->logVarSet_;

  } else {
    moveToBottom (intersect.elements());
    ct->moveToTop (intersect.elements());

    unsigned level = getLevel (intersect.front());
    CTNodes nodes = getNodesAtLevel (level);
    
    Tuples tuples;
    CTNodes continNodes;
    getTuples (ct->root(),
               Tuples(),
               intersect.size(),
               tuples,
               continNodes);

    for (unsigned i = 0; i < tuples.size(); i++) {
      bool tupleFounded = false;
      for (unsigned j = 0; j < nodes.size(); j++) {
        tupleFounded |= join (nodes[j], tuples[i], 0, continNodes[i]);
      }
      if (assertWhenNotFound) {
        assert (tupleFounded);
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
  for (unsigned i = 0; i < nodes.size(); i++) {
    nodes[i]->removeAndDeleteAllChilds();
  }
  logVars_.resize (logVars_.size() - X.size());
  logVarSet_ -= X;
}



bool
ConstraintTree::ConstraintTree::isSingleton (LogVar X)
{
  SymbolSet symbols;
  CTNodes nodes = getNodesAtLevel (getLevel (X));
  for (unsigned i = 0; i < nodes.size(); i++) {
    symbols.insert (nodes[i]->symbol());
  }
  return symbols.size() == 1;
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
  for (unsigned i = 1; i < nodes.size(); i++) {
    out << "\"" << nodes[i] << "\"";
    out << " [label=\"" << *nodes[i] << "\"]" ;
    out << endl;
  }
  for (unsigned i = 0; i < nodes.size(); i++) {
    const CTNodes& childs = nodes[i]->childs();
    for (unsigned j = 0; j < childs.size(); j++) {
      out << "\"" << nodes[i] << "\"" ;
      out << " -> " ;
      out << "\"" << childs[j] << "\"" << endl ;
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
  unsigned count = countTuples (nodes[0]);
  for (unsigned i = 1; i < nodes.size(); i++) {
    if (countTuples (nodes[i]) != count) {
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
    n = n->childs()[0];
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
    for (unsigned i = 0; i < nodes.size(); i++) {
      counts.insert (countTuples (nodes[i]));
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
  assert (stopLevel > 0);
  assert (stopLevel <= logVars_.size());
  assert (stopLevel <= ct->logVars_.size());

  CTNodes commNodes;
  ConstraintTree* exclCt = new ConstraintTree (*this);
  split (exclCt->root(), ct->root(), commNodes, stopLevel);

  ConstraintTree* commCt = new ConstraintTree (logVars_);
  for (unsigned i = 0; i < commNodes.size(); i++) {
    commCt->root()->addChild (commNodes[i]);
  }
  // cout << commCt->tupleSet() << " + " ;
  // cout << exclCt->tupleSet() << " = " ;
  // cout << tupleSet() << endl << endl;
  // if (((commCt->tupleSet() | exclCt->tupleSet()) == tupleSet()) == false) {
  //   exportToGraphViz ("_fail.dot", true);
  //   commCt->exportToGraphViz ("_fail_comm.dot", true);
  //   exclCt->exportToGraphViz ("_fail_excl.dot", true);
  // }
  // assert ((commCt->tupleSet() | exclCt->tupleSet()) == tupleSet());
  // assert ((exclCt->tupleSet (stopLevel) & ct->tupleSet (stopLevel)).empty());
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
  const CTNodes& childs = root_->childs();

  for (unsigned i = 0; i < childs.size(); i++) {
    const vector<pair<CTNode*, unsigned>>& res 
        = countNormalize (childs[i], stopLevel);
    for (unsigned j = 0; j < res.size(); j++) {
      unordered_map<unsigned, ConstraintTree*>::iterator it
          = countMap.find (res[j].second);
      if (it == countMap.end()) {
        ConstraintTree* newCt = new ConstraintTree (logVars_);
        it = countMap.insert (make_pair (res[j].second, newCt)).first;
        cts.push_back (newCt);
      }
      it->second->root_->addChild (res[j].first);
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
    const CTNodes& childs = normCts2[j]->root_->childs();
    for (unsigned k = 0; k < childs.size(); k++) {
      normCts1[i]->root_->addChild (CTNode::copySubtree (childs[k]));
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
ConstraintTree::overlap (
  const ConstraintTree* ct1,
  const ConstraintTree* ct2,
  unsigned stopLevel)
{
  const CTNodes& childs1 = ct1->root_->childs();
  const CTNodes& childs2 = ct2->root_->childs();
  for (unsigned i = 0; i < childs1.size(); i++) {
    for (unsigned j = 0; j < childs2.size(); j++) {
      if (overlap (childs1[i], childs2[j], stopLevel)) {
        return true;
      }
    }
  }
  return false;
}



LogVars
ConstraintTree::expand (LogVar X)
{
  moveToBottom ({X});
  assert (isCountNormalized (X));
  CTNodes nodes = getNodesAtLevel (logVars_.size() - 1);
  unsigned nrSymbols = getConditionalCount (X);
  for (unsigned i = 0; i < nodes.size(); i++) {
    Symbols symbols;
    const CTNodes& childs = nodes[i]->childs();
    for (unsigned j = 0; j < childs.size(); j++) {
      symbols.push_back (childs[j]->symbol());
    }
    nodes[i]->removeAndDeleteAllChilds();
    CTNode* prev = nodes[i];
    assert (symbols.size() == nrSymbols);
    for (unsigned j = 0; j < nrSymbols; j++) {
      CTNode* newNode = new CTNode (symbols[j], nodes[i]->level() + j);
      prev->addChild (newNode);
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
  const CTNodes& nodes = root_->childs();
  for (unsigned i = 0; i < nodes.size(); i++) {
    CTNode* copy = CTNode::copySubtree (nodes[i]);
    copy->setSymbol (nodes[i]->symbol());
    ConstraintTree* newCt = new ConstraintTree (logVars_);
    newCt->root()->addChild (copy);
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
  const CTNodes& childs = n->childs();
  for (unsigned i = 0; i < childs.size(); i++) {
    sum += countTuples (childs[i]);
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
    for (unsigned i = 0; i < node->childs().size(); i++) {
      queue.push (node->childs()[i]);
    }
    queue.pop();
  }
  return nodes;
}



CTNodes
ConstraintTree::getNodesAtLevel (unsigned level) const
{
  assert (level <= logVars_.size());
  CTNodes nodes;
  queue<CTNode*> queue;
  queue.push (root_);
  while (queue.empty() == false) {
    CTNode* node = queue.front();
    if (node->level() == level) {
      nodes.push_back (node);
    } else {
      for (unsigned i = 0; i < node->childs().size(); i++) {
        queue.push (node->childs()[i]);
      }
    }
    queue.pop();
  }
  return nodes;
}



void
ConstraintTree::swapLogVar (LogVar X)
{
  LogVars::iterator it = 
      std::find (logVars_.begin(),logVars_.end(), X);
  assert (it != logVars_.end());
  unsigned pos = std::distance (logVars_.begin(), it);

  const CTNodes& nodes = getNodesAtLevel (pos);
  for (unsigned i = 0; i < nodes.size(); i++) {
    const CTNodes childs = nodes[i]->childs();
    for (unsigned j = 0; j < childs.size(); j++) {
       nodes[i]->removeChild (childs[j]);
       const CTNodes grandsons = childs[j]->childs();
       for (unsigned k = 0; k < grandsons.size(); k++) {
         CTNode* childCopy = new CTNode (*childs[j]);
         const CTNodes greatGrandsons = grandsons[k]->childs();
         for (unsigned t = 0; t < greatGrandsons.size(); t++) {
           grandsons[k]->removeChild (greatGrandsons[t]);
           childCopy->addChild (greatGrandsons[t], false);
         }
         childCopy->setLevel (childCopy->level() + 1); 
         grandsons[k]->addChild (childCopy, false);
         grandsons[k]->setLevel (grandsons[k]->level() - 1);
         nodes[i]->addChild (grandsons[k], false);
       }
       delete childs[j];
    }
  }

  std::swap (logVars_[pos], logVars_[pos + 1]);
}



bool
ConstraintTree::join (
    CTNode* n,
    const Tuple& tuple,
    unsigned currIdx,
    CTNode* appendNode)
{
  bool tupleFounded = false;
  if (n->symbol() == tuple[currIdx]) {
    if (currIdx == tuple.size() - 1) {
      const CTNodes& childs = appendNode->childs();
      for (unsigned i = 0; i < childs.size(); i++) {
        n->addChild (CTNode::copySubtree (childs[i]));
      }
      return true;
    }
    const CTNodes& childs = n->childs();
    for (unsigned i = 0; i < childs.size(); i++) {
      tupleFounded |= join (childs[i], tuple, currIdx + 1, appendNode);
    }
  }
  return tupleFounded;
}



bool
ConstraintTree::indenticalSubtrees (
    const CTNode* n1,
    const CTNode* n2,
    bool  compare) const
{
  if (compare) {
    if (n1->symbol() != n2->symbol()) {
      return false;
    }
  }
  const CTNodes& childs1 = n1->childs();
  const CTNodes& childs2 = n2->childs();
  if (childs1.size() != childs2.size()) {
    return false;
  }
  for (unsigned i = 0; i < childs1.size(); i++) {
    if (indenticalSubtrees (childs1[i], childs2[i], true) == false) {
      return false;
    }
  }
  return true;
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

  const CTNodes& childs = n->childs();
  for (unsigned i = 0; i < childs.size(); i++) {
    getTuples (childs[i],
               currTuples,
               stopLevel,
               tuplesCollected,
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
  const CTNodes& childs = n->childs();
  for (unsigned i = 0; i < childs.size(); i++) {
    const vector<pair<CTNode*, unsigned>>& lowerRes =
        countNormalize (childs[i], stopLevel);
    for (unsigned j = 0; j < lowerRes.size(); j++) {
      CTNode* newNode = new CTNode (*n);
      newNode->addChild (lowerRes[j].first);
      res.push_back (make_pair (newNode, lowerRes[j].second));
    }
  }
  return res;
}



void
ConstraintTree::split (
    CTNode*  n1,
    CTNode*  n2,
    CTNodes& nodes,
    unsigned stopLevel) 
{
  CTNodes& childs1 = n1->childs();
  CTNodes& childs2 = n2->childs();
  for (unsigned i = 0; i < childs1.size(); i++) {
    CTNode* intersectNode = 0;
    for (unsigned j = 0; j < childs2.size(); j++) {
      if (childs1[i]->symbol() == childs2[j]->symbol()) {
        intersectNode = childs2[j];
        break;
      }
    }
    if (intersectNode == 0) {
      continue;
    }
    if (childs1[i]->level() == stopLevel) {
      CTNode* newNode = CTNode::copySubtree (childs1[i]);
      nodes.push_back (newNode);
      childs1[i]->setSymbol (Symbol::invalid());
    } else {
      CTNodes lowerNodes;
      split (childs1[i], intersectNode, lowerNodes, stopLevel);
      if (lowerNodes.empty() == false) {
        CTNode* newNode = new CTNode (*childs1[i]);
        for (unsigned j = 0; j < lowerNodes.size(); j++) {
          newNode->addChild (lowerNodes[j]);
        }
        nodes.push_back (newNode);
      }
    }
  }

  for (int i = 0; i < (int)childs1.size(); i++) {
    if (childs1[i]->symbol() == Symbol::invalid()) {
      n1->removeAndDeleteChild (childs1[i]);
      i --;
    } else if (childs1[i]->isLeaf() && 
        childs1[i]->level() != stopLevel) {
      n1->removeAndDeleteChild (childs1[i]);
      i --;
    }
  }
}



bool
ConstraintTree::overlap (
    const CTNode* n1,
    const CTNode* n2,
    unsigned stopLevel)
{
  if (n1->isRoot() == false) {
    if (n1->level() == stopLevel) {
      return n1->symbol() == n2->symbol();
    } 
    if (n1->symbol() != n2->symbol()) {
      return false;
    }
  }
  const CTNodes& childsI = n1->childs();
  const CTNodes& childsJ = n2->childs(); 
  for (unsigned i = 0; i < childsI.size(); i++) {
    for (unsigned j = 0; j < childsJ.size(); j++) {
      if (overlap (childsI[i], childsJ[j], stopLevel)) {
        return true;
      }
    }
  }
  return false;
}

