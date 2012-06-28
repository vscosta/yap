#ifndef HORUS_BAYESBALLGRAPH_H
#define HORUS_BAYESBALLGRAPH_H

#include <vector>
#include <queue>
#include <list>
#include <map>

#include "Var.h"
#include "Horus.h"

using namespace std;

class BBNode : public Var
{
  public:
    BBNode (Var* v) : Var (v) , visited_(false),
        markedOnTop_(false), markedOnBottom_(false) { }

    const vector<BBNode*>& childs (void) const { return childs_;  }

    vector<BBNode*>& childs (void) { return childs_;  }

    const vector<BBNode*>& parents (void) const { return parents_; }

    vector<BBNode*>& parents (void) { return parents_; }

    void addParent (BBNode* p) { parents_.push_back (p); }

    void addChild (BBNode* c) { childs_.push_back (c); }

    bool isVisited (void) const { return visited_; }
 
    void setAsVisited (void) { visited_ = true; }

    bool isMarkedOnTop (void) const { return markedOnTop_; }
 
    void markOnTop (void) { markedOnTop_ = true; }

    bool isMarkedOnBottom (void) const { return markedOnBottom_; }
 
    void markOnBottom (void) { markedOnBottom_ = true; }

    void clear (void) { visited_ = markedOnTop_ = markedOnBottom_ = false; }

  private:
    bool visited_;
    bool markedOnTop_;
    bool markedOnBottom_;

    vector<BBNode*> childs_;
    vector<BBNode*> parents_;
};


class BayesBallGraph
{
  public:
    BayesBallGraph (void) { }

    void addNode (BBNode* n);

    void addEdge (VarId vid1, VarId vid2);

    const BBNode* getNode (VarId vid) const;
 
    BBNode* getNode (VarId vid);

    bool empty (void) const { return nodes_.empty(); }

    void setIndexes (void);

    void clear (void);

    void exportToGraphViz (const char*);

  private:
    vector<BBNode*> nodes_;

    unordered_map<VarId, BBNode*> varMap_;
};

#endif // HORUS_BAYESBALLGRAPH_H

