#ifndef HORUS_BAYESNET_H
#define HORUS_BAYESNET_H

#include <vector>
#include <queue>
#include <list>
#include <map>

#include "GraphicalModel.h"
#include "Horus.h"


using namespace std;


class VarNode;

class DAGraphNode : public VarNode
{
  public:
    DAGraphNode (VarNode* vn) : VarNode (vn) , visited_(false),
        markedOnTop_(false), markedOnBottom_(false) { }

    const vector<DAGraphNode*>& childs (void) const { return childs_;  }

    vector<DAGraphNode*>& childs  (void) { return childs_;  }

    const vector<DAGraphNode*>& parents (void) const { return parents_; }

    vector<DAGraphNode*>& parents (void) { return parents_; }

    void addParent (DAGraphNode* p) { parents_.push_back (p); }

    void addChild (DAGraphNode* c) { childs_.push_back (c); }

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

    vector<DAGraphNode*> childs_;
    vector<DAGraphNode*> parents_;
};


class DAGraph
{
  public:
    DAGraph (void) { }

    void addNode (DAGraphNode* n);

    void addEdge (VarId vid1, VarId vid2);

    const DAGraphNode* getNode (VarId vid) const;
 
    DAGraphNode* getNode (VarId vid);

    bool empty (void) const { return nodes_.empty(); }

    void setIndexes (void);

    void clear (void);

    void exportToGraphViz (const char*);

  private:
    vector<DAGraphNode*> nodes_;

    unordered_map<VarId, DAGraphNode*> varMap_;
};


#endif // HORUS_BAYESNET_H
