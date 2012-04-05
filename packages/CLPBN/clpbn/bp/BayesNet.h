#ifndef HORUS_BAYESNET_H
#define HORUS_BAYESNET_H

#include <vector>
#include <queue>
#include <list>
#include <map>

#include "GraphicalModel.h"
#include "BayesNode.h"
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



class BayesNet : public GraphicalModel
{
  public:
    BayesNet (void) { };

   ~BayesNet (void);

    void readFromBifFormat (const char*);

    BayesNode* addNode (BayesNode*);

    BayesNode* addNode (string, const States&);

    BayesNode* getBayesNode (VarId) const;

    BayesNode* getBayesNode (string) const;

    VarNode* getVariableNode (VarId) const;

    VarNodes getVariableNodes (void) const;

    const BnNodeSet& getBayesNodes (void) const;

    unsigned nrNodes (void) const;

    BnNodeSet getRootNodes (void) const;

    BnNodeSet getLeafNodes (void) const;

    bool isPolyTree (void) const;

    void setIndexes (void);

    void printGraphicalModel (void) const;

    void exportToGraphViz (const char*, bool = true, 
        const VarIds& = VarIds()) const;

    void exportToBifFormat (const char*) const;

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNet);

    bool containsUndirectedCycle (void) const;

    bool containsUndirectedCycle (int, int, vector<bool>&)const;

    vector<int> getAdjacentNodes (int) const;

    Params reorderParameters (const Params&, unsigned) const;

    Params revertParameterReorder (const Params&, unsigned) const;

    BnNodeSet nodes_;

    typedef unordered_map<unsigned, unsigned> IndexMap;
    IndexMap  varMap_;
};

#endif // HORUS_BAYESNET_H
