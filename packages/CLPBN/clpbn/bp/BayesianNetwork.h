#ifndef BAYESIAN_NETWORK_H
#define BAYESIAN_NETWORK_H

#include <vector>
#include <string>

using namespace std;

class BayesianNode;
class Distribution;

class BayesianNetwork
{
  public:
    // constructs
    BayesianNetwork (void);
    // destruct
    virtual ~BayesianNetwork (void);
    // methods
    virtual void           addNode (string, vector<BayesianNode*>, int, int);
    virtual void           addNode (string, vector<BayesianNode*>,
                                    double*, int, vector<string>);
    BayesianNode*          getNode (string) const;
    void                   addDistribution (int, double*, int, vector<string>);
    vector<BayesianNode*>  getNodes (void) const;
    vector<BayesianNode*>  getRootNodes (void) const;
    vector<BayesianNode*>  getLeafNodes (void) const;
    bool                   isPolyTree (void) const;
    void                   printNetwork (void) const;

  protected:
    // members
    vector<BayesianNode*>  nodes_;
    vector<Distribution*>  dists_;

  private:
    BayesianNetwork (const BayesianNetwork&);  // disallow copy
    void operator= (const BayesianNetwork&);   // disallow assign
    bool                   containsCycle (void) const;
    bool                   containsCycle (int, int, vector<bool>&) const;
    int                    getIndexOf (const BayesianNode*) const;
    vector<int>            getAdjacentVertexes (int) const ;
};

#endif // BAYESIAN_NETWORK_H

