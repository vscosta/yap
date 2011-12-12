#ifndef HORUS_STATESINDEXER_H
#define HORUS_STATESINDEXER_H

#include <iomanip>

class StatesIndexer {
  public:

    StatesIndexer (const Ranges& ranges)
    {
      maxIndex_ = 1;
      states_.resize (ranges.size(), 0);
      ranges_ = ranges;
      for (unsigned i = 0; i < ranges.size(); i++) {
        maxIndex_ *= ranges[i];
      }
      linearIndex_ = 0;
    }


    StatesIndexer (const VarNodes& vars)
    {
      maxIndex_ = 1;
      states_.resize (vars.size(), 0);
      ranges_.reserve (vars.size());
      for (unsigned i = 0; i < vars.size(); i++) {
        ranges_.push_back (vars[i]->nrStates());
        maxIndex_ *= vars[i]->nrStates();
      }
      linearIndex_ = 0;
    }

    StatesIndexer& operator++ (void) {
      for (int i = ranges_.size() - 1; i >= 0; i--) {
        states_[i] ++;
        if (states_[i] == (int)ranges_[i]) {
          states_[i] = 0;
        } else {
          break;
        }
      }
      linearIndex_ ++;
      return *this;
    }

    StatesIndexer& operator-- (void) {
      for (int i = ranges_.size() - 1; i >= 0; i--) {
        states_[i] --;
        if (states_[i] == -1) {
          states_[i] = ranges_[i] - 1;
        } else {
          break;
        }
      }
      linearIndex_ --;
      return *this;
    }

    void incrementState (unsigned whichVar)
    {
      for (int i = whichVar; i >= 0; i--) {
        states_[i] ++;
        if (states_[i] == (int)ranges_[i] && i != 0) {
          if (i == 0) {
            linearIndex_ = maxIndex_;
          } else {
            states_[i] = 0;
          }
        } else {
          linearIndex_ = getLinearIndexFromStates();
          return;
        }
      }
    }

    void decrementState (unsigned whichVar)
    {
      for (int i = whichVar; i >= 0; i--) {
        states_[i] --;
        if (states_[i] == -1) {
          if (i == 0) {
            linearIndex_ = -1;
          } else {
            states_[i] = ranges_[i] - 1;
          }
        } else {
          linearIndex_ = getLinearIndexFromStates();
          return;
        }
      }
    }

    void nextSameState (unsigned whichVar)
    {
      for (int i = ranges_.size() - 1; i >= 0; i--) {
        if (i != (int)whichVar) {
          states_[i] ++;
          if (states_[i] == (int)ranges_[i]) {
            if (i == 0 || (i-1 == (int)whichVar && whichVar == 0)) {
              linearIndex_ = maxIndex_;
            } else {
              states_[i] = 0;
            }
          } else {
            linearIndex_ = getLinearIndexFromStates();
            return;
          }
        }
      }
    }

    void previousSameState (unsigned whichVar)
    {
      for (int i = ranges_.size() - 1; i >= 0; i--) {
        if (i != (int)whichVar) {
          states_[i] --;
          if (states_[i] == - 1) {
            if (i == 0 || (i-1 == (int)whichVar && whichVar == 0)) {
              linearIndex_ = -1;
            } else {
              states_[i] = ranges_[i] - 1;
            }
          } else {
            linearIndex_ = getLinearIndexFromStates();
            return;
          }
        }
      }
    }

    void moveToBegin (void)
    {
      std::fill (states_.begin(), states_.end(), 0);
      linearIndex_ = 0;
    }

    void moveToEnd (void)
    {
      for (unsigned i = 0; i < states_.size(); i++) {
        states_[i] = ranges_[i] - 1;
      }
      linearIndex_ = maxIndex_ - 1;
    }

    bool valid (void) const
    {
      return linearIndex_ >= 0 && linearIndex_ < (int)maxIndex_;
    }

    unsigned getLinearIndex (void) const
    {
      return linearIndex_;
    }

    const vector<int>& getStates (void) const
    {
      return states_;
    }

    unsigned operator[] (unsigned whichVar) const
    {
      assert (valid());
      assert (whichVar < states_.size());
      return states_[whichVar];
    }

    string toString (void) const
    {
      stringstream ss;
      ss << "linear index=" << setw (3) << linearIndex_ << "   " ;
      ss << "states= [" << states_[0] ;
      for (unsigned i = 1; i < states_.size(); i++) {
        ss << ", " << states_[i];
      }
      ss << "]" ;
      return ss.str();
    }

  private:
    unsigned getLinearIndexFromStates (void)
    {
      unsigned prod = 1;
      unsigned linearIndex = 0;
      for (int i = states_.size() - 1; i >= 0; i--) {
        linearIndex += states_[i] * prod;
        prod *= ranges_[i];
      }
      return linearIndex;
    }

    int                linearIndex_;
    int                maxIndex_;
    vector<int>        states_;
    vector<unsigned>   ranges_;
};


/*
  FgVarNode* v1 = new FgVarNode (0, 4);
  FgVarNode* v2 = new FgVarNode (1, 3);
  FgVarNode* v3 = new FgVarNode (2, 2);
  FgVarSet vars = {v1,v2,v3};
  ParamSet params = {
      0.2, 0.44, 0.1, 0.88, 0.22,0.62,0.32, 0.42, 0.11, 0.88, 0.8,0.5,
      0.22, 0.4, 0.11, 0.8, 0.224,0.6,0.21, 0.44, 0.14, 0.68, 0.41,0.6 
  };
  Factor f (vars,params);
  StatesIndexer idx (vars);
  while (idx.valid())
  {
    cout << idx.toString() << " p=" << params[idx.getLinearIndex()] << endl;
    idx.incrementVariableState (0);
    idx.nextSameState (1);
    ++idx;
  }
  cout << endl;
  idx.moveToEnd(); 
  while (idx.valid())
  {
    cout << idx.toString() << " p=" << params[idx.getLinearIndex()] << endl;
    idx.decrementVariableState (0);
    idx.previousSameState (1);
    --idx;
  }
*/


/*
  FgVarNode* x0 = new FgVarNode (0, 2);
  FgVarNode* x1 = new FgVarNode (1, 2);
  FgVarNode* x2 = new FgVarNode (2, 2);
  FgVarNode* x3 = new FgVarNode (2, 2);
  FgVarNode* x4 = new FgVarNode (2, 2);
  FgVarSet vars_ = {x0,x1,x2,x3,x4};
  ParamSet params_ = {
     0.2, 0.44, 0.1, 0.88, 0.11, 0.88, 0.8, 0.5,
		 0.2, 0.44, 0.1, 0.88, 0.11, 0.88, 0.8, 0.5,
		 0.2, 0.44, 0.1, 0.88, 0.11, 0.88, 0.8, 0.5,
		 0.2, 0.44, 0.1, 0.88, 0.11, 0.88, 0.8, 0.5
  };
  Factor ff (vars_,params_);
  ff.printFactor();
*/

#endif // HORUS_STATESINDEXER_H

