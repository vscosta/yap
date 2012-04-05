#ifndef HORUS_VARNODE_H
#define HORUS_VARNODE_H

#include <cassert>

#include <iostream>

#include "Horus.h"

using namespace std;

class VarNode
{
  public:
    VarNode (const VarNode*);

    VarNode (VarId, unsigned, int = Constants::NO_EVIDENCE);

    virtual ~VarNode (void) { };

    unsigned varId (void) const { return varId_; }

    unsigned range (void) const { return range_; }

    int getEvidence (void) const  { return evidence_; }

    unsigned getIndex (void) const { return index_; }

    void setIndex (unsigned idx) { index_ = idx; }

    operator unsigned () const { return index_; }

    bool hasEvidence (void) const
    {
      return evidence_ != Constants::NO_EVIDENCE;
    }

    bool operator== (const VarNode& var) const
    {
      assert (!(varId_ == var.varId() && range_ != var.range()));
      return varId_ == var.varId();
    }

    bool operator!= (const VarNode& var) const
    {
      assert (!(varId_ == var.varId() && range_ != var.range()));
      return varId_ != var.varId();
    }

    bool isValidState (int);

    bool isValidState (const string&);

    void setEvidence (int);

    void setEvidence (const string&);

    string label (void) const;

    States states (void) const;

  private:
    VarId     varId_;
    unsigned  range_;
    int       evidence_;
    unsigned  index_;

};

#endif // BP_VARNODE_H

