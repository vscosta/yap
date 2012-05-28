#ifndef HORUS_VAR_H
#define HORUS_VAR_H

#include <cassert>

#include <iostream>

#include "Util.h"
#include "Horus.h"


using namespace std;


struct VarInfo
{
  VarInfo (string l, const States& sts) : label(l), states(sts) { }
  string label;
  States states;
};



class Var
{
  public:
    Var (const Var*);

    Var (VarId, unsigned, int = Constants::NO_EVIDENCE);

    virtual ~Var (void) { };

    VarId varId (void) const { return varId_; }

    unsigned range (void) const { return range_; }

    int getEvidence (void) const  { return evidence_; }

    size_t getIndex (void) const { return index_; }

    void setIndex (size_t idx) { index_ = idx; }

    bool hasEvidence (void) const
    {
      return evidence_ != Constants::NO_EVIDENCE;
    }

    operator size_t (void) const { return index_; }

    bool operator== (const Var& var) const
    {
      assert (!(varId_ == var.varId() && range_ != var.range()));
      return varId_ == var.varId();
    }

    bool operator!= (const Var& var) const
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

    static void addVarInfo (
        VarId vid, string label, const States& states)
    {
      assert (Util::contains (varsInfo_, vid) == false);
      varsInfo_.insert (make_pair (vid, VarInfo (label, states)));
    }

    static VarInfo getVarInfo (VarId vid)
    {
      assert (Util::contains (varsInfo_, vid));
      return varsInfo_.find (vid)->second;
    }

    static bool varsHaveInfo (void)
    {
      return varsInfo_.size() != 0;
    }

    static void clearVarsInfo (void)
    {
      varsInfo_.clear();
    }

  private:
    VarId     varId_;
    unsigned  range_;
    int       evidence_;
    size_t    index_;

    static unordered_map<VarId, VarInfo> varsInfo_;

};

#endif // HORUS_VAR_H

