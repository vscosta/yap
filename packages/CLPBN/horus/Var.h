#ifndef HORUS_VAR_H
#define HORUS_VAR_H

#include <cassert>

#include "Util.h"
#include "Horus.h"


using namespace std;


struct VarInfo
{
  VarInfo (string l, const States& sts)
      : label(l), states(sts) { }
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

    bool hasEvidence (void) const;

    operator size_t (void) const;

    bool operator== (const Var& var) const;

    bool operator!= (const Var& var) const;

    bool isValidState (int);

    void setEvidence (int);

    string label (void) const;

    States states (void) const;

    static void addVarInfo (
        VarId vid, string label, const States& states);

    static VarInfo getVarInfo (VarId vid);

    static bool varsHaveInfo (void);

    static void clearVarsInfo (void);

  private:
    VarId     varId_;
    unsigned  range_;
    int       evidence_;
    size_t    index_;

    static unordered_map<VarId, VarInfo> varsInfo_;
};



inline bool
Var::hasEvidence (void) const
{
  return evidence_ != Constants::NO_EVIDENCE;
}



inline
Var::operator size_t (void) const
{
  return index_;
}



inline bool
Var::operator== (const Var& var) const
{
  assert (!(varId_ == var.varId() && range_ != var.range()));
  return varId_ == var.varId();
}



inline bool
Var::operator!= (const Var& var) const
{
  return !(*this == var);
}


#endif // HORUS_VAR_H

