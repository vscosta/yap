#ifndef YAP_PACKAGES_CLPBN_HORUS_VAR_H_
#define YAP_PACKAGES_CLPBN_HORUS_VAR_H_

#include <cassert>

#include <unordered_map>
#include <string>

#include "Util.h"
#include "Horus.h"


namespace Horus {

class Var {
  public:
    Var (const Var*);

    Var (VarId, unsigned range, int evidence = Constants::unobserved);

    virtual ~Var() { };

    VarId varId() const { return varId_; }

    unsigned range() const { return range_; }

    int getEvidence() const  { return evidence_; }

    size_t getIndex() const { return index_; }

    void setIndex (size_t idx) { index_ = idx; }

    bool hasEvidence() const;

    operator size_t() const;

    bool operator== (const Var& var) const;

    bool operator!= (const Var& var) const;

    bool isValidState (int);

    void setEvidence (int);

    std::string label() const;

    States states() const;

    static void addVarInfo (
        VarId vid, std::string label, const States& states);

    static bool varsHaveInfo();

    static void clearVarsInfo();

  private:
    typedef std::pair<std::string, States> VarInfo;

    VarId     varId_;
    unsigned  range_;
    int       evidence_;
    size_t    index_;

    static std::unordered_map<VarId, VarInfo> varsInfo_;

    DISALLOW_COPY_AND_ASSIGN(Var);
};



inline bool
Var::hasEvidence() const
{
  return evidence_ != Constants::unobserved;
}



inline
Var::operator size_t() const
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

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_VAR_H_

