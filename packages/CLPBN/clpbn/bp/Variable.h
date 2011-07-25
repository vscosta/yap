#ifndef BP_GENERIC_VARIABLE_H
#define BP_GENERIC_VARIABLE_H

#include <sstream>

#include <algorithm>
#include "Shared.h"

using namespace std;

class Variable
{
  public:

    Variable (unsigned varId)
    {
      this->varId_      = varId;
      this->dsize_      = 0;
      this->evidence_   = -1;
      this->label_      = 0;
    }

    Variable (unsigned varId, unsigned dsize, int evidence = -1)
    {
      assert (dsize != 0);
      assert (evidence < (int)dsize);
      this->varId_      = varId;
      this->dsize_      = dsize;
      this->evidence_   = evidence;
      this->label_      = 0;
    }

    Variable (unsigned varId, const Domain& domain, int evidence = -1)
    {
      assert (!domain.empty());
      assert (evidence < (int)domain.size());
      this->varId_      = varId;
      this->dsize_      = domain.size();
      this->domain_     = domain;
      this->evidence_   = evidence;
      this->label_      = 0;
    }

   ~Variable (void)
    {
      delete label_;
    }

    unsigned       getVarId (void) const       { return varId_; }
    unsigned       getIndex (void) const       { return index_; }
    void           setIndex (unsigned idx)     { index_ = idx; }
    int            getDomainSize (void) const  { return dsize_; }
    bool           hasEvidence (void) const    { return evidence_ != -1; }
    int            getEvidence (void) const    { return evidence_; }
    bool           hasDomain (void)            { return !domain_.empty(); }
    bool           hasLabel (void)             { return label_ != 0; }

   bool isValidStateIndex (int index)
   {
     return index >= 0 && index < dsize_;
   }

    bool isValidState (const string& state)
    {
      return find (domain_.begin(), domain_.end(), state) != domain_.end();
    }

    Domain getDomain (void) const
    {
      assert (dsize_ != 0);
      if (domain_.size() == 0) {
        Domain d;
        for (int i = 0; i < dsize_; i++) {
          stringstream ss;
          ss << "x" << i ;
          d.push_back (ss.str());
        }
        return d;
      } else {
        return domain_;
      }
    }

    void setDomainSize (unsigned dsize)
    {
      assert (dsize != 0);
      dsize_ = dsize;
    }

    void setDomain (const Domain& domain)
    {
      assert (!domain.empty());
      domain_ = domain; 
      dsize_  = domain.size();
    }

    void setEvidence (int ev) 
    { 
      assert (ev < dsize_);
      evidence_ = ev;
    }

    void setEvidence (const string& ev) 
    { 
      assert (isValidState (ev));
      for (unsigned i = 0; i < domain_.size(); i++) {
        if (domain_[i] == ev) {
          evidence_ = i;
        }
      }
    }

    void setLabel (string label)
    {
      label_ = new string (label);
    }

    string getLabel (void) const
    {
      if (label_ == 0) {
        stringstream ss;
        ss << "v" << varId_;
        return ss.str();
      } else {
        return *label_;
      }
    }

  protected:
    unsigned       varId_;
    string*        label_;
    unsigned       index_;
    int            evidence_;

  private:
    DISALLOW_COPY_AND_ASSIGN (Variable);
    Domain         domain_;
    int            dsize_;

};

#endif // BP_GENERIC_VARIABLE_H

