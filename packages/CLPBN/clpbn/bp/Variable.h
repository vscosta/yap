#ifndef BP_VARIABLE_H
#define BP_VARIABLE_H

#include <algorithm>

#include <sstream>

#include "Shared.h"

using namespace std;

class Variable
{
  public:

    Variable (const Variable* v)
    {
      vid_ = v->getVarId();
      dsize_ = v->getDomainSize();
      if (v->hasDomain()) {
        domain_ = v->getDomain();
        dsize_ = domain_.size();
      } else {
        dsize_ = v->getDomainSize();
      }
      evidence_ = v->getEvidence();
      if (v->hasLabel()) {
        label_ = new string (v->getLabel());
      } else {
        label_ = 0;
      }
    }

    Variable (Vid vid)
    {
      this->vid_       = vid;
      this->dsize_     = 0;
      this->evidence_  = NO_EVIDENCE;
      this->label_     = 0;
    }

    Variable (Vid vid, unsigned dsize, int evidence = NO_EVIDENCE,
                                       const string& lbl = string())
    {
      assert (dsize != 0);
      assert (evidence < (int)dsize);
      this->vid_       = vid;
      this->dsize_     = dsize;
      this->evidence_  = evidence;
      if (!lbl.empty()) {
        this->label_   = new string (lbl);
      } else {
        this->label_   = 0;
      }
    }

    Variable (Vid vid, const Domain& domain, int evidence = NO_EVIDENCE,
                                             const string& lbl = string())
    {
      assert (!domain.empty());
      assert (evidence < (int)domain.size());
      this->vid_       = vid;
      this->dsize_     = domain.size();
      this->domain_    = domain;
      this->evidence_  = evidence;
      if (!lbl.empty()) {
        this->label_   = new string (lbl);
      } else {
        this->label_   = 0;
      }
    }

   ~Variable (void)
    {
      delete label_;
    }

    unsigned   getVarId (void) const       { return vid_; }
    unsigned   getIndex (void) const       { return index_; }
    void       setIndex (unsigned idx)     { index_ = idx; }
    unsigned   getDomainSize (void) const  { return dsize_; }
    bool       hasEvidence (void) const    { return evidence_ != NO_EVIDENCE; }
    int        getEvidence (void) const    { return evidence_; }
    bool       hasDomain (void) const      { return !domain_.empty(); }
    bool       hasLabel (void) const       { return label_ != 0; }

    bool isValidStateIndex (int index)
    {
      return index >= 0 && index < (int)dsize_;
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
        for (unsigned i = 0; i < dsize_; i++) {
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

    void setLabel (const string& label)
    {
      label_ = new string (label);
    }

    string getLabel (void) const
    {
      if (label_ == 0) {
        stringstream ss;
        ss << "v" << vid_;
        return ss.str();
      } else {
        return *label_;
      }
    }


  private:
    DISALLOW_COPY_AND_ASSIGN (Variable);

    Vid         vid_;
    unsigned    dsize_;
    int         evidence_;
    Domain      domain_;
    string*     label_;
    unsigned    index_;

};

#endif // BP_VARIABLE_H

