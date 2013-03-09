#ifndef YAP_PACKAGES_CLPBN_HORUS_WEIGHTEDBP_H_
#define YAP_PACKAGES_CLPBN_HORUS_WEIGHTEDBP_H_

#include "BeliefProp.h"


namespace Horus {

class WeightedBp : public BeliefProp {
  public:
    WeightedBp (const FactorGraph& fg,
        const std::vector<std::vector<unsigned>>& weights);

   ~WeightedBp();

    Params getPosterioriOf (VarId);

   private:
     class WeightedLink : public BeliefProp::BpLink {
       public:
         WeightedLink (FacNode* fn, VarNode* vn, size_t idx,
             unsigned weight);

         size_t index() const { return index_; }

         unsigned weight() const { return weight_; }

         const Params& powMessage() const { return pwdMsg_; }

         void updateMessage();

       private:
         size_t    index_;
         unsigned  weight_;
         Params    pwdMsg_;

         DISALLOW_COPY_AND_ASSIGN (WeightedLink);
     };

     void createLinks();

     void maxResidualSchedule();

     void calcFactorToVarMsg (BpLink*);

     Params getVarToFactorMsg (const BpLink*);

     void printLinkInformation() const;

     std::vector<std::vector<unsigned>> weights_;

     DISALLOW_COPY_AND_ASSIGN (WeightedBp);
};




inline void
WeightedBp::WeightedLink::updateMessage()
{
  pwdMsg_ = *nextMsg_;
  swap (currMsg_, nextMsg_);
  LogAware::pow (pwdMsg_, weight_);
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_WEIGHTEDBP_H_

