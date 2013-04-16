#ifndef YAP_PACKAGES_CLPBN_HORUS_FACTOR_H_
#define YAP_PACKAGES_CLPBN_HORUS_FACTOR_H_

#include <cassert>

#include <vector>
#include <string>

#include "GenericFactor.h"
#include "Util.h"


namespace Horus {

class Factor : public GenericFactor<VarId> {
  public:
    Factor() { }

    Factor (const VarIds&, const Ranges&, const Params&,
        unsigned = Util::maxUnsigned());

    Factor (const Vars&, const Params&,
        unsigned = Util::maxUnsigned());

    void sumOut (VarId);

    void sumOutAllExcept (VarId);

    void sumOutAllExcept (const VarIds&);

    void sumOutAllExceptIndex (size_t idx);

    Factor& multiply (const Factor&);

    std::string getLabel() const;

    void print() const;

  private:
    void sumOutFirstVariable();

    void sumOutLastVariable();

    void sumOutArgs (const std::vector<bool>& mask);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_FACTOR_H_

