#ifndef YAP_PACKAGES_CLPBN_HORUS_GENERICFACTOR_H_
#define YAP_PACKAGES_CLPBN_HORUS_GENERICFACTOR_H_

#include <vector>

#include "Util.h"


namespace Horus {

template <typename T>
class GenericFactor {
  public:
    const std::vector<T>& arguments() const { return args_; }

    std::vector<T>& arguments() { return args_; }

    const Ranges& ranges() const { return ranges_; }

    const Params& params() const { return params_; }

    Params& params() { return params_; }

    size_t nrArguments() const { return args_.size(); }

    size_t size() const { return params_.size(); }

    unsigned distId() const { return distId_; }

    void setDistId (unsigned id) { distId_ = id; }

    void normalize() { LogAware::normalize (params_); }

    size_t indexOf (const T& t) const { return Util::indexOf (args_, t); }

    const T& argument (size_t idx) const;

    T& argument (size_t idx);

    unsigned range (size_t idx) const;

    bool contains (const T& arg) const;

    bool contains (const std::vector<T>& args) const;

    void setParams (const Params& newParams);

    double operator[] (size_t idx) const;

    double& operator[] (size_t idx);

    GenericFactor<T>& multiply (const GenericFactor<T>& g);

    void sumOutIndex (size_t idx);

    void absorveEvidence (const T& arg, unsigned obsIdx);

    void reorderArguments (const std::vector<T>& new_args);

  protected:
    std::vector<T>  args_;
    Ranges          ranges_;
    Params          params_;
    unsigned        distId_;

  private:
    void extend (unsigned range_prod);

    void cartesianProduct (
      Params::const_iterator first2, Params::const_iterator last2);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_GENERICFACTOR_H_

