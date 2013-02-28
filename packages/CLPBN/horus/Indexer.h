#ifndef YAP_PACKAGES_CLPBN_HORUS_INDEXER_H_
#define YAP_PACKAGES_CLPBN_HORUS_INDEXER_H_

#include <vector>
#include <algorithm>
#include <numeric>

#include "Util.h"


namespace Horus {

class Indexer {
  public:
    Indexer (const Ranges& ranges, bool calcOffsets = true);

    void increment();

    void incrementDimension (size_t dim);

    void incrementExceptDimension (size_t dim);

    Indexer& operator++();

    operator size_t() const;

    unsigned operator[] (size_t dim) const;

    bool valid() const;

    void reset();

    void resetDimension (size_t dim);

    size_t size() const;

  private:
    void calculateOffsets();

    friend std::ostream& operator<< (std::ostream&, const Indexer&);

    size_t               index_;
    Ranges               indices_;
    const Ranges&        ranges_;
    size_t               size_;
    std::vector<size_t>  offsets_;

    DISALLOW_COPY_AND_ASSIGN (Indexer);
};



inline
Indexer::Indexer (const Ranges& ranges, bool calcOffsets)
    : index_(0), indices_(ranges.size(), 0), ranges_(ranges),
      size_(Util::sizeExpected (ranges))
{
  if (calcOffsets) {
    calculateOffsets();
  }
}



inline void
Indexer::increment()
{
  for (size_t i = ranges_.size(); i-- > 0; ) {
    indices_[i] ++;
    if (indices_[i] != ranges_[i]) {
      break;
    } else {
      indices_[i] = 0;
    }
  }
  index_ ++;
}



inline void
Indexer::incrementDimension (size_t dim)
{
  assert (dim < ranges_.size());
  assert (ranges_.size() == offsets_.size());
  assert (indices_[dim] < ranges_[dim]);
  indices_[dim] ++;
  index_ += offsets_[dim];
}



inline void
Indexer::incrementExceptDimension (size_t dim)
{
  assert (ranges_.size() == offsets_.size());
  for (size_t i = ranges_.size(); i-- > 0; ) {
    if (i != dim) {
      indices_[i] ++;
      index_ += offsets_[i];
      if (indices_[i] != ranges_[i]) {
        return;
      } else {
        indices_[i] = 0;
        index_ -= offsets_[i] * ranges_[i];
      }
    }
  }
  index_ = size_;
}



inline Indexer&
Indexer::operator++()
{
  increment();
  return *this;
}



inline
Indexer::operator size_t() const
{
  return index_;
}



inline unsigned
Indexer::operator[] (size_t dim) const
{
  assert (valid());
  assert (dim < ranges_.size());
  return indices_[dim];
}



inline bool
Indexer::valid() const
{
  return index_ < size_;
}



inline void
Indexer::reset()
{
  index_ = 0;
  std::fill (indices_.begin(), indices_.end(), 0);
}



inline  void
Indexer::resetDimension (size_t dim)
{
  indices_[dim] = 0;
  index_ -= offsets_[dim] * ranges_[dim];
}



inline size_t
Indexer::size() const
{
  return size_ ;
}



inline void
Indexer::calculateOffsets()
{
  size_t prod = 1;
  offsets_.resize (ranges_.size());
  for (size_t i = ranges_.size(); i-- > 0; ) {
    offsets_[i] = prod;
    prod *= ranges_[i];
  }
}



class MapIndexer {
  public:
    MapIndexer (const Ranges& ranges, const std::vector<bool>& mask);

    MapIndexer (const Ranges& ranges, size_t dim);

    template <typename T>
    MapIndexer (
        const std::vector<T>& allArgs,
        const Ranges& allRanges,
        const std::vector<T>& wantedArgs,
        const Ranges& wantedRanges);

    MapIndexer& operator++();

    operator size_t() const;

    unsigned operator[] (size_t dim) const;

    bool valid() const;

    void reset();

  private:
    friend std::ostream& operator<< (std::ostream&, const MapIndexer&);

    size_t               index_;
    Ranges               indices_;
    const Ranges&        ranges_;
    bool                 valid_;
    std::vector<size_t>  offsets_;

    DISALLOW_COPY_AND_ASSIGN (MapIndexer);
};



inline
MapIndexer::MapIndexer (
    const Ranges& ranges,
    const std::vector<bool>& mask)
    : index_(0), indices_(ranges.size(), 0), ranges_(ranges),
      valid_(true)
{
  size_t prod = 1;
  offsets_.resize (ranges.size(), 0);
  for (size_t i = ranges.size(); i-- > 0; ) {
    if (mask[i]) {
      offsets_[i] = prod;
      prod *= ranges[i];
    }
  }
  assert (ranges.size() == mask.size());
}



inline
MapIndexer::MapIndexer (const Ranges& ranges, size_t dim)
    : index_(0), indices_(ranges.size(), 0), ranges_(ranges),
      valid_(true)
{
  size_t prod = 1;
  offsets_.resize (ranges.size(), 0);
  for (size_t i = ranges.size(); i-- > 0; ) {
    if (i != dim) {
      offsets_[i] = prod;
      prod *= ranges[i];
    }
  }
}



template <typename T> inline
MapIndexer::MapIndexer (
    const std::vector<T>& allArgs,
    const Ranges& allRanges,
    const std::vector<T>& wantedArgs,
    const Ranges& wantedRanges)
    : index_(0), indices_(allArgs.size(), 0), ranges_(allRanges),
      valid_(true)
{
  size_t prod = 1;
  std::vector<size_t> offsets (wantedRanges.size());
  for (size_t i = wantedRanges.size(); i-- > 0; ) {
    offsets[i] = prod;
    prod *= wantedRanges[i];
  }
  offsets_.reserve (allArgs.size());
  for (size_t i = 0; i < allArgs.size(); i++) {
    size_t idx = Util::indexOf (wantedArgs, allArgs[i]);
    offsets_.push_back (idx != wantedArgs.size() ? offsets[idx] : 0);
  }
}



inline MapIndexer&
MapIndexer::operator++()
{
  assert (valid_);
  for (size_t i = ranges_.size(); i-- > 0; ) {
    indices_[i] ++;
    index_ += offsets_[i];
    if (indices_[i] != ranges_[i]) {
      return *this;
    } else {
      indices_[i] = 0;
      index_ -= offsets_[i] * ranges_[i];
    }
  }
  valid_ = false;
  return *this;
}



inline
MapIndexer::operator size_t() const
{
  assert (valid());
  return index_;
}



inline unsigned
MapIndexer::operator[] (size_t dim) const
{
  assert (valid());
  assert (dim < ranges_.size());
  return indices_[dim];
}



inline bool
MapIndexer::valid() const
{
  return valid_;
}



inline void
MapIndexer::reset()
{
  index_ = 0;
  std::fill (indices_.begin(), indices_.end(), 0);
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_INDEXER_H_

