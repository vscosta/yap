#ifndef HORUS_INDEXER_H
#define HORUS_INDEXER_H

#include <algorithm>
#include <functional>

#include <sstream>
#include <iomanip>

#include "Util.h"


class Indexer
{
  public:
    Indexer (const Ranges& ranges, bool calcOffsets = true)
        : index_(0), indices_(ranges.size(), 0), ranges_(ranges)
    {
      size_ = std::accumulate (ranges.begin(), ranges.end(), 1,
          std::multiplies<unsigned>());
      if (calcOffsets) {
        calculateOffsets();
      }
    }

    void increment (void)
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

    void incrementDimension (size_t dim)
    {
      assert (dim < ranges_.size());
      assert (ranges_.size() == offsets_.size());
      assert (indices_[dim] < ranges_[dim]);
      indices_[dim] ++;
      index_ += offsets_[dim];
    }

    void incrementExceptDimension (size_t dim)
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

    Indexer& operator++ (void)
    {
      increment();
      return *this;
    }

    operator size_t (void) const
    {
      return index_;
    }

    unsigned operator[] (size_t dim) const
    {
      assert (valid());
      assert (dim < ranges_.size());
      return indices_[dim];
    }

    bool valid (void) const
    {
      return index_ < size_;
    }

    void reset (void)
    {
      std::fill (indices_.begin(), indices_.end(), 0);
      index_ = 0;
    }

    void resetDimension (size_t dim)
    {
      indices_[dim] = 0;
      index_ -= offsets_[dim] * ranges_[dim];
    }

    size_t size (void) const
    {
      return size_ ;
    }

    friend std::ostream& operator<< (std::ostream&, const Indexer&);

  private:
    void calculateOffsets (void)
    {
      size_t prod = 1;
      offsets_.resize (ranges_.size());
      for (size_t i = ranges_.size(); i-- > 0; ) {
        offsets_[i] = prod;
        prod *= ranges_[i];
      }
    }

    size_t          index_;
    Ranges          indices_;
    const Ranges&   ranges_;
    size_t          size_;
    vector<size_t>  offsets_;
};



inline std::ostream&
operator<< (std::ostream& os, const Indexer& indexer)
{
  os << "(" ;
  os << std::setw (2) << std::setfill('0') << indexer.index_;
  os << ") " ;
  os << indexer.indices_;
  return os;
}



class MapIndexer
{
  public:
    MapIndexer (const Ranges& ranges, const vector<bool>& mask)
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

    MapIndexer (const Ranges& ranges, size_t dim)
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
    
   template <typename T>
   MapIndexer (
        const vector<T>& allArgs,
        const Ranges&    allRanges,
        const vector<T>& wantedArgs,
        const Ranges&    wantedRanges)
     : index_(0), indices_(allArgs.size(), 0), ranges_(allRanges),
       valid_(true)
    {
      size_t prod = 1;
      vector<size_t> offsets (wantedRanges.size());
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

    MapIndexer& operator++ (void)
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

    operator size_t (void) const
    {
      assert (valid());
      return index_;
    }

    unsigned operator[] (size_t dim) const
    {
      assert (valid());
      assert (dim < ranges_.size());
      return indices_[dim];
    }

    bool valid (void) const
    {
      return valid_;
    }

    void reset (void)
    {
      std::fill (indices_.begin(), indices_.end(), 0);
      index_ = 0;
    }

    friend std::ostream& operator<< (std::ostream&, const MapIndexer&);

  private:
    size_t          index_;
    Ranges          indices_;
    const Ranges&   ranges_;
    bool            valid_;
    vector<size_t>  offsets_;
};



inline std::ostream&
operator<< (std::ostream &os, const MapIndexer& indexer)
{
  os << "(" ;
  os << std::setw (2) << std::setfill('0') << indexer.index_;
  os << ") " ;
  os << indexer.indices_;
  return os;
}


#endif // HORUS_INDEXER_H

