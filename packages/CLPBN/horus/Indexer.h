#ifndef HORUS_STATESINDEXER_H
#define HORUS_STATESINDEXER_H

#include <algorithm>
#include <numeric>
#include <functional>

#include <sstream>
#include <iomanip>

#include "Var.h"
#include "Util.h"



class StatesIndexer
{
  public:

    StatesIndexer (const Ranges& ranges, bool calcOffsets = true)
    {
      li_  = 0;
      size_ = std::accumulate (ranges.begin(), ranges.end(), 0);
      indices_.resize (ranges.size(), 0);
      ranges_ = ranges;
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
      li_ ++;
    }

    void increment (size_t dim)
    {
      assert (dim < ranges_.size());
      assert (ranges_.size() == offsets_.size());
      assert (indices_[dim] < ranges_[dim]);
      indices_[dim] ++;
      li_ += offsets_[dim];
    }

    void incrementExcluding (size_t skipDim)
    {
      assert (ranges_.size() == offsets_.size());
      for (size_t i = ranges_.size(); i-- > 0; ) {
        if (i != (int)skipDim) {
          indices_[i] ++;
          li_ += offsets_[i];
          if (indices_[i] != ranges_[i]) {
            return;
          } else {
            indices_[i] = 0;
            li_ -= offsets_[i] * ranges_[i];
          }
        }
      }
      li_ = size_;
    }

    size_t linearIndex (void) const
    {
      return li_;
    }

    const vector<unsigned>& indices (void) const
    {
      return indices_;
    }

    StatesIndexer& operator ++ (void)
    {
      increment();
      return *this;
    }

    operator size_t (void) const
    {
      return li_;
    }

    unsigned operator[] (size_t dim) const
    {
      assert (valid());
      assert (dim < ranges_.size());
      return indices_[dim];
    }

    bool valid (void) const
    {
      return li_ < size_;
    }

    void reset (void)
    {
      std::fill (indices_.begin(), indices_.end(), 0);
      li_ = 0;
    }

    void reset (size_t dim)
    {
      indices_[dim] = 0;
      li_ -= offsets_[dim] * ranges_[dim];
    }

    size_t size (void) const
    {
      return size_ ;
    }

    friend ostream& operator<< (ostream &os, const StatesIndexer& idx)
    {
      os << "(" << std::setw (2) << std::setfill('0') << idx.li_ << ") " ;
      os << idx.indices_;
      return os;
    }

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

    size_t            li_;
    size_t            size_;
    vector<unsigned>  indices_;
    vector<unsigned>  ranges_;
    vector<size_t>    offsets_;
};



class MapIndexer
{
  public:
    MapIndexer (const Ranges& ranges, const vector<bool>& mapDims)
    {
      assert (ranges.size() == mapDims.size());
      size_t prod = 1;
      offsets_.resize (ranges.size());
      for (size_t i = ranges.size(); i-- > 0; ) {
        if (mapDims[i]) {
          offsets_[i] = prod;
          prod *= ranges[i];
        }
      }
      indices_.resize (ranges.size(), 0);
      ranges_ = ranges;
      index_ = 0;
      valid_ = true;
    }

    MapIndexer (const Ranges& ranges, size_t ignoreDim)
    {
      size_t prod = 1;
      offsets_.resize (ranges.size());
      for (size_t i = ranges.size(); i-- > 0; ) {
        if (i != ignoreDim) {
          offsets_[i] = prod;
          prod *= ranges[i];
        }
      }
      indices_.resize (ranges.size(), 0);
      ranges_ = ranges;
      index_ = 0;
      valid_ = true;
    }
    
    /*
    MapIndexer (
        const VarIds& loopVids,
        const Ranges& loopRanges,
        const VarIds& mapVids,
        const Ranges& mapRanges)
    {
      unsigned prod = 1;
      vector<unsigned> offsets (mapRanges.size());
      for (size_t i = mapRanges.size(); i-- > 0; ) {
        offsets[i] = prod;
        prod *= mapRanges[i];
      }

      offsets_.reserve (loopVids.size());
      for (size_t i = 0; i < loopVids.size(); i++) {
        VarIds::const_iterator it =
            std::find (mapVids.begin(), mapVids.end(), loopVids[i]);
        if (it != mapVids.end()) {
          offsets_.push_back (offsets[it - mapVids.begin()]);
        } else {
          offsets_.push_back (0);
        }
      }

      indices_.resize (loopVids.size(), 0);
      ranges_ = loopRanges;
      index_ = 0;
      size_ = prod;
    }
    */

    MapIndexer& operator ++ (void)
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

    size_t mappedIndex (void) const
    {
      return index_;
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
      return valid_;
    }

    void reset (void)
    {
      std::fill (indices_.begin(), indices_.end(), 0);
      index_ = 0;
    }

    friend ostream& operator<< (ostream &os, const MapIndexer& idx)
    {
      os << "(" << std::setw (2) << std::setfill('0') << idx.index_ << ") " ;
      os << idx.indices_;
      return os;
    }

  private:
    size_t            index_;
    bool              valid_;
    vector<unsigned>  ranges_;
    vector<unsigned>  indices_;
    vector<size_t>    offsets_;
};


#endif // HORUS_STATESINDEXER_H

