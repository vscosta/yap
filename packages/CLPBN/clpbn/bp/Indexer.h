#ifndef HORUS_STATESINDEXER_H
#define HORUS_STATESINDEXER_H

#include <algorithm>
#include <numeric>
#include <functional>

#include <sstream>
#include <iomanip>

#include "VarNode.h"
#include "Util.h"


class StatesIndexer {
  public:

    StatesIndexer (const Ranges& ranges, bool calcOffsets = true)
    {
      size_ = 1;
      indices_.resize (ranges.size(), 0);
      ranges_ = ranges;
      for (unsigned i = 0; i < ranges.size(); i++) {
        size_ *= ranges[i];
      }
      li_ = 0;
      if (calcOffsets) {
        calculateOffsets();
      }
    }

    StatesIndexer (const VarNodes& vars, bool calcOffsets = true)
    {
      size_ = 1;
      indices_.resize (vars.size(), 0);
      ranges_.reserve (vars.size());
      for (unsigned i = 0; i < vars.size(); i++) {
        ranges_.push_back (vars[i]->nrStates());
        size_ *= vars[i]->nrStates();
      }
      li_ = 0;
      if (calcOffsets) {
        calculateOffsets();
      }
    }

    void increment (void)
    {
      for (int i = ranges_.size() - 1; i >= 0; i--) {
        indices_[i] ++;
        if (indices_[i] != ranges_[i]) {
          break;
        } else {
          indices_[i] = 0;
        }
      }
      li_ ++;
    }

    void increment (unsigned dim)
    {
      assert (dim < ranges_.size());
      assert (ranges_.size() == offsets_.size());
      assert (indices_[dim] < ranges_[dim]);
      indices_[dim] ++;
      li_ += offsets_[dim];
    }

    void incrementExcluding (unsigned skipDim)
    {
      assert (ranges_.size() == offsets_.size());
      for (int i = ranges_.size() - 1; i >= 0; i--) {
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

    unsigned linearIndex (void) const
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

    operator unsigned (void) const
    {
      return li_;
    }

    unsigned operator[] (unsigned dim) const
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

    void reset (unsigned dim)
    {
      indices_[dim] = 0;
      li_ -= offsets_[dim] * ranges_[dim];
    }

    unsigned size (void) const
    {
      return size_ ;
    }

    friend ostream& operator<< (ostream &out, const StatesIndexer& idx)
    {
      out << "(" << std::setw (2) << std::setfill('0') << idx.li_ << ") " ;
      out << idx.indices_;
      return out;
    }

  private:
    void calculateOffsets (void)
    {
      unsigned prod = 1;
      offsets_.resize (ranges_.size());
      for (int i = ranges_.size() - 1; i >= 0; i--) {
        offsets_[i] = prod;
        prod *= ranges_[i];
      }
    }

    unsigned          li_;
    unsigned          size_;
    vector<unsigned>  indices_;
    vector<unsigned>  ranges_;
    vector<unsigned>  offsets_;
};



class MapIndexer
{
  public:
    MapIndexer (const Ranges& ranges, const vector<bool>& mapDims)
    {
      assert (ranges.size() == mapDims.size());
      unsigned prod = 1;
      offsets_.resize (ranges.size());
      for (int i = ranges.size() - 1; i >= 0; i--) {
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

    MapIndexer (const Ranges& ranges, unsigned ignoreDim)
    {
      unsigned prod = 1;
      offsets_.resize (ranges.size());
      for (int i = ranges.size() - 1; i >= 0; i--) {
        if (i != (int)ignoreDim) {
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
      for (int i = mapRanges.size() - 1; i >= 0; i--) {
        offsets[i] = prod;
        prod *= mapRanges[i];
      }

      offsets_.reserve (loopVids.size());
      for (unsigned i = 0; i < loopVids.size(); i++) {
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
      for (int i = ranges_.size() - 1; i >= 0; i--) {
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

    unsigned mappedIndex (void) const
    {
      return index_;
    }

    operator unsigned (void) const
    {
      return index_;
    }

    unsigned operator[] (unsigned dim) const
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

    friend ostream& operator<< (ostream &out, const MapIndexer& idx)
    {
      out << "(" << std::setw (2) << std::setfill('0') << idx.index_ << ") " ;
      out << idx.indices_;
      return out;
    }

  private:
    MapIndexer (const Ranges& ranges) : 
        ranges_(ranges),
        indices_(ranges.size(), 0),
        offsets_(ranges.size())
    {
      index_ = 0;
    }
    unsigned          index_;
    bool              valid_;
    vector<unsigned>  ranges_;
    vector<unsigned>  indices_;
    vector<unsigned>  offsets_;
};


#endif // HORUS_STATESINDEXER_H

