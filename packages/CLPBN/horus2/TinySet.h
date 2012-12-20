#ifndef HORUS_TINYSET_H
#define HORUS_TINYSET_H

#include <vector>
#include <algorithm>

using namespace std;


template <typename T, typename Compare = std::less<T>>
class TinySet
{
  public:

    typedef typename vector<T>::iterator        iterator;
    typedef typename vector<T>::const_iterator  const_iterator;

    TinySet (const TinySet& s)
        : vec_(s.vec_), cmp_(s.cmp_) { }

    TinySet (const Compare& cmp = Compare())
        : vec_(), cmp_(cmp) { }

    TinySet (const T& t, const Compare& cmp = Compare()) 
        : vec_(1, t), cmp_(cmp) { }

    TinySet (const vector<T>& elements, const Compare& cmp = Compare())
        : vec_(elements), cmp_(cmp)
    {
      std::sort (begin(), end(), cmp_);
      iterator it = unique_cmp (begin(), end());
      vec_.resize (it - begin());
    }

    iterator insert (const T& t)
    {
      iterator it = std::lower_bound (begin(), end(), t, cmp_);
      if (it == end() || cmp_(t, *it)) {
        vec_.insert (it, t);
      }
      return it;
    }

    void insert_sorted (const T& t)
    {
       vec_.push_back (t);
       assert (consistent());
    }

    void remove (const T& t)
    {
      iterator it = std::lower_bound (begin(), end(), t, cmp_);
      if (it != end()) {
        vec_.erase (it);
      }
    }

    const_iterator find (const T& t) const
    {
      const_iterator it = std::lower_bound (begin(), end(), t, cmp_);
      return it == end() || cmp_(t, *it) ? end() : it;
    }

    iterator find (const T& t)
    {
      iterator it = std::lower_bound (begin(), end(), t, cmp_);
      return it == end() || cmp_(t, *it) ? end() : it;
    }

    /* set union */
    TinySet operator| (const TinySet& s) const
    {
      TinySet res;
      std::set_union (
          vec_.begin(), vec_.end(),
          s.vec_.begin(), s.vec_.end(),
          std::back_inserter (res.vec_),
          cmp_);
      return res;
    }

    /* set intersection */
    TinySet operator& (const TinySet& s) const
    {
      TinySet res;
      std::set_intersection (
          vec_.begin(), vec_.end(),
          s.vec_.begin(), s.vec_.end(),
          std::back_inserter (res.vec_),
          cmp_);
      return res;
    }

    /* set difference */
    TinySet operator- (const TinySet& s) const
    {
      TinySet res;
      std::set_difference (
          vec_.begin(), vec_.end(),
          s.vec_.begin(), s.vec_.end(),
          std::back_inserter (res.vec_),
          cmp_);
      return res;
    }

    TinySet& operator|= (const TinySet& s)
    {
      return *this = (*this | s);
    }

    TinySet& operator&= (const TinySet& s)
    {
      return *this = (*this & s);
    }

    TinySet& operator-= (const TinySet& s)
    {
      return *this = (*this - s);
    }

    bool contains (const T& t) const
    {
      return std::binary_search (
          vec_.begin(), vec_.end(), t, cmp_);
    }

    bool contains (const TinySet& s) const
    {
      return std::includes (
          vec_.begin(),
          vec_.end(),
          s.vec_.begin(),
          s.vec_.end(),
          cmp_);
    }

    bool in (const TinySet& s) const
    {
      return std::includes (
          s.vec_.begin(),
          s.vec_.end(),
          vec_.begin(),
          vec_.end(),
          cmp_);
    }

    bool intersects (const TinySet& s) const
    {
      return (*this & s).size() > 0;
    }

    const T& operator[] (typename vector<T>::size_type i) const
    {
      return vec_[i];
    }
    
    T& operator[] (typename vector<T>::size_type i)
    {
      return vec_[i];
    }
  
    T front (void) const
    {
      return vec_.front();
    }

    T& front (void)
    {
      return vec_.front();
    }

    T back (void) const
    {
      return vec_.back();
    }

    T& back (void)
    {
      return vec_.back();
    }

    const vector<T>& elements (void) const
    {
      return vec_;
    }

    bool empty (void) const
    {
      return size() == 0;
    }

    typename vector<T>::size_type size (void) const
    {
      return vec_.size();
    }

    void clear (void)
    {
      vec_.clear();
    }

    void reserve (typename vector<T>::size_type size)
    {
      vec_.reserve (size);
    }

    iterator       begin (void)        { return vec_.begin(); }
    iterator       end   (void)        { return vec_.end();   }
    const_iterator begin (void) const  { return vec_.begin(); }
    const_iterator end   (void) const  { return vec_.end();   }

    friend bool operator== (const TinySet& s1, const TinySet& s2)
    {
      return s1.vec_ == s2.vec_;
    }

    friend bool operator!= (const TinySet& s1, const TinySet& s2)
    {
      return ! (s1.vec_ == s2.vec_);
    }

    friend std::ostream& operator << (std::ostream& out, const TinySet& s)
    {
      out << "{" ;
      typename vector<T>::size_type i;
      for (i = 0; i < s.size(); i++) {
        out << ((i != 0) ? "," : "") << s.vec_[i];
      }
      out << "}" ;
      return out;
    }

  private:
    iterator unique_cmp (iterator first, iterator last)
    {
      if (first == last) {
        return last;
      }
      iterator result = first;
      while (++first != last) {
        if (cmp_(*result, *first)) {
          *(++result) = *first;
        }
      }
      return ++result;
    }

    bool consistent (void) const
    {
      typename vector<T>::size_type i;
      for (i = 0; i < vec_.size() - 1; i++) {
        if ( ! cmp_(vec_[i], vec_[i + 1])) {
          return false;
        }
      }
      return true;
    }

    vector<T>  vec_;
    Compare    cmp_;
};

#endif // HORUS_TINYSET_H

