#ifndef YAP_PACKAGES_CLPBN_HORUS_TINYSET_H_
#define YAP_PACKAGES_CLPBN_HORUS_TINYSET_H_

#include <vector>
#include <algorithm>
#include <ostream>


namespace Horus {

template <typename T, typename Compare = std::less<T>>
class TinySet {
  public:
    typedef typename std::vector<T>::iterator       iterator;
    typedef typename std::vector<T>::const_iterator const_iterator;

    TinySet (const TinySet& s)
        : vec_(s.vec_), cmp_(s.cmp_) { }

    TinySet (const Compare& cmp = Compare())
        : vec_(), cmp_(cmp) { }

    TinySet (const T& t, const Compare& cmp = Compare())
        : vec_(1, t), cmp_(cmp) { }

    TinySet (const std::vector<T>& elements, const Compare& cmp = Compare());

    iterator insert (const T& t);

    void insert_sorted (const T& t);

    void remove (const T& t);

    const_iterator find (const T& t) const;

    iterator find (const T& t);
    
    /* set union */
    TinySet operator| (const TinySet& s) const;

    /* set intersection */
    TinySet operator& (const TinySet& s) const;

    /* set difference */
    TinySet operator- (const TinySet& s) const;

    TinySet& operator|= (const TinySet& s);

    TinySet& operator&= (const TinySet& s);

    TinySet& operator-= (const TinySet& s);

    bool contains (const T& t) const;

    bool contains (const TinySet& s) const;

    bool in (const TinySet& s) const;

    bool intersects (const TinySet& s) const;

    const T& operator[] (typename std::vector<T>::size_type i) const;

    T& operator[] (typename std::vector<T>::size_type i);

    T front() const;

    T& front();

    T back() const;

    T& back();

    const std::vector<T>& elements() const;

    bool empty() const;

    typename std::vector<T>::size_type size() const;

    void clear();

    void reserve (typename std::vector<T>::size_type size);

    iterator       begin()        { return vec_.begin(); }
    iterator       end  ()        { return vec_.end();   }
    const_iterator begin() const  { return vec_.begin(); }
    const_iterator end  () const  { return vec_.end();   }

  private:
    iterator unique_cmp (iterator first, iterator last);

    bool consistent() const;

    friend bool operator== (const TinySet& s1, const TinySet& s2)
    {
      return s1.vec_ == s2.vec_;
    }

    friend bool operator!= (const TinySet& s1, const TinySet& s2)
    {
      return ! (s1.vec_ == s2.vec_);
    }

    friend std::ostream& operator<< (std::ostream& out, const TinySet& s)
    {
      out << "{" ;
      typename std::vector<T>::size_type i;
      for (i = 0; i < s.size(); i++) {
        out << ((i != 0) ? "," : "") << s.vec_[i];
      }
      out << "}" ;
      return out;
    }

    std::vector<T>  vec_;
    Compare         cmp_;
};



template <typename T, typename C> inline
TinySet<T,C>::TinySet (const std::vector<T>& elements, const C& cmp)
    : vec_(elements), cmp_(cmp)
{
  std::sort (begin(), end(), cmp_);
  iterator it = unique_cmp (begin(), end());
  vec_.resize (it - begin());
}



template <typename T, typename C> inline typename TinySet<T,C>::iterator
TinySet<T,C>::insert (const T& t)
{
  iterator it = std::lower_bound (begin(), end(), t, cmp_);
  if (it == end() || cmp_(t, *it)) {
    vec_.insert (it, t);
  }
  return it;
}



template <typename T, typename C> inline void
TinySet<T,C>::insert_sorted (const T& t)
{
   vec_.push_back (t);
   assert (consistent());
}



template <typename T, typename C> inline void
TinySet<T,C>::remove (const T& t)
{
  iterator it = std::lower_bound (begin(), end(), t, cmp_);
  if (it != end()) {
    vec_.erase (it);
  }
}



template <typename T, typename C> inline typename TinySet<T,C>::const_iterator
TinySet<T,C>::find (const T& t) const
{
  const_iterator it = std::lower_bound (begin(), end(), t, cmp_);
  return it == end() || cmp_(t, *it) ? end() : it;
}



template <typename T, typename C> inline typename TinySet<T,C>::iterator
TinySet<T,C>::find (const T& t)
{
  iterator it = std::lower_bound (begin(), end(), t, cmp_);
  return it == end() || cmp_(t, *it) ? end() : it;
}



/* set union */
template <typename T, typename C> inline TinySet<T,C>
TinySet<T,C>::operator| (const TinySet& s) const
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
template <typename T, typename C> inline TinySet<T,C>
TinySet<T,C>::operator& (const TinySet& s) const
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
template <typename T, typename C> inline TinySet<T,C>
TinySet<T,C>::operator- (const TinySet& s) const
{
  TinySet res;
  std::set_difference (
      vec_.begin(), vec_.end(),
      s.vec_.begin(), s.vec_.end(),
      std::back_inserter (res.vec_),
      cmp_);
  return res;
}



template <typename T, typename C> inline TinySet<T,C>&
TinySet<T,C>::operator|= (const TinySet& s)
{
  return *this = (*this | s);
}



template <typename T, typename C> inline TinySet<T,C>&
TinySet<T,C>::operator&= (const TinySet& s)
{
  return *this = (*this & s);
}



template <typename T, typename C> inline TinySet<T,C>&
TinySet<T,C>::operator-= (const TinySet& s)
{
  return *this = (*this - s);
}



template <typename T, typename C> inline bool
TinySet<T,C>::contains (const T& t) const
{
  return std::binary_search (
      vec_.begin(), vec_.end(), t, cmp_);
}



template <typename T, typename C> inline bool
TinySet<T,C>::contains (const TinySet& s) const
{
  return std::includes (
      vec_.begin(), vec_.end(),
      s.vec_.begin(), s.vec_.end(),
      cmp_);
}



template <typename T, typename C> inline bool
TinySet<T,C>::in (const TinySet& s) const
{
  return std::includes (
      s.vec_.begin(), s.vec_.end(),
      vec_.begin(), vec_.end(),
      cmp_);
}



template <typename T, typename C> inline bool
TinySet<T,C>::intersects (const TinySet& s) const
{
  return (*this & s).size() > 0;
}



template <typename T, typename C> inline const T&
TinySet<T,C>::operator[] (typename std::vector<T>::size_type i) const
{
  return vec_[i];
}



template <typename T, typename C> inline T&
TinySet<T,C>::operator[] (typename std::vector<T>::size_type i)
{
  return vec_[i];
}



template <typename T, typename C> inline T
TinySet<T,C>::front() const
{
  return vec_.front();
}



template <typename T, typename C> inline T&
TinySet<T,C>::front()
{
  return vec_.front();
}



template <typename T, typename C> inline T
TinySet<T,C>::back() const
{
  return vec_.back();
}



template <typename T, typename C> inline T&
TinySet<T,C>::back()
{
  return vec_.back();
}



template <typename T, typename C> inline const std::vector<T>&
TinySet<T,C>::elements() const
{
  return vec_;
}



template <typename T, typename C> inline bool
TinySet<T,C>::empty() const
{
  return vec_.empty();
}



template <typename T, typename C> inline typename std::vector<T>::size_type
TinySet<T,C>::size() const
{
  return vec_.size();
}



template <typename T, typename C> inline void
TinySet<T,C>::clear()
{
  vec_.clear();
}



template <typename T, typename C> inline void
TinySet<T,C>::reserve (typename std::vector<T>::size_type size)
{
  vec_.reserve (size);
}



template <typename T, typename C> typename TinySet<T,C>::iterator
TinySet<T,C>::unique_cmp (iterator first, iterator last)
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



template <typename T, typename C> inline bool
TinySet<T,C>::consistent() const
{
  typename std::vector<T>::size_type i;
  for (i = 0; i < vec_.size() - 1; i++) {
    if ( ! cmp_(vec_[i], vec_[i + 1])) {
      return false;
    }
  }
  return true;
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_TINYSET_H_

