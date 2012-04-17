#ifndef HORUS_TINYSET_H
#define HORUS_TINYSET_H

#include <vector>
#include <algorithm>

using namespace std;


template <typename T>
class TinySet
{
  public:
    TinySet (void) { }

    TinySet (const T& t)
    {
      elements_.push_back (t);
    }

    TinySet (const vector<T>& elements)
    {
      elements_.reserve (elements.size());
      for (unsigned i = 0; i < elements.size(); i++) {
        insert (elements[i]);
      }
    }

    TinySet (const TinySet<T>& s) : elements_(s.elements_) { }

    void insert (const T& t)
    {
      typename vector<T>::iterator it;
      it = std::lower_bound (elements_.begin(), elements_.end(), t);
      if (it == elements_.end() || *it != t) {
        elements_.insert (it, t);
      }
    }

    void remove (const T& t)
    {
      typename vector<T>::iterator it;
      it = std::lower_bound (elements_.begin(), elements_.end(), t);
      if (it != elements_.end()) {
        elements_.erase (it);
      }
    }

    /* set union */
    TinySet operator| (const TinySet& s) const
    {
      TinySet res;
      std::set_union (
          elements_.begin(), elements_.end(),
          s.elements_.begin(), s.elements_.end(),
          std::back_inserter (res.elements_));
      return res;
    }

    /* set intersection */
    TinySet operator& (const TinySet& s) const
    {
      TinySet res;
      std::set_intersection (
          elements_.begin(), elements_.end(),
          s.elements_.begin(), s.elements_.end(),
          std::back_inserter (res.elements_));
      return res;
    }

    /* set difference */
    TinySet operator- (const TinySet& s) const
    {
      TinySet res;
      std::set_difference (
          elements_.begin(), elements_.end(),
          s.elements_.begin(), s.elements_.end(),
          std::back_inserter (res.elements_));
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
          elements_.begin(), elements_.end(), t);
    }

    bool contains (const TinySet& s) const
    {
      return std::includes (
          elements_.begin(),
          elements_.end(),
          s.elements_.begin(),
          s.elements_.end());
    }

    bool in (const TinySet& s) const
    {
      return std::includes (
          s.elements_.begin(),
          s.elements_.end(),
          elements_.begin(),
          elements_.end());
    }

    bool intersects (const TinySet& s) const
    {
      return (*this & s).size() > 0;
    }

    T operator[] (unsigned i) const
    {
      return elements_[i];
    }

    const vector<T>& elements (void) const
    {
      return elements_;
    }
  
    T front (void) const
    {
      return elements_.front();
    }

    T back (void) const
    {
      return elements_.back();
    }

    unsigned size (void) const
    {
      return elements_.size();
    }

    bool empty (void) const
    {
      return elements_.size() == 0;
    }

    typedef typename std::vector<T>::const_iterator const_iterator;

    const_iterator begin (void) const
    {
      return elements_.begin();
    }

    const_iterator end (void) const
    {
      return elements_.end();
    }

    friend bool operator== (const TinySet& s1, const TinySet& s2)
    {
      return s1.elements_ == s2.elements_;
    }

    friend bool operator!= (const TinySet& s1, const TinySet& s2)
    {
      return s1.elements_ != s2.elements_;
    }

    friend std::ostream& operator << (std::ostream& out, const TinySet<T>& s)
    {
      out << "{" ;
      for (unsigned i = 0; i < s.size(); i++) {
        out << ((i != 0) ? "," : "") << s.elements()[i];
      } 
      out << "}" ;
      return out;
    }

  protected:
    vector<T> elements_;
};



template <typename T, typename Compare = std::less<T>>
class SortedVector
{
  public:
    SortedVector (const Compare& c = Compare()) : vec_(), cmp_(c) { }
    /*
    template <class InputIterator>
    SortedVector (InputIterator first, InputIterator last,
        const Compare& c = Compare()) : vec_(first, last), cmp_(c)
    {
      std::sort (begin(), end(), cmp_);
    }
    */
    typedef typename vector<T>::iterator iterator;
    typedef typename vector<T>::const_iterator const_iterator;
    iterator       begin (void)       { return vec_.begin(); }
    iterator       end (void)         { return vec_.end(); }
    const_iterator begin (void) const { return vec_.begin(); }
    const_iterator end (void)   const { return vec_.end(); }

    iterator insert (const T& t)
    {
      iterator i = std::lower_bound (begin(), end(), t, cmp_);
      if (i == end() || cmp_(t, *i))
        vec_.insert(i, t);
      return i;
    }

    void push_back (const T& t)
    {
       vec_.push_back (t);
       assert (consistent());
    }

    const_iterator find (const T& t) const
    {
      const_iterator i = std::lower_bound (begin(), end(), t, cmp_);
      return i == end() || cmp_(t, *i) ? end() : i;
    }

    iterator find (const T& t)
    {
      iterator i = std::lower_bound (begin(), end(), t, cmp_);
      return i == end() || cmp_(t, *i) ? end() : i;
    }

    const vector<T>& elements (void) { return vec_; }

    void reserve (unsigned space) { vec_.reserve (space); }

    unsigned size (void) const { return vec_.size(); }

    bool empty (void) const { return vec_.empty(); }
   
    void clear (void) { vec_.clear(); }
  
    iterator erase (iterator it) { return vec_.erase (it); }

  private:

    bool consistent (void) const
    {
      for (unsigned i = 0; i < vec_.size() - 1; i++) {
        if (cmp_(vec_[i], vec_[i+1]) == false) {
          return false;
        }
      }
      return true;
    }

    std::vector<T> vec_;
    Compare cmp_;
};


#endif // HORUS_TINYSET_H

