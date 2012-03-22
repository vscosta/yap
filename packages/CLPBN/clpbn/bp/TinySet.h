#ifndef HORUS_TINYSET_H
#define HORUS_TINYSET_H

#include <vector>
#include <algorithm>

using namespace std;


template <typename T>
class TinySet
{
  public:
    TinySet (void) {}

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
      typename vector<T>::iterator it =
          std::lower_bound (elements_.begin(), elements_.end(), t);
      if (it == elements_.end() || *it != t) {
        elements_.insert (it, t);
      }
    }

    void remove (const T& t)
    {
      typename vector<T>::iterator it = 
          std::lower_bound (elements_.begin(), elements_.end(), t);
      if (it != elements_.end()) {
        elements_.erase (it);
      }
    }

    /* set union */
    TinySet operator| (const TinySet& s) const
    {
      TinySet res;
      std::set_union (
          elements_.begin(),
          elements_.end(),
          s.elements_.begin(), 
          s.elements_.end(),
          std::back_inserter (res.elements_));
      return res;
    }

    /* set intersection */
    TinySet operator& (const TinySet& s) const
    {
      TinySet res;
      std::set_intersection (
          elements_.begin(),
          elements_.end(),
          s.elements_.begin(),
          s.elements_.end(),
          std::back_inserter (res.elements_));
      return res;
    }

    /* set difference */
    TinySet operator- (const TinySet& s) const
    {
      TinySet res;
      std::set_difference (
          elements_.begin(),
          elements_.end(),
          s.elements_.begin(),
          s.elements_.end(),
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


#endif // HORUS_TINYSET_H

