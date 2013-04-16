#include <sstream>
#include <iomanip>

#include "Indexer.h"


namespace Horus {

std::ostream&
operator<< (std::ostream& os, const Indexer& indexer)
{
  os << "(" ;
  os << std::setw (2) << std::setfill('0') << indexer.index_;
  os << ") " ;
  os << indexer.indices_;
  return os;
}



std::ostream&
operator<< (std::ostream &os, const MapIndexer& indexer)
{
  os << "(" ;
  os << std::setw (2) << std::setfill('0') << indexer.index_;
  os << ") " ;
  os << indexer.indices_;
  return os;
}

}  // namespace Horus

