#detect cudd setup, as it is shared between different installations.

find_package(CUDD)
#  CUDD_FOUND       - system has CUDD 
#  CUDD_LIBRARIES   - Link these to use CUDD
#  CUDD_INCLUDE_DIR - Include directory for using CUDD
#

macro_log_feature (CUDD_FOUND "CUDD"
  "Use CUDD Library"
  "http://vlsi.colorado.edu/~fabio/CUDD/" FALSE)



