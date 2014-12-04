#  Find the Readline libraries
#
#  READLINE_FOUND - system has Readline lib
#  READLINE_INCLUDE_DIR - the Readline include directory
#  READLINE_LIBRARIES - Libraries needed to use Readline
#  READLINE_HAVE_READLINE_HISTORY_H - true if readline/history.h is available

if (READLINE_INCLUDE_DIR AND READLINE_LIBRARIES)
  # Already in cache, be silent
  set(READLINE_FIND_QUIETLY TRUE)
endif (READLINE_INCLUDE_DIR AND READLINE_LIBRARIES)

find_path(READLINE_INCLUDE_DIR NAMES readline/readline.h )
find_library(READLINE_LIBRARIES NAMES readline libreadline)

find_file(READLINE_HAVE_READLINE_HISTORY_H readline/history.h)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(READLINE DEFAULT_MSG READLINE_INCLUDE_DIR READLINE_LIBRARIES)

mark_as_advanced(READLINE_INCLUDE_DIR READLINE_LIBRARIES)
