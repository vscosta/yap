# - Find the readline library
# This module defines
#  READLINE_INCLUDE_DIR, path to readline/readline.h, etc.
#  READLINE_LIBRARIES, the libraries required to use READLINE.
#  READLINE_FOUND, If false, do not try to use READLINE.
# also defined, but not for general use are
# READLINE_readline_LIBRARY, where to find the READLINE library.
# READLINE_ncurses_LIBRARY, where to find the ncurses library [might not be defined]


# Apple readline does not support readline hooks
# So we look for another one by default
#
# # try to extract R from readline to avoid collision
FIND_PATH(READLINE_INCLUDE_DIR NAMES readline/readline.h)


# Apple readline does not support readline hooks
# So we look for another one by default
FIND_LIBRARY(READLINE_readline_LIBRARY NAMES readline)

# Sometimes readline really needs ncurses
FIND_LIBRARY(READLINE_ncurses_LIBRARY NAMES ncurses)

FIND_LIBRARY(READLINE_ncursesw_LIBRARY NAMES ncursesw)

# Sometimes ncurses really needs terminfo
FIND_LIBRARY(READLINE_tinfo_LIBRARY NAMES tinfo)

FIND_LIBRARY(READLINE_tinfow_LIBRARY NAMES tinfow)

MARK_AS_ADVANCED(
  READLINE_INCLUDE_DIR
  READLINE_readline_LIBRARY
  READLINE_ncurses_LIBRARY
  READLINE_tinfo_LIBRARY
  READLINE_ncursesw_LIBRARY
  READLINE_tinfow_LIBRARY
  )


    message(STATUS "readline headers found at   ${READLINE_INCLUDE_DIR}")
    message(STATUS "readline library found at   ${READLINE_readline_LIBRARY} ${READLINE_ncurses_LIBRARY} ${READLINE_tinfo_LIBRARY} ${READLINE_ncursesw_LIBRARY} ${READLINE_tinfow_LIBRARY} ")

set(HAVE_LIBREADLINE FALSE CACHE BOOL "Readline works.")
    SET( READLINE_FOUND "NO" )
IF(READLINE_INCLUDE_DIR)
  IF(READLINE_readline_LIBRARY)
    SET( READLINE_FOUND "YES" )
  MESSAGE(STATUS "Found readline library")
  set(HAVE_LIBREADLINE CACHE YES BOOL "Readline works." )
   SET( READLINE_LIBRARIES
      ${READLINE_readline_LIBRARY}
      )

    # some readline libraries depend on ncurses
    IF(READLINE_ncurses_LIBRARY)
      list(APPEND READLINE_LIBRARIES ${READLINE_ncurses_LIBRARY})
    endif ()

    # some readline libraries depend on ncurses
    IF(READLINE_ncursesw_LIBRARY)
      list(APPEND READLINE_LIBRARIES ${READLINE_ncursesw_LIBRARY})
    endif ()

    # some readline libraries depend on tinfo
    IF(READLINE_tinfo_LIBRARY)
      list(APPEND READLINE_LIBRARIES ${READLINE_tinfo_LIBRARY})
    endif ()

    # some readline libraries depend on tinfo
    IF(READLINE_tinfow_LIBRARY)
      list(APPEND READLINE_LIBRARIES ${READLINE_tinfow_LIBRARY})
    endif ()

  ENDIF(READLINE_readline_LIBRARY)
ENDIF(READLINE_INCLUDE_DIR)


IF(NOT READLINE_FOUND)
 IF(READLINE_FIND_REQUIRED)
    MESSAGE(SYSTEM_ERROR_FATAL "Could not find readline -- please give some paths to CMake")
  ENDIF(READLINE_FIND_REQUIRED)
ENDIF(NOT READLINE_FOUND)

find_package_handle_standard_args(Readline READLINE_INCLUDE_DIR READLINE_LIBRARIES  READLINE_readline_LIBRARY READLINE_ncurses_LIBRARY   READLINE_tinfo_LIBRARY)


MARK_AS_ADVANCED(
  READLINE_FOUND
  HAVE_LIBREADLINE
  )
