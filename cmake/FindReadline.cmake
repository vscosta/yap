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

FIND_LIBRARY(READLINE_readline_LIBRARY NAMES readline)

# Sometimes readline really needs ncurses
FIND_LIBRARY(READLINE_ncurses_LIBRARY NAMES ncurses)

FIND_LIBRARY(READLINE_ncursesw_LIBRARY NAMES ncursesw)

# Sometimes ncurses really needs terminfo
FIND_LIBRARY(READLINE_tinfo_LIBRARY NAMES tinfo)

FIND_LIBRARY(READLINE_tinfow_LIBRARY NAMES tinfow)



# Apple readline does not support readline hooks
# So we look for another one by default
if ( READLINE_readline_LIBRARY)
    set (HAVE_LIBREADLINE YES CACHE BOOL "ibReadline ACCESS")
endif()

IF(READLINE_readline_LIBRARY)
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

  IF(READLINE_INCLUDE_DIR)
      SET( READLINE_FOUND "YES" CACHE BOOL "Readline ACCESS.")
  ENDIF(READLINE_INCLUDE_DIR)
  ENDIF(READLINE_readline_LIBRARY)


message(STATUS "readline found:   ${READLINE_FOUND}")

if (READLINE_FOUND)
message(STATUS "readline headers found at   ${READLINE_INCLUDE_DIR}")
message(STATUS "readline library found at   ${READLINE_LIBRARIES}")
endif()
