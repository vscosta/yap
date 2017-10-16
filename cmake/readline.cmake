# - Find the readline library
# This module defines
#  READLINE_INCLUDE_DIR, path to readline/readline.h, etc.
#  READLINE_LIBRARIES, the libraries required to use READLINE.
#  READLINE_FOUND, If false, do not try to use READLINE.
# also defined, but not for general use are
# READLINE_readline_LIBRARY, where to find the READLINE library.
# , where to find the ncurses library [might not be defined]


# include subdirectories configuration
## after we have all functionality in
#
# ADD_SUBDIRECTORY(console/terminal)

option (WITH_READLINE  "GNU readline console" ON)


if (WITH_READLINE)
  find_library(READLINE_ncurses_LIBRARY
    NAMES ncurses
    HINTS ${Readline_ROOT_DIR}/lib
    )

  find_path(READLINE_INCLUDE_DIR
    NAMES readline/readline.h
    HINTS ${Readline_ROOT_DIR}/include
    )
  find_library(READLINE_readline_LIBRARY
    NAMES readline
    HINTS ${READLINE_INCLUDE_DIR/../lib} ${Readline_ROOT_DIR}/lib )
  if (READLINE_readline_LIBRARY)
    set (HAVE_LIBREADLINE TRUE)
    if (READLINE_INCLUDE_DIR)
      set(READLINE_FOUND TRUE CACHE BOOL "readline is installed correctly")
      set (READLINE_LIBRARIES ${READLINE_ncurses_LIBRARY} ${READLINE_readline_LIBRARY} )
    endif()
    message(STATUS "readline headers found at   ${READLINE_INCLUDE_DIR}")
    message(STATUS "readline library found at   ${READLINE_readline_LIBRARY} ${READLINE_ncurses_LIBRARY} ")
  endif ()
  set(YAP_SYSTEM_OPTIONS "readline" ${YAP_SYSTEM_OPTIONS} )


  set(EXTRALIBS ${EXTRALIBS} readline)
endif ()

