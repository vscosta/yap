## after we have all functionality in
#
# ADD_SUBDIRECTORY(console/terminal)

option (WITH_READLINE  "GNU readline console" ON)


if (WITH_READLINE)
      find_library(READLINE_tinfo_LIBRARY
        NAMES tinfo
     message(STATUS "tinfo DLL found at   ${READLINE_tinfo_LIBRARY}")
       HINTS ${READLINE_ROOT_DIR}/lib
        )
      find_library(READLINE_tinfow_LIBRARY
        NAMES tinfow
        HINTS ${READLINE_ROOT_DIR}/lib
        )
  find_library(READLINE_ncurses_LIBRARY
essage(STATUS "tinfo DLL found at   ${READLINE_tinfo_LIBRARY}")
       HINTS ${READLINE_ROOT_DIR}/lib
        )
      find_library(READLINE_tinfow_LIBRARY
        NAMES tinfow
        HINTS ${READLINE_ROOT_DIR}/lib
        )
  find_library(READLINE_ncurses_LIBRARY
    NAMES ncurses
    HINTS ${READLINE_ROOT_DIR}/lib
    message(STATUS "readline ncurses DLL found at   ${READLINE_ncurses_LIBRARY}")
    )
    find_library(READLINE_ncursesw_LIBRARY
      NAMES ncursesw
    message(STATUS "readline ncursesw DLL found at   ${READLINE_ncursesw_LIBRARY}")
      HINTS ${READLINE_ROOT_DIR}/lib
      )

  find_path(READLINE_INCLUDE_DIR
    NAMES readline/readline.h
    HINTS ${READLINE_ROOT_DIR}/include
    )
  find_library(READLINE_readline_LIBRARY
    NAMES readline
    HINTS ${READLINE_INCLUDE_DIR/../lib} ${READLINE_ROOT_DIR}/lib 
  if (READLINE_readline_LIBRARY)
    set (HAVE_LIBREADLINE TRUE)
    if (READLINE_INCLUDE_DIR)
      set(READLINE_FOUND TRUE CACHE BOOL "readline is installed correctly")
      set (READLINE_LIBRARIES ${READLINE_ncursesw_LIBRARY} ${READLINE_tinfow_LIBRARY}$ ${READLINE_tinfo_LIBRARY}$ ${READLINE_ncurses_LIBRARY} ${READLINE_readline_LIBRARY} )
    endif()
    message(STATUS "headers found so far at   ${READLINE_INCLUDE_DIR}")
    message(STATUS "libraries found so far at   ${READLINE_readline_LIBRARY} ${READLINE_ncurses_LIBRARY} ${READLINE_tinfo_LIBRARY} ${READLINE_tinfow_LIBRARY} ${READLINE_ncursesw_LIBRARY} ")
    else
  endif ()
  set(YAP_SYSTEM_OPTIONS "readline" ${YAP_SYSTEM_OPTIONS} )


  set(EXTRALIBS ${EXTRALIBS} readline)
endif ()
