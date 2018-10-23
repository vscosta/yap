    set (PYTHONLIBS_FOUND YES CACHE BOOL "MINGW/MSYS2" FORCE )
    set (PYTHON_LIBRARY $ENV{PREFIX}/lib/libpython$ENV{PY_VER}m.$ENV{SHLIB_EXT} CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHON_LIBRARIES ${PYTHON_LIBRARY} CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHON_INCLUDE_PATH $ENV{PREFIX}/include/python$ENV{PY_VER}m CACHE PATH "MINGW/MSYS2" FORCE )
    set (PYTHON_INCLUDE_DIRS ${PYTHON_INCLUDE_PATH} CACHE PATH "MINGW/MSYS2" FORCE  )
    set (PYTHON_EXECUTABLE $ENV{PREFIX}/bin/python CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHONLIBS_VERSION_STRING $ENV{PY_VER} CACHE STRING "MINGW/MSYS2" FORCE )


    # # try to extract R from readline to avoid collision
    set(READLINE_INCLUDE_DIR $ENV{PREFIX}/include CACHE PATH "readline" FORCE)


    # Apple readline does not support readline hooks
    # So we look for another one by default
    set(READLINE_readline_LIBRARY $ENV{PREFIX}/lib/libreadline.$ENV{SHLIB_EXT} CACHE PATH "readline")

    # Sometimes readline really needs ncurses
    set(READLINE_ncurses_LIBRARY  $ENV{PREFIX}/lib/libncurses.$ENV{SHLIB_EXT} CACHE PATH "readline")

    set(READLINE_ncursesw_LIBRARY $ENV{PREFIX}/lib/libncursesw.$ENV{SHLIB_EXT} CACHE PATH "readline")

    # Sometimes ncurses really needs terminfo
    set(READLINE_tinfo_LIBRARY  $ENV{PREFIX}/lib/libntinfo.$ENV{SHLIB_EXT} CACHE PATH "readline")

    set(READLINE_tinfow_LIBRARY  $ENV{PREFIX}/lib/libntinfow.$ENV{SHLIB_EXT} CACHE PATH "readline")

    SET( READLINE_FOUND "YES" CACHE BOOL "Readline ACCESS.")


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


    set (GMP_INCLUDE_DIRS $ENV{PREFIX}/include)
    set (GMP_LIBRARIES $ENV{PREFIX}/lib/libgmp.${SHLIB_EXT})
    set (GMP_FOUND ON)
    set (GMP_LIBRARIES_DIR $ENV{PREFIX}/lib)