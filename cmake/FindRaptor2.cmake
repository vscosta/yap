# - Try to find the Raptor RDF parsing library (http://librdf.org/raptor/)
# Once done this will define
#
#  Raptor2_FOUND       - System has Raptor
#  Raptor2_LIBRARIES   - Raptor and dependencies
#  Raptor2_INCLUDE_DIR - Include directory for using Raptor
#  Raptor2_DEFINITIONS - Compiler switches required for using Raptor
#
#  Capabilities
#       RAPTOR_HAVE_TRIG   - Set if raptor has TRIG

# (c) 2014 J Kyle Medley
# (c) 2007-2011 Sebastian Trueg <trueg@kde.org>
# (c) 2011 Artem Serebriyskiy <v.for.vandal@gmail.com>
# (c) 2011 Michael Jansen <kde@michael-jansen.biz>
#
# Based on FindFontconfig Copyright (c) 2006,2007 Laurent Montel, <montel@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.


include(LibFindMacros)

if ( NOT( RAPTOR_INCLUDE_DIR AND RAPTOR_LIBRARIES ) OR NOT RAPTOR_FOUND )

  include(FindLibraryWithDebug)
  include(MacroEnsureVersion)
  find_package(PkgConfig)

  # By default look for version 2.0
  if (NOT Raptor_FIND_VERSION )
    set( Raptor_FIND_VERSION "2.0")
    set( Raptor_FIND_VERSION_MAJOR "2" )
    set( Raptor_FIND_VERSION_MINOR "0" )
  endif ()

  if ( Raptor_FIND_VERSION_MAJOR EQUAL "2" )
    if ( NOT WIN32 )
      pkg_check_modules(PC_RAPTOR2 QUIET raptor2)
      if ( PC_RAPTOR2_FOUND )
        set(VRAPTOR_FOUND TRUE)
        set(RAPTOR2_LIBS "")
        set(RAPTOR2_INCLUDES "")
        foreach(lib ${PC_RAPTOR2_LIBRARIES})
          message(STATUS "lib ${lib}")
          find_library(RAPTOR2_LIBRARY_${lib} NAMES ${lib}
            HINTS ${PC_RAPTOR2_LIBDIR} ${PC_RAPTOR2_LIBRARY_DIRS})
          message(STATUS "lib2 ${RAPTOR2_LIBRARY_${lib}}")
          list(APPEND RAPTOR2_LIBS ${RAPTOR2_LIBRARY_${lib}})
          if (NOT RAPTOR2_LIBRARY_${lib}_FOUND)
            set(VRAPTOR_FOUND FALSE)
          endif()
        endforeach()
        foreach(path ${PC_RAPTOR2_INCLUDE_DIRS})
          list(APPEND RAPTOR2_INCLUDES ${path})
        endforeach()
      else()
        message(SEND_ERROR "No pkg-config info for raptor2")
      endif ()
    endif ()
  elseif ( Raptor_FIND_VERSION_MAJOR EQUAL "1" )
    message(SEND_ERROR "Raptor 1 is not supported")
  else ()
    message( SEND_ERROR "No idea how to check for version : ${Raptor_FIND_VERSION}")
  endif()

  if (RAPTOR_VERSION)
    MACRO_ENSURE_VERSION("1.4.16" ${RAPTOR_VERSION} RAPTOR_HAVE_TRIG)
  endif (RAPTOR_VERSION)

  mark_as_advanced(RAPTOR_INCLUDE_DIR RAPTOR_LIBRARIES)

endif () # Check for cached values

mark_as_advanced(RAPTOR_VERSION)

if (NOT VRAPTOR_FOUND AND Raptor_FIND_VERSION_MAJOR EQUAL "2" AND NOT Raptor_FIND_QUIET )
  pkg_check_modules(PC_RAPTOR QUIET raptor)
  if (PC_RAPTOR_FOUND)
    message( STATUS "You have raptor1 version ${PC_RAPTOR_VERSION} installed. Please update." )
  endif ()
endif ()

set(Raptor2_PROCESS_INCLUDES RAPTOR2_INCLUDES)
set(Raptor2_PROCESS_LIBS RAPTOR2_LIBS)
libfind_process(Raptor2)
