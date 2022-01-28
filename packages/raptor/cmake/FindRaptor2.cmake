# - Find the Raptor2 RDF syntax library
# This module defines
#
#  RAPTOR2_FOUND - System has Raptor2
#  RAPTOR2_INCLUDE_DIR - The Raptor2 include directory
#  RAPTOR2_LIBRARIES - The libraries needed to use Raptor2
#  RAPTOR2_RAPPER_EXECUTABLE - Raptor2's "rapper" RDF processing tool

#=============================================================================
# Copyright 2012 Daniel Richard G. <skunk@iSKUNK.ORG>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

# use pkg-config to get the directories and then use these values
# in the FIND_PATH() and FIND_LIBRARY() calls
FIND_PACKAGE(PkgConfig)
PKG_CHECK_MODULES(PC_RAPTOR2 raptor2 QUIET)
#SET(RAPTOR2_DEFINITIONS ${PC_RAPTOR2_CFLAGS_OTHER})

FIND_PATH(RAPTOR2_INCLUDE_DIR NAMES raptor2.h
    HINTS
    ${PC_RAPTOR2_INCLUDEDIR}
    ${PC_RAPTOR2_INCLUDE_DIRS}
    PATH_SUFFIXES raptor2
)

FIND_LIBRARY(RAPTOR2_LIBRARIES NAMES raptor2 libraptor2
    HINTS
    ${PC_RAPTOR2_LIBDIR}
    ${PC_RAPTOR2_LIBRARY_DIRS}
)

FIND_PROGRAM(RAPTOR2_RAPPER_EXECUTABLE rapper)

# Handle the QUIETLY and REQUIRED arguments and set RAPTOR2_FOUND to TRUE if
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Raptor2 DEFAULT_MSG RAPTOR2_LIBRARIES RAPTOR2_INCLUDE_DIR)

MARK_AS_ADVANCED(RAPTOR2_INCLUDE_DIR RAPTOR2_LIBRARIES RAPTOR2_RAPPER_EXECUTABLE)
