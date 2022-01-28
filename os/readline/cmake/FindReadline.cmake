# - Try to find readline include dirs and libraries
#
# Usage of this module as follows:
#
#     find_package(Readline)
#
# Variables defined by this module:
#
#  READLINE_FOUND            System has readline, include and lib dirs found
#  READLINE_INCLUDE_DIR      The readline include directories.
#  READLINE_LIBRARY          The readline library.

find_path(READLINE_INCLUDE_DIR NAMES readline/readline.h)
find_library(READLINE_LIBRARY NAMES readline HINTS)

if(READLINE_INCLUDE_DIR)
	file(READ "${READLINE_INCLUDE_DIR}/readline/readline.h" READLINE_VERSION_FILE)

	string(REGEX MATCH "RL_VERSION_MAJOR[\ \t]*([0-9]*)" _ ${READLINE_VERSION_FILE})
	set(READLINE_VERSION ${CMAKE_MATCH_1})

	string(REGEX MATCH "RL_VERSION_MINOR[\ \t]*([0-9]*)" _ ${READLINE_VERSION_FILE})
	set(READLINE_VERSION "${READLINE_VERSION}.${CMAKE_MATCH_1}")
endif()

# Handle the QUIETLY and REQUIRED arguments and set GRAPHVIZ_FOUND to TRUE if all listed variables are TRUE.
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Readline
	FOUND_VAR READLINE_FOUND
	REQUIRED_VARS READLINE_INCLUDE_DIR READLINE_LIBRARY
	VERSION_VAR READLINE_VERSION
	)

mark_as_advanced(READLINE_INCLUDE_DIR READLINE_LIBRARY)
