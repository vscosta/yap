set(LIBJIT_MAJOR_VERSION 0)
set(LIBJIT_MINOR_VERSION 1)
set(LIBJIT_PATCH_VERSION 0)
set(LIBJIT_FULL_VERSION
${LIBJIT_MAJOR_VERSION}.${LIBJIT_MINOR_VERSION}.${LIBJIT_PATCH_VERSION})

set(LIBJIT_SOURCES
  jit_analysispreds.c
  jit_configpreds.c
  jit_statisticpreds.c
  jit_codegenpreds.c
  jit_debugpreds.c
  jit_traced.c
  jit_transformpreds.c
  JIT_Compiler.cpp
  JIT_Init.cpp
)

set(LIBJIT_HEADERS
  HPP/JIT.hpp
  HPP/JIT_Compiler.hpp
  HPP/jit_predicates.hpp
  ../OPTYap/traced_or.insts.h
  ../OPTYap/traced_tab.insts.h
  ../OPTYap/traced_tab.tries.insts.h
  ../C/traced_absmi_insts.h
)

# The following variables are defined:
#  LLVM_FOUND          - true if LLVM was found
#  LLVM_CXXFLAGS       - C++ compiler flags for files that include LLVM headers.
#  LLVM_HOST_TARGET    - Target triple used to configure LLVM.
#  LLVM_INCLUDE_DIRS   - Directory containing LLVM include files.
#  LLVM_LDFLAGS        - Linker flags to add when linking against LLVM
#                        (includes -LLLVM_LIBRARY_DIRS).
#  LLVM_LIBRARIES      - Full paths to the library files to link against.
#  LLVM_LIBRARY_DIRS   - Directory containing LLVM libraries.
#  LLVM_ROOT_DIR       - The root directory of the LLVM installation.
#                        llvm-config is searched for in ${LLVM_ROOT_DIR}/bin.
#  LLVM_VERSION_MAJOR  - Major version of LLVM.
#  LLVM_VERSION_MINOR  - Minor version of LLVM.
#  LLVM_VERSION_STRING - Full LLVM version string (e.g. 2.9).
#
# Note: The variable names were chosen in conformance with the offical CMake
# guidelines, see ${CMAKE_ROOT}/Modules/readme.txt.

# Try suffixed versions to pick up the newest LLVM install available on Debian
# derivatives.
# We also want an user-specified LLVM_ROOT_DIR to take precedence over the
# system default locations such as /usr/local/bin. Executing find_program()
# multiples times is the approach recommended in the docs.

macro_optional_find_package (LLVM OFF)

set (YAP_JIT  ${LLVM_FOUND} SCOPE GLOBAL)

set (POSITION_INDEPENDENT_CODE TRUE)

set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} ${LLVM_CXXFLAGS})

add_lib (libyapjit
  ${LIBJIT_SOURCES}
  ${LIBJIT_HEADERS}
)

set_target_properties(libyapjit
  PROPERTIES
  SOVERSION ${LIBJIT_MAJOR_VERSION}.${LIBJIT_MINOR_VERSION}
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME YapJIT
)

target_link_libraries(libyapjit libYap ${LLVM_LIBRARIES})

set ( YAP_YAPJITLIB $<TARGET_FILE_NAME:libyapjit> )
