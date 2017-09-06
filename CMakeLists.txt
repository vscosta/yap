# value of 3.4.0 or lower.
cmake_minimum_required(VERSION 3.4)

  if(COMMAND cmake_policy)
      cmake_policy(SET CMP0003 NEW)
    endif(COMMAND cmake_policy)

# Sets the version of CMake required to build the native
# library. You should either keep the default value or  pass a
# value of 3.4.0 or lower.
include(CMakeToolsHelpers OPTIONAL)

project( YAP )

include(FeatureSummary)

set(YAP_APP_DIR "${CMAKE_SOURCE_DIR}/../..")
   cmake_policy(VERSION 3.4)

set(
  CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}"
  "${CMAKE_SOURCE_DIR}/cmake"
  )

include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(CMakeDependentOption)
include(MacroOptionalAddSubdirectory)
include(MacroOptionalFindPackage)
include(FeatureSummary)
include(FindPackageHandleStandardArgs)
include (GNUInstallDirs)

# Creates and names a library, sets it as either STATIC
# or SHARED, and provides the relative paths to its source code.z
# You can define  libraries, and CMake builds it for you.
# Gradle automatically packages shared libraries with your APK.

#cross-compilation support
# Search packages for host system instead of packages for target system
# in case of cross compilation define these macro by toolchain file
if(NOT COMMAND find_host_package)
    macro(find_host_package)
        find_package(${ARGN})
    endmacro()
endif()
if(NOT COMMAND find_host_program)
    macro(find_host_program)
        find_program(${ARGN})
    endmacro()
endif()

option(BUILD_SHARED_LIBS "Build shared library" ON)
set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)
set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

include(Prelims NO_POLICY_SCOPE)

include(Sources NO_POLICY_SCOPE)

include(Model NO_POLICY_SCOPE)


include_directories ( utf8proc packages/myddas packages/myddas/sqlite3/src)

if (ANDROID)
    include_directories (
            packages/myddas/sqlite3/src/Android/jni/sqlite
            packages/myddas/sqlite3/src/Android/jni/sqlite/nativehelper
    )
endif (ANDROID)

set_property( SOURCE ${LIBYAP_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1)

add_definitions(-DUSE_MYDDAS=1 -DMYDDAS_SQLITE3=1)

if (MYSQL_FOUND)
    add_definitions(= -DMYDDAS_MYSQL=1)
endif()

if (ODBC_FOUND)
    add_definitions(= -DMYDDAS_ODBC=1)
endif()

if (MYSQL_POSTGRES)
    add_definitions(= -DMYDDAS_POSTGRES=1)
endif()


OPTION (WITH_SWIG " Enable SWIG interfaces to foreign languages"  ON)

IF (WITH_SWIG OR ANDROID)
    find_host_package (SWIG)
    # macro_log_feature (SWIG_FOUND "Swig"
    #         "Use SWIG Interface Generator "
    #         "http://www.swig.org" ON)
ENDIF (WITH_SWIG OR ANDROID)

option (WITH_PYTHON
        "Allow Python->YAP  and YAP->Python" ON)

IF (WITH_PYTHON)
    include(python NO_POLICY_SCOPE)
ENDIF (WITH_PYTHON)



    List (APPEND YLIBS    $<TARGET_OBJECTS:libOPTYap> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:libYAPOs> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:utf8proc> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:myddas> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:libswi> )
    if (WIN32)
        List (APPEND YLIBS    $<TARGET_OBJECTS:Yapsqlite3> )
        List (APPEND YLIBS    $<TARGET_OBJECTS:YAP++> )
        if (WITH_PYTHON )
            List (APPEND YLIBS    $<TARGET_OBJECTS:Py4YAP> )
        endif()
    endif()




add_lib( # Sets the name of the library.
        libYap

        # Sets the library as a shared library.
        SHARED

        ${ENGINE_SOURCES}
        ${C_INTERFACE_SOURCES}
        //${STATIC_SOURCES}
        # cmake object libraries
        ${YLIBS}
        )

if (WIN32)
    target_link_libraries(libYap ${WINDLLS})
    if (PYTHON_INCLUDE_DIRS AND PYTHON_LIBRARIES )
        target_link_libraries(libYap ${PYTHON_LIBRARIES})
    endif()
endif (WIN32)

include(libYap NO_POLICY_SCOPE)

if (USE_READLINE)
    target_link_libraries(libYap ${READLINE_LIBRARIES})
endif (USE_READLINE)

if (ANDROID)
    target_link_libraries(libYap android log)

endif ()

set_target_properties(libYap
      PROPERTIES OUTPUT_NAME Yap
  )

MY_include(Packages NO_POLICY_SCOPE)


if (PYTHONLIBS_FOUND AND SWIG_FOUND)
    add_subdirectory(packages/python/swig)

    include(FindPythonModule)

    find_python_module( jupyter )

    if (PY_JUPYTER)
        add_subdirectory(packages/python/yap_kernel)
    ENDIF()
endif()


IF (SWIG_FOUND OR ANDROID)
    add_subDIRECTORY (packages/swig  NO_POLICY_SCOPE)
ENDIF()

include(Config NO_POLICY_SCOPE)

feature_summary(WHAT ENABLED_FEATURES DISABLED_FEATURES
                INCLUDE_QUIET_PACKAGES
)
