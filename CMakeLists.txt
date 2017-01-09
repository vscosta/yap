# value of 3.4.0 or lower.

# Sets the minimum version of CMake required to build the native
# library. You should either keep the default value or only pass a
# value of 3.4.0 or lower.

project( YAP )

if (ANDROID)
  set(YAP_APP_DIR "${CMAKE_SOURCE_DIR}/../..")
   cmake_policy(VERSION 3.4)

else ()
cmake_minimum_required(VERSION 2.8)
include(CMakeToolsHelpers OPTIONAL)


endif()

set(
        CMAKE_MODULE_PATH
        "${CMAKE_SOURCE_DIR}"
        "${CMAKE_SOURCE_DIR}/cmake")

include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(CMakeDependentOption)
include(MacroOptionalAddSubdirectory)
include(MacroOptionalFindPackage)
include(MacroLogFeature)
include(FindPackageHandleStandardArgs)
include (GNUInstallDirs)

# Creates and names a library, sets it as either STATIC
# or SHARED, and provides the relative paths to its source code.
# You can define multiple libraries, and CMake builds it for you.
# Gradle automatically packages shared libraries with your APK.

#cross-compilation support
# Search packages for host system instead of packages for target system
# in case of cross compilation these macro should be defined by toolchain file
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

if (ANDROID)

    ADD_SUBDIRECTORY(os)
    ADD_SUBDIRECTORY(OPTYap)
    ADD_SUBDIRECTORY(packages/myddas)
    ADD_SUBDIRECTORY(utf8proc)
    ADD_SUBDIRECTORY(CXX)


set (SWIG_FILES    ${CMAKE_SOURCE_DIR}/../generated/jni/yap_wrap.cpp )

else ()
    set(YLIBS
            $<TARGET_OBJECTS:core>
            $<TARGET_OBJECTS:libYAPOs>
            $<TARGET_OBJECTS:libOPTYap>
            $<TARGET_OBJECTS:myddas>
            $<TARGET_OBJECTS:Yapsqlite3>
            $<TARGET_OBJECTS:libswi>
            $<TARGET_OBJECTS:utf8proc>
            )


	endif ()

if (WIN32)
  list (APPEND YLIBS    $<TARGET_OBJECTS:YapC++>)
endif()

	

  add_component (core
          ${ENGINE_SOURCES}
        ${SWIG_FILES}
        ${C_INTERFACE_SOURCES}
        ${STATIC_SOURCES}
        ${ALL_SOURCES}
                                                                                                                                         )


add_library( # Sets the name of the library.
        libYap

        # Sets the library as a shared library.
        SHARED
        ${YLIBS}
        ${WINDLLS}
        )

include(libYap NO_POLICY_SCOPE)


if (USE_READLINE)
    target_link_libraries(libYap ${READLINE_LIBRARIES})
endif (USE_READLINE)


if (ANDROID)
    add_dependencies(libYap  plmyddas )

    target_link_libraries(libYap android log)

endif ()

set_target_properties(libYap
        PROPERTIES OUTPUT_NAME Yap
        )


MY_include(Packages NO_POLICY_SCOPE)
    include(Config NO_POLICY_SCOPE)
