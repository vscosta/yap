# Sets the minimum version of CMake required to build the native
# library. You should either keep the default value or only pass a
# value of 3.4.0 or lower.

# Sets the minimum version of CMake required to build the native
# library. You should either keep the default value or only pass a
# value of 3.4.0 or lower.

project( YAP )

cmake_minimum_required(VERSION 2.8)

set(
        CMAKE_MODULE_PATH
        "${CMAKE_SOURCE_DIR}"
        "${CMAKE_SOURCE_DIR}/cmake")

set(LIBYAP_SOURCES ../../externalNativeBuild/swig/yapj.cpp)

include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(CMakeDependentOption)
include(MacroOptionalAddSubdirectory)
include(MacroOptionalFindPackage)
include(MacroLogFeature)
include(FindPackageHandleStandardArgs)
# Creates and names a library, sets it as either STATIC
# or SHARED, and provides the relative paths to its source code.
# You can define multiple libraries, and CMake builds it for you.
# Gradle automatically packages shared libraries with your APK.


include(Prelims NO_POLICY_SCOPE)

include(Sources NO_POLICY_SCOPE)

include(Model NO_POLICY_SCOPE)

include_directories ( utf8proc packages/myddas packages/myddas/sqlite3 )
if (ANDROID)
  include_directories (  packages/myddas/sqlite3/Android/jni/sqlite/nativehelper packages/myddas/sqlite3/Android/jni/sqlite )
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

    set(ALL_SOURCES ../../externalNativeBuild/swig/yapj.cpp)

    ADD_SUBDIRECTORY(os)
    ADD_SUBDIRECTORY(OPTYap)
    ADD_SUBDIRECTORY(packages/myddas)
    ADD_SUBDIRECTORY(library/random)
    ADD_SUBDIRECTORY(library/system)
    ADD_SUBDIRECTORY(utf8proc)
    ADD_SUBDIRECTORY(CXX)

else ()
    set(YLIBS
            $<TARGET_OBJECTS:libYAPOs>
            $<TARGET_OBJECTS:libOPTYap>
            $<TARGET_OBJECTS:myddas>
            $<TARGET_OBJECTS:Yapsqlite3>
            $<TARGET_OBJECTS:libswi>
            $<TARGET_OBJECTS:utf8proc>
            )


endif ()

if (WIN32)
  list (APPEND YLIBS    $<TARGET_OBJECTS:Yap++>)

  endif()

add_library( # Sets the name of the library.
        libYap

        # Sets the library as a shared library.
        SHARED

        ${ENGINE_SOURCES}
        ${C_INTERFACE_SOURCES}
        ${STATIC_SOURCES}
        ${ALL_SOURCES}
        ${YLIBS}
        ${WINDLLS}
        )

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


    include(Config NO_POLICY_SCOPE)
MY_include(Packages NO_POLICY_SCOPE)
