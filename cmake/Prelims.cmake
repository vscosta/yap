
#begining stuff for top CMakeLists

include (MacroLogFeature)

if (POLICY CMP0042)
    cmake_policy( SET CMP0042 NEW)
endif()

if (ANDROID)
    macro ( MY_add_custom_target)
    endmacro()
else()
    macro ( MY_add_custom_target )
        add_custom_target (${ARGN})
    endmacro()
endif()


if (ANDROID)
    macro ( add_component arg1)
        foreach(item ${ARGN})
            get_filename_component(i ${item} ABSOLUTE)
            set( ALL_SOURCES ${ALL_SOURCES} ${i} )
        endforeach()
        set( ALL_SOURCES ${ALL_SOURCES} PARENT_SCOPE )
    endmacro()
    macro ( add_external arg1)
        foreach(item ${ARGN})
            get_filename_component(i ${item} ABSOLUTE)
            set( ALL_SOURCES ${ALL_SOURCES} ${i} )
        endforeach()
        set( ALL_SOURCES ${ALL_SOURCES} PARENT_SCOPE )
    endmacro()
else()
    macro ( add_component arg1 )
        add_library ( ${arg1} OBJECT ${ARGN})
    endmacro()
    macro ( add_external arg1 )
        add_library ( ${arg1} SHARED ${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_add_dependencies)
    endmacro()
else()
    macro ( MY_add_dependencies )
        add_dependencies (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_add_library)
    endmacro()
else()
    macro ( MY_add_library )
        add_library (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_add_subdirectory)
    endmacro()
else()
    macro ( MY_add_subdirectory )
        add_subdirectory (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_include)
    endmacro()
else()
    macro ( MY_include )
        include (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_install)
    endmacro()
else()
    macro ( MY_install )
        install (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_set_target_properties)
    endmacro()
else()
    macro ( MY_set_target_properties )
        set_target_properties (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_target_link_libraries)
    endmacro()
else()
    macro ( MY_target_link_libraries )
        target_link_libraries (${ARGN})
    endmacro()
endif()


# where we have most scripts
# set path to additional CMake modules

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(disallow)

disallow_intree_builds()

# set(CMAKE_BUILD_TYPE  Debug)

if (APPLE)
    set (MACOSX_RPATH ON)
endif (APPLE )
if(POLICY CMP0042)
    cmake_policy(SET CMP0042 NEW)
endif(POLICY CMP0042)
if(POLICY CMP0043)
    cmake_policy(SET CMP0043 NEW)
endif(POLICY CMP0043)


if (ANDROID)
    set ( prefix ${YAP_APP_DIR}/build )
    set ( datarootdir ${prefix}/assets )
else()
set ( prefix "${CMAKE_INSTALL_PREFIX}")
    set ( datarootdir ${prefix}/share )
endif()
    set ( libpl  ${datarootdir}/Yap)
set ( includedir "${prefix}/include")
set ( exec_prefix "${prefix}")
set ( datadir "${datarootdir}")
set ( mandir "${datarootdir}/man")
set ( docdir "${datarootdir}/doc/Yap")
set ( dlls "${exec_prefix}/lib/Yap")

set(YAP_ROOTDIR ${prefix})

# erootdir -> rootdir
# bindir defined above
# libdir defined above
set(YAP_LIBDIR "${YAP_INSTALL_LIBDIR}")
set(YAP_SHAREDIR "${datarootdir}")
set(YAP_BINDIR "${CMAKE_INSTALL_BINDIR}")
set(YAP_INCLUDEDIR "${CMAKE_INSTALL_INCLUDEDIR}")
set(YAP_ROOTDIR "${prefix}")

#
#
# include( Sources NO_POLICY_SCOPE )
#
# include( Model NO_POLICY_SCOPE  )

include (cudd NO-POLICY-SCOPE)
include (python NO-POLICY-SCOPE)
include (java NO-POLICY-SCOPE)
