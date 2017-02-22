set(YAP_FOUND ON)

set(YAP_MAJOR_VERSION 6)
set(YAP_MINOR_VERSION 3)
set(YAP_PATCH_VERSION 5)

set(YAP_FULL_VERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_FVERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_NUMERIC_VERSION
  ${YAP_MAJOR_VERSION}*10000+${YAP_MINOR_VERSION}*100+${YAP_PATCH_VERSION})
set(MYDDAS_VERSION MYDDAS-0.9.1)

site_name( YAP_SITE )


if (WIN32)
  set (YAP_ARCH $ENV{PROCESSOR_ARCHITECTURE})
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  "MS_WIN64=1" )
  set(YAP_SYSTEM_OPTIONS "windows " ${YAP_SYSTEM_OPTIONS})
endif()
if (UNIX)
  find_program (UNAME uname)
  execute_process (
    COMMAND ${UNAME} -m
    OUTPUT_VARIABLE YAP_ARCH OUTPUT_STRIP_TRAILING_WHITESPACE )
    set(YAP_SYSTEM_OPTIONS "unix " ${YAP_SYSTEM_OPTIONS})
endif()


#
set ( MIN_STACKSPACE 1024*SIZEOF_INT_P )
set ( MIN_HEAPSPACE 32*1024*SIZEOF_INT_P )
set ( MIN_TRAILSPACE 512*SIZEOF_INT_P )
set ( DEF_STACKSPACE 0 )
set ( DEF_HEAPSPACE 0 )
set ( DEF_TRAILSPACE 0 )

# option (RATIONAL_TREES "support infinite rational trees" ON)
# dd_definitions (-D)

## don't touch these opts
  set_property( DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS  DEPTH_LIMIT=1;COROUTINING=1;RATIONAL_TREES=1 )

# inform we are compiling YAP
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  "_YAP_NOT_INSTALLED_=1;HAVE_CONFIG_H=1;_GNU_SOURCE" )

# Compilation model
#  target_compile_definitions(libYap PUBLIC  _XOPEN_SOURCE=700 )

#add_definitions( -Wall  -Wstrict-prototypes -Wmissing-prototypes)

# Model Specific
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS   $<$<CONFIG:Debug>:DEBUG=1> )

#ensure cells are properly aligned in code
set (ALIGN_LONGS 1)

#ensure best access to slots in environments
set (MSHIFTOFFS 1)

set (C_COMPILER CMAKE_C_COMPILER_ID)

if ( ${C_COMPILER} MATCHES "GNU")
  set (HAVE_GCC 1)
endif()

# compatible compilers
if ( ${C_COMPILER} MATCHES "Clang")
  set (HAVE_GCC 1)
endif()

if ( ${C_COMPILER} MATCHES "Intel")
  set (HAVE_GCC 1)
endif()

# Model Specific
if (HAVE_GCC)
  set_property( DIRECTORY APPEND PROPERTY COMPILE_OPTIONS  -Wall )
if ( ${C_COMPILER} MATCHES "GNU")
  set_property( DIRECTORY APPEND PROPERTY COMPILE_OPTIONS  $<$<CONFIG:Release>:-O3;-fomit-frame-pointer;-fstrict-aliasing;-freorder-blocks;-fsched-interblock> )
else()
   set_property( DIRECTORY APPEND PROPERTY COMPILE_OPTIONS  $<$<CONFIG:Release>:-O3;-fstrict-aliasing;-freorder-blocks;-fsched-interblock> )
endif()
  set_property( DIRECTORY APPEND  PROPERTY  COMPILE_OPTIONS  -fexceptions )
endif()

# set_property( DIRECTORY APPEND_STRING PROPERTY -fsanitize=memory;-fsanitize-memory-track-origins=2)
if (HAVE_GCC)
  # replace instructions codes by the address of their code
  option (WITH_THREADED_CODE "threaded code" ON)
  if (WITH_THREADED_CODE)
    set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  THREADED_CODE=1)
  endif (WITH_THREADED_CODE)
endif (HAVE_GCC)

#
#option (YAP_SWI_IO ON)

OPTION (WITH_CALL_TRACER
"support for procedure-call tracing" ON)
#TODO:
if (WITH_CALL_TRACER)
    list (APPEND YAP_SYSTEM_OPTIONS "call_tracer " ${YAP_SYSTEM_OPTIONS})
set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS $<$<CONFIG:Debug>:LOW_LEVEL_TRACER=1> )
endif (WITH_CALL_TRACER)

set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  UTF8PROC=1)



