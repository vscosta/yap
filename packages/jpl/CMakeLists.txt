
#CHECK: JavaLibs

set (JPL_SOURCES
  src/c/jpl.c)

set(TARGET_JNI 8)

if (Java_Development_FOUND)
  
include ( UseJava)

#  add_jar(target_name
#          [SOURCES] source1 [source2 ...] [resource1 ...]
#          [INCLUDE_JARS jar1 [jar2 ...]]
#          [ENTRY_POINT entry]
#          [VERSION version]
#          [OUTPUT_NAME name]
#          [OUTPUT_DIR dir]
#          )
#
# This command creates a <target_name>.jar.  It compiles the given
# source files (source) and adds the given resource files (resource) to
# the jar file.  If only resource files are given then just a jar file
# is created.  The list of include jars are added to the classpath when
# compiling the java sources and also to the dependencies of the target.
# INCLUDE_JARS also accepts other target names created by add_jar.  For
# backwards compatibility, jar files listed as sources are ignored (as
# they have been since the first version of this module).
#
# The default OUTPUT_DIR can also be changed by setting the variable
# CMAKE_JAVA_TARGET_OUTPUT_DIR.
#

  set (JPLJAVA
src/java/org/jpl7/test/Garbo.java
src/java/org/jpl7/Variable.java
src/java/org/jpl7/Version.java
src/java/org/jpl7/JPLException.java
src/java/org/jpl7/JPL.java
src/java/org/jpl7/util/Overload.java
src/java/org/jpl7/util/Getenv.java
src/java/org/jpl7/util/Prog.java
src/java/org/jpl7/util/SwingGadget.java
src/java/org/jpl7/util/Mod.java
src/java/org/jpl7/util/Test2.java
src/java/org/jpl7/util/Util2.java
src/java/org/jpl7/util/Util.java
src/java/org/jpl7/util/PopupMenuDemo.java
src/java/org/jpl7/util/Xfer.java
src/java/org/jpl7/util/SwingGadget2.java
src/java/org/jpl7/util/Test1.java
src/java/org/jpl7/util/Overload2.java
src/java/org/jpl7/Term.java
src/java/org/jpl7/Compound.java
src/java/org/jpl7/Query.java
src/java/org/jpl7/Integer.java
src/java/org/jpl7/JRef.java
src/java/org/jpl7/Float.java
src/java/org/jpl7/Atom.java
src/java/org/jpl7/Util.java
src/java/org/jpl7/PrologException.java
src/java/org/jpl7/fli/BooleanHolder.java
src/java/org/jpl7/fli/Int64Holder.java
src/java/org/jpl7/fli/module_t.java
src/java/org/jpl7/fli/IntHolder.java
src/java/org/jpl7/fli/LongHolder.java
src/java/org/jpl7/fli/qid_t.java
src/java/org/jpl7/fli/PointerHolder.java
src/java/org/jpl7/fli/engine_t.java
src/java/org/jpl7/fli/DoubleHolder.java
src/java/org/jpl7/fli/ObjectHolder.java
src/java/org/jpl7/fli/term_t.java
src/java/org/jpl7/fli/predicate_t.java
src/java/org/jpl7/fli/StringHolder.java
src/java/org/jpl7/fli/fid_t.java
src/java/org/jpl7/fli/Prolog.java
src/java/org/jpl7/fli/functor_t.java
src/java/org/jpl7/fli/atom_t.java
  )


add_jar(jpl
  SOURCES ${JPLJAVA}
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})

install_jar(jpl ${YAP_INSTALL_DATADIR} )

if (JNI_FOUND)

set(TARGET_JNI 8)

# set(CMAKE_MACOSX_RPATH 1)


add_library(jplYap src/c/jpl.c)

include_directories (${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_PATH} )

  target_link_libraries(jplYap  libYap ${JNI_LIBRARIES})

  set_target_properties(jplYap PROPERTIES
    OUTPUT_NAME jpl
    )

  add_definitions( -DJPLPJAR=${YAP_INSTALL_DATADIR}/jpl.jar)
  add_definitions( -DJNIVERSION=JNI_VERSION_1_${TARGET_JNI})

  # set(YAP_SYSTEM_OPTIONS "jpl " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
  install(TARGETS  jplYap
          LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
	  )

  endif (JNI_FOUND)


  install(FILES jpl.pl
    DESTINATION ${YAP_INSTALL_DATADIR}
    )

endif (Java_Development_FOUND)
