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

  set (CLS
    jpl/Atom.java
  jpl/Compound.java
  jpl/Float.java
  jpl/Integer.java
  jpl/JRef.java
  jpl/JPLException.java
  jpl/JPL.java
  jpl/PrologException.java
  jpl/Query.java
  jpl/Term.java
  jpl/Util.java
  jpl/Variable.java
  jpl/Version.java
  )

set (FLI
  jpl/fli/atom_t.java
  jpl/fli/BooleanHolder.java
  jpl/fli/DoubleHolder.java
  jpl/fli/engine_t.java
  jpl/fli/fid_t.java
  jpl/fli/functor_t.java
  jpl/fli/IntHolder.java
  jpl/fli/Int64Holder.java
  jpl/fli/LongHolder.java
  jpl/fli/module_t.java
  jpl/fli/ObjectHolder.java
  jpl/fli/PointerHolder.java
  jpl/fli/predicate_t.java
  jpl/fli/Prolog.java
  jpl/fli/qid_t.java
  jpl/fli/StringHolder.java
  jpl/fli/term_t.java
  )

set (TEST
  jpl/test/Family.java
  jpl/test/FetchBigTree.java
  jpl/test/FetchLongList.java
  jpl/test/Ga2.java
  jpl/test/Ga.java
  jpl/test/Garbo.java
  jpl/test/Masstest.java
  jpl/test/MaxObjects.java
  jpl/test/ShadowA.java
  jpl/test/ShadowB.java
  jpl/test/SyntaxError.java
  jpl/test/Test.java
  jpl/test/TestJUnit.java
  jpl/test/TestOLD.java
  )

set ( JPLJAVA ${CLS}  ${FLI} )

add_jar(jpl
  SOURCES ${JPLJAVA}
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})

install_jar(jpl ${libpl} )
