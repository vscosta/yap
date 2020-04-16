gcmake_minimum_required(VERSION 3.5)
project(swipl-jpl-jar)

find_package(Java COMPONENTS Development)
include(UseJava)
include(JUnit)

set(CLS	org/jpl7/Atom.java
	org/jpl7/Compound.java
	org/jpl7/Float.java
	org/jpl7/Integer.java
	org/jpl7/JRef.java
	org/jpl7/JPLException.java
	org/jpl7/JPL.java
	org/jpl7/PrologException.java
	org/jpl7/Query.java
	org/jpl7/Term.java
	org/jpl7/Util.java
	org/jpl7/Variable.java
	org/jpl7/Version.java)

set(FLI	org/jpl7/fli/atom_t.java
	org/jpl7/fli/BooleanHolder.java
	org/jpl7/fli/DoubleHolder.java
	org/jpl7/fli/engine_t.java
	org/jpl7/fli/fid_t.java
	org/jpl7/fli/functor_t.java
	org/jpl7/fli/IntHolder.java
	org/jpl7/fli/Int64Holder.java
	org/jpl7/fli/LongHolder.java
	org/jpl7/fli/module_t.java
	org/jpl7/fli/ObjectHolder.java
	org/jpl7/fli/PointerHolder.java
	org/jpl7/fli/predicate_t.java
	org/jpl7/fli/Prolog.java
	org/jpl7/fli/qid_t.java
	org/jpl7/fli/StringHolder.java
	org/jpl7/fli/term_t.java)

set(TEST org/jpl7/test/Family.java
	org/jpl7/test/FetchBigTree.java
	org/jpl7/test/FetchLongList.java
	org/jpl7/test/Ga2.java
	org/jpl7/test/Ga.java
	org/jpl7/test/Garbo.java
	org/jpl7/test/Masstest.java
	org/jpl7/test/MaxObjects.java
	org/jpl7/test/ShadowA.java
	org/jpl7/test/ShadowB.java
	org/jpl7/test/SyntaxError.java
	org/jpl7/test/Test.java
	org/jpl7/test/TestJUnit.java
	org/jpl7/test/TestOLD.java)

option(JAVA_COMPATIBILITY
       "Make jpl.jar compatible with older Java versions"
       ON)
set(JAVA_COMPATIBILITY_VERSION 7 CACHE STRING
    "Java compatibility version for jpl.jar")

# The -Xlint:-options silences the build warning about boot classpath
# compatibility.

if(JAVA_COMPATIBILITY)
  set(CMAKE_JAVA_COMPILE_FLAGS
p      -source 8 -target 8 -Xlint:-options)
else()
  set(CMAKE_JAVA_COMPILE_FLAGS)
endif()

add_jar(jpl_jar
	SOURCES ${CLS} ${FLI}
	OUTPUT_NAME jpl)

if(JUNIT_JAR)
add_jar(jpltest
	SOURCES ${TEST}
	INCLUDE_JARS ${JUNIT_JAR})
endif()

install_jar(jpl_jar
	    DESTINATION ${YAP_INSTALL_DATADIR}v\ )

