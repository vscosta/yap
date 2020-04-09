#!/bin/sh

# set variables for Prolog
. ../swipl.sh < /dev/null

if [ ! -z "$LD_LIBRARY_PATH" ]; then
  LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
fi

if [ ! -z "$LD_PRELOAD" ]; then
  export LD_PRELOAD="$JAVA_PRELOAD $LD_PRELOAD"
else
  export LD_PRELOAD="$JAVA_PRELOAD"
fi

if [ -z "$JUNIT" -a -r /usr/share/java/junit.jar ]; then
  JUNIT=/usr/share/java/junit.jar
fi

if [ -z "$JAVA" ]; then
  JAVA=java
fi

export CLASSPATH=$JUNIT:jpl.jar:jpltest.jar

$JAVA junit.textui.TestRunner jpl.test.TestJUnit
