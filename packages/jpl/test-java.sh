#!/bin/sh

# set variables for Prolog
. ../swipl.sh < /dev/null

findexe()
{ oldifs="$IFS"
  IFS=:
  for d in $PATH; do
    if [ -x $d/$1 ]; then
       IFS="$oldifs"
       return 0
    fi
  done
  IFS="$oldifs"
  return 1
}

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

if findexe java && [ -r $JUNIT ]; then
  export CLASSPATH=$JUNIT:jpl.jar:jpltest.jar

  $JAVA junit.textui.TestRunner org.jpl7.test.TestJUnit
else
  echo "Warning: cannot find java or junit.jar; skipping tests"
  echo "Warning: that embed SWI-Prolog in Java"
fi
