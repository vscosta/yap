#!/bin/bash

# run all examples. Note that the   Exceptions* examples are expected to
# throw exceptions.

for r in */run.sh; do
  d=$(dirname $r)
  echo "**** $d ****"
  (cd $d && ./run.sh)
done
