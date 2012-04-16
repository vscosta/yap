#!/bin/bash

source city.sh
source ../benchs.sh

SOLVER="fove"

YAP=~/bin/$SHORTNAME-$SOLVER

LOG_FILE=$SOLVER.log
#LOG_FILE=results`date "+ %H:%M:%S %d-%m-%Y"`.

rm -f $LOG_FILE
rm -f ignore.$LOG_FILEE

run_all_graphs "fove                                  "

