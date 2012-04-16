#!/bin/bash

source city.sh
source ../benchs.sh

SOLVER="bp"

YAP=~/bin/$SHORTNAME-$SOLVER

LOG_FILE=$SOLVER.log
#LOG_FILE=results`date "+ %H:%M:%S %d-%m-%Y"`.

rm -f $LOG_FILE
rm -f ignore.$LOG_FILE

run_all_graphs "bp(shedule=seq_fixed)                 " seq_fixed

