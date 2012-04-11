#!/bin/bash

source wa.sh
source ../benchs.sh

SOLVER="bp"

YAP=~/bin/$SHORTNAME-$SOLVER

run_all_graphs "bp(shedule=seq_fixed)                 " seq_fixed

