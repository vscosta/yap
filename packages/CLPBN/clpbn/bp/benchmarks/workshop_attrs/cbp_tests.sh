#!/bin/bash

source wa.sh
source ../benchs.sh

SOLVER="cbp"

YAP=~/bin/$SHORTNAME-$SOLVER

run_all_graphs "cbp(shedule=seq_fixed)                " seq_fixed

