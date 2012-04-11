#!/bin/bash

source wa.sh
source ../benchs.sh

SOLVER="hve"

YAP=~/bin/$SHORTNAME-$SOLVER

run_all_graphs "hve(elim_heuristic=min_neighbors)     " min_neighbors

