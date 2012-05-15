#!/bin/bash

source sm.sh
source ../benchs.sh

SOLVER="hve"

function run_all_graphs
{
  write_header $1
  run_solver pop25   $2
  run_solver pop50   $2
  #run_solver pop75   $2
  #run_solver pop100  $2
  #run_solver pop125  $2
  #run_solver pop150  $2
}

prepare_new_run
run_all_graphs "hve(elim_heuristic=min_neighbors)     " min_neighbors
#run_all_graphs "hve(elim_heuristic=min_weight)        " min_weight
#run_all_graphs "hve(elim_heuristic=min_fill)          " min_fill
#run_all_graphs "hve(elim_heuristic=weighted_min_fill) " weighted_min_fill

