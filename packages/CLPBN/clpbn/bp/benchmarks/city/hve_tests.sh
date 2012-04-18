#!/bin/bash

source city.sh
source ../benchs.sh

SOLVER="hve"

function run_all_graphs
{
  write_header $1
  run_solver city1000     $2
  run_solver city5000     $2
  run_solver city10000    $2
  run_solver city50000    $2
  run_solver city100000   $2
  run_solver city500000   $2
  run_solver city1000000  $2
}

prepare_new_run
run_all_graphs "hve(elim_heuristic=min_neighbors)     " min_neighbors

