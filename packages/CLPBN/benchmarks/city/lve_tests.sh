#!/bin/bash

source city.sh
source ../benchs.sh

SOLVER="lve"

function run_all_graphs
{
  write_header $1
  run_solver city1000     $2
  run_solver city5000     $2
  run_solver city10000    $2
  run_solver city15000    $2
  run_solver city20000    $2
  run_solver city25000    $2
  run_solver city30000    $2
  run_solver city35000    $2
  run_solver city40000    $2
  run_solver city45000    $2
  run_solver city50000    $2
  run_solver city55000    $2
  run_solver city60000    $2
  run_solver city65000    $2
  run_solver city70000    $2
  run_solver city75000    $2
  run_solver city80000    $2
  run_solver city85000    $2
  run_solver city90000    $2
  run_solver city95000    $2
  run_solver city100000   $2
}

prepare_new_run
run_all_graphs "lve                                   "

