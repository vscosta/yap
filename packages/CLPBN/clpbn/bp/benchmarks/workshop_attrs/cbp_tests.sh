#!/bin/bash

source wa.sh
source ../benchs.sh

SOLVER="cbp"

function run_all_graphs
{
  write_header $1
  run_solver pop1000     $2
  run_solver pop5000     $2
  run_solver pop10000    $2
  run_solver pop50000    $2
  run_solver pop100000   $2
  run_solver pop500000   $2
  run_solver pop1000000  $2
}

prepare_new_run
run_all_graphs "cbp(shedule=seq_fixed)                " seq_fixed

