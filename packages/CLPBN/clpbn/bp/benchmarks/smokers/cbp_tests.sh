#!/bin/bash

source sm.sh
source ../benchs.sh

SOLVER="cbp"

function run_all_graphs
{
  write_header $1
  run_solver pop25   $2
  run_solver pop50   $2
  run_solver pop75   $2
  run_solver pop100  $2
  run_solver pop125  $2
  run_solver pop150  $2
}

prepare_new_run
run_all_graphs "cbp(shedule=seq_fixed)                " seq_fixed

