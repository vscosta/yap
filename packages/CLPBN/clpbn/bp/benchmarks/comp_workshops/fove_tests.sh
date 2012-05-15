#!/bin/bash

source cw.sh
source ../benchs.sh

SOLVER="fove"

function run_all_graphs
{
  write_header $1
  run_solver p1000w$N_WORKSHOPS   $2
  run_solver p5000w$N_WORKSHOPS   $2
  run_solver p10000w$N_WORKSHOPS  $2
  run_solver p20000w$N_WORKSHOPS  $2
  run_solver p30000w$N_WORKSHOPS  $2
  run_solver p40000w$N_WORKSHOPS  $2
  run_solver p50000w$N_WORKSHOPS  $2
}

prepare_new_run
run_all_graphs "fove                                  "


