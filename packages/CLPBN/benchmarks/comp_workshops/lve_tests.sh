#!/bin/bash

source cw.sh
source ../benchs.sh

SOLVER="lve"

function run_all_graphs
{
  write_header $1
  run_solver p1000w$N_WORKSHOPS   $2
  run_solver p5000w$N_WORKSHOPS   $2
  run_solver p10000w$N_WORKSHOPS  $2
  run_solver p15000w$N_WORKSHOPS  $2
  run_solver p20000w$N_WORKSHOPS  $2
  run_solver p25000w$N_WORKSHOPS  $2
  run_solver p30000w$N_WORKSHOPS  $2
  run_solver p35000w$N_WORKSHOPS  $2
  run_solver p40000w$N_WORKSHOPS  $2
  run_solver p45000w$N_WORKSHOPS  $2
  run_solver p50000w$N_WORKSHOPS  $2
  run_solver p55000w$N_WORKSHOPS  $2
  run_solver p60000w$N_WORKSHOPS  $2
  run_solver p65000w$N_WORKSHOPS  $2
  run_solver p70000w$N_WORKSHOPS  $2
}

prepare_new_run
run_all_graphs "lve                                   "


