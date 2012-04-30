#!/bin/bash

source wa.sh
source ../benchs.sh

SOLVER="fove"

function run_all_graphs
{
  write_header $1
  run_solver p1000attrs$N_ATTRS   $2
  run_solver p5000attrs$N_ATTRS   $2
  run_solver p10000attrs$N_ATTRS  $2
  run_solver p20000attrs$N_ATTRS  $2
  run_solver p30000attrs$N_ATTRS  $2
  run_solver p40000attrs$N_ATTRS  $2
  run_solver p50000attrs$N_ATTRS  $2
}

prepare_new_run
run_all_graphs "fove                                  "


