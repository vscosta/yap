#!/bin/bash

source wa.sh
source ../benchs.sh

SOLVER="hve"

function run_all_graphs
{
  write_header $1
  run_solver p1000attrs$N_ATTRS   $2
  run_solver p5000attrs$N_ATTRS   $2
  run_solver p10000attrs$N_ATTRS  $2
  run_solver p15000attrs$N_ATTRS  $2
  run_solver p20000attrs$N_ATTRS  $2
  run_solver p25000attrs$N_ATTRS  $2
  run_solver p30000attrs$N_ATTRS  $2
  run_solver p35000attrs$N_ATTRS  $2
  run_solver p40000attrs$N_ATTRS  $2
  run_solver p45000attrs$N_ATTRS  $2
  run_solver p50000attrs$N_ATTRS  $2
  run_solver p55000attrs$N_ATTRS  $2
  run_solver p60000attrs$N_ATTRS  $2
  run_solver p65000attrs$N_ATTRS  $2
  run_solver p70000attrs$N_ATTRS  $2
  run_solver p75000attrs$N_ATTRS  $2
  run_solver p80000attrs$N_ATTRS  $2
  run_solver p85000attrs$N_ATTRS  $2
  run_solver p90000attrs$N_ATTRS  $2
  run_solver p95000attrs$N_ATTRS  $2
  run_solver p100000attrs$N_ATTRS $2
}

prepare_new_run
run_all_graphs "hve(hve_elim_heuristic=min_neighbors) " min_neighbors

