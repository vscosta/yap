#!/bin/bash

source sn2ev.sh
source ../benchs.sh

SOLVER="lve"

function run_all_graphs
{
  write_header $1
  run_solver ev0p$POP  $2
  run_solver ev5p$POP  $2
  run_solver ev10p$POP $2
  run_solver ev15p$POP $2
  run_solver ev20p$POP $2
  run_solver ev25p$POP $2
  run_solver ev30p$POP $2
  run_solver ev35p$POP $2
  run_solver ev40p$POP $2
  run_solver ev45p$POP $2
  run_solver ev50p$POP $2
  run_solver ev55p$POP $2
  run_solver ev60p$POP $2
  run_solver ev65p$POP $2
  run_solver ev70p$POP $2
  run_solver ev75p$POP $2
  run_solver ev80p$POP $2
  run_solver ev85p$POP $2
  run_solver ev90p$POP $2
}

prepare_new_run
run_all_graphs "lve                                   "


