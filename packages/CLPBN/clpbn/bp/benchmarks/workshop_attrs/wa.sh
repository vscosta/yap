#!/bin/bash

NETWORK="'../../examples/workshop_attrs'"
SHORTNAME="wa"
QUERY="series(X)"


function run_all_graphs
{
  LOG_FILE=$SOLVER.log
  #LOG_FILE=results`date "+ %H:%M:%S %d-%m-%Y"`.

  rm -f $LOG_FILE
  rm -f ignore.$LOG_FILE

  cp ~/bin/yap $YAP

  echo -n "**********************************" >> $LOG_FILE
  echo    "**********************************" >> $LOG_FILE
  echo    "results for solver $1"              >> $LOG_FILE
  echo -n "**********************************" >> $LOG_FILE
  echo    "**********************************" >> $LOG_FILE
   run_solver pop_10       $2
  #run_solver pop_1000     $2
  #run_solver pop_5000     $2
  #run_solver pop_10000    $2
  #run_solver pop_50000    $2
  #run_solver pop_100000   $2
  #run_solver pop_500000   $2
  #run_solver pop_1000000  $2
}


