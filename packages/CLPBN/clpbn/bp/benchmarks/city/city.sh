#!/bin/bash

NETWORK="'../../examples/city'"
SHORTNAME="city"
QUERY="is_joe_guilty(X)"


function run_all_graphs
{
  cp ~/bin/yap $YAP
  echo -n "**********************************" >> $LOG_FILE
  echo    "**********************************" >> $LOG_FILE
  echo    "results for solver $1"              >> $LOG_FILE
  echo -n "**********************************" >> $LOG_FILE
  echo    "**********************************" >> $LOG_FILE
   run_solver city_5        $2
  #run_solver city_1000     $2
  #run_solver city_5000     $2
  #run_solver city_10000    $2
  #run_solver city_50000    $2
  #run_solver city_100000   $2
  #run_solver city_500000   $2
  #run_solver city_1000000  $2
}

