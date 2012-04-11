#!/bin/bash

cp ~/bin/yap ~/bin/city_fove
YAP=~/bin/city_fove

LOG_FILE=fove.log
#LOG_FILE=results`date "+ %H:%M:%S %d-%m-%Y"`.log

CITY_LOCATION="'../../examples/city'"

rm -f $LOG_FILE
rm -f ignore.$LOG_FILE


function run_solver
{
  solver=$1
  network=$2
  solver_flag=true
  if [ -n "$3" ]; then
    if [ $solver = hve ]; then
      extra_flag=clpbn_horus:set_horus_flag\(elim_heuristic,$3\)
    elif [ $solver = bp  ]; then
      extra_flag=clpbn_horus:set_horus_flag\(schedule,$3\)
    elif [ $solver = cbp ]; then
      extra_flag=clpbn_horus:set_horus_flag\(schedule,$3\)
    else
      echo "unknow flag $3"
    fi
  fi
  /usr/bin/time -o $LOG_FILE -a -f "real:%E\tuser:%U\tsys:%S" \
      $YAP << EOF >> $LOG_FILE 2>> ignore.$LOG_FILE
[$CITY_LOCATION].
[$network].
clpbn_horus:set_solver($solver).
clpbn_horus:set_horus_flag(use_logarithms, true).
$solver_flag.
is_joe_guilty(X).
open("$LOG_FILE", 'append', S), format(S, '$network: ~15+ ', []), close(S).
EOF
}


function run_all_graphs
{
  echo -n "**********************************" >> $LOG_FILE
  echo    "**********************************" >> $LOG_FILE
  echo    "results for solver $2"              >> $LOG_FILE
  echo -n "**********************************" >> $LOG_FILE
  echo    "**********************************" >> $LOG_FILE
   run_solver $1 city_5        $3
  #run_solver $1 city_1000     $3
  #run_solver $1 city_5000     $3
  #run_solver $1 city_10000    $3
  #run_solver $1 city_50000    $3
  #run_solver $1 city_100000   $3
  #run_solver $1 city_500000   $3
  #run_solver $1 city_1000000  $3
}

run_all_graphs fove "fove                                  "

