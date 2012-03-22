#!/bin/bash

cp ~/bin/yap ~/bin/town_cbp
YAP=~/bin/town_cbp

#OUT_FILE_NAME=results`date "+ %H:%M:%S %d-%m-%Y"`.log
OUT_FILE_NAME=cbp.log
rm -f $OUT_FILE_NAME
rm -f ignore.$OUT_FILE_NAME


function run_solver
{
if [ $2 = bp ]
then
    extra_flag1=clpbn_bp:set_horus_flag\(inf_alg,$4\)
    extra_flag2=clpbn_bp:set_horus_flag\(schedule,$5\)
else
  extra_flag1=true
  extra_flag2=true
fi
/usr/bin/time -o $OUT_FILE_NAME -a -f "real:%E\tuser:%U\tsys:%S" $YAP << EOF >> $OUT_FILE_NAME 2>> ignore.$OUT_FILE_NAME
[$1].
clpbn:set_clpbn_flag(solver,$2),
  clpbn_bp:set_horus_flag(use_logarithms, true),
  $extra_flag1, $extra_flag2,
  run_query(_R),
  open("$OUT_FILE_NAME", 'append',S),
  format(S, '$3: ~15+ ',[]),
  close(S).
EOF
}


function run_all_graphs
{
  echo "*******************************************************************" >> "$OUT_FILE_NAME"
  echo "results for solver $2" >> $OUT_FILE_NAME
  echo "*******************************************************************" >> "$OUT_FILE_NAME"
  run_solver  town_1000      $1  town_1000      $3  $4  $5
  run_solver  town_5000      $1  town_5000      $3  $4  $5
  run_solver  town_10000     $1  town_10000     $3  $4  $5
  run_solver  town_50000     $1  town_50000     $3  $4  $5
  run_solver  town_100000    $1  town_100000    $3  $4  $5
  run_solver  town_500000    $1  town_500000    $3  $4  $5
  run_solver  town_1000000   $1  town_1000000   $3  $4  $5
  run_solver  town_2500000   $1  town_2500000   $3  $4  $5
  run_solver  town_5000000   $1  town_5000000   $3  $4  $5
  run_solver  town_7500000   $1  town_7500000   $3  $4  $5
  run_solver  town_10000000  $1  town_10000000  $3  $4  $5
}

run_all_graphs  bp     "cbp(seq_fixed)                "  cbp   seq_fixed

