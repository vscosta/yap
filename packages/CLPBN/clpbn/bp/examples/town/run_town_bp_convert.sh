#!/bin/bash

YAP=~/bin/town_conv

#OUT_FILE_NAME=results`date "+ %H:%M:%S %d-%m-%Y"`.log
OUT_FILE_NAME=bp_convert.log
rm -f $OUT_FILE_NAME
rm -f ignore.$OUT_FILE_NAME


function run_solver
{
if [ $2 = bp ]
then
    extra_flag1=clpbn_bp:set_solver_parameter\(run_mode,$4\)
    extra_flag2=clpbn_bp:set_solver_parameter\(schedule,$5\)
    extra_flag3=clpbn_bp:set_solver_parameter\(always_loopy_solver,$6\)
else
  extra_flag1=true
  extra_flag2=true
  extra_flag3=true
fi
/usr/bin/time -o $OUT_FILE_NAME -a -f "real:%E\tuser:%U\tsys:%S" $YAP << EOF >> $OUT_FILE_NAME 2>> ignore.$OUT_FILE_NAME
[$1].
clpbn:set_clpbn_flag(solver,$2),
  clpbn_bp:use_log_space,
  $extra_flag1, $extra_flag2, $extra_flag3,
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
  run_solver  town_1000     $1  town_1000     $3  $4  $5
  run_solver  town_5000     $1  town_5000     $3  $4  $5
  run_solver  town_10000    $1  town_10000    $3  $4  $5
  run_solver  town_50000    $1  town_50000    $3  $4  $5
  run_solver  town_100000   $1  town_100000   $3  $4  $5
  run_solver  town_500000   $1  town_500000   $3  $4  $5
  run_solver  town_1000000  $1  town_1000000  $3  $4  $5
}

run_all_graphs  bp     "bp(convert,seq_fixed)         "  convert   seq_fixed     false

