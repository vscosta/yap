#!/bin/bash

#cp ~/bin/yap ~/bin/school_all
#YAP=~/bin/school_all
YAP=~/bin/yap

#OUT_FILE_NAME=results`date "+ %H:%M:%S %d-%m-%Y"`.log
OUT_FILE_NAME=results.log
rm -f $OUT_FILE_NAME
rm -f ignore.$OUT_FILE_NAME

# yap -g "['../../../../examples/School/sch32'], [missing5], use_module(library(clpbn/learning/em)), graph(L), clpbn:set_clpbn_flag(em_solver,bp), clpbn_horus:set_horus_flag(inf_alg, bp), statistics(runtime, _), em(L,0.01,10,_,Lik), statistics(runtime, [T,_])."

function run_solver
{
if [ $2 = bp ]
then
  if [ $4 = ve ]
  then
    extra_flag1=clpbn_horus:set_horus_flag\(inf_alg,$4\)
    extra_flag2=clpbn_horus:set_horus_flag\(elim_heuristic,$5\)
  else
    extra_flag1=clpbn_horus:set_horus_flag\(inf_alg,$4\)
    extra_flag2=clpbn_horus:set_horus_flag\(schedule,$5\)
  fi
else
  extra_flag1=true
  extra_flag2=true
fi
/usr/bin/time -o "$OUT_FILE_NAME" -a -f "real:%E\tuser:%U\tsys:%S" $YAP << EOF &>> "ignore.$OUT_FILE_NAME"
:- [pos:train].
:- ['../../../../examples/School/sch32'].
:- use_module(library(clpbn/learning/em)).
:- use_module(library(clpbn/bp)).
[$1].
graph(L),
  clpbn:set_clpbn_flag(em_solver,$2),
  $extra_flag1, $extra_flag2,
  em(L,0.01,10,_,Lik),
  open("$OUT_FILE_NAME", 'append',S),
  format(S, '$3: ~11+ Lik = ~3f, ',[Lik]),
  close(S).
EOF
}


function run_all_graphs
{
  echo "************************************************************************" >> "$OUT_FILE_NAME"
  echo "results for solver $2" >> "$OUT_FILE_NAME"
  echo "************************************************************************" >> "$OUT_FILE_NAME"
  run_solver  missing5   $1  missing5   $3  $4  $5
  run_solver  missing10  $1  missing10  $3  $4  $5
  #run_solver  missing20  $1  missing20  $3  $4  $5
  #run_solver  missing30  $1  missing30  $3  $4  $5
  #run_solver  missing40  $1  missing40  $3  $4  $5
  #run_solver  missing50  $1  missing50  $3  $4  $5
}

					
#run_all_graphs  bp     "hve(min_neighbors) "  ve   min_neighbors
#run_all_graphs  bp     "bp(seq_fixed)      "  bp   seq_fixed
#run_all_graphs  bp     "cbp(seq_fixed)     "  cbp  seq_fixed
exit


run_all_graphs  bp    "hve(min_neighbors) "  ve   min_neighbors
run_all_graphs  bp    "hve(min_weight)    "  ve   min_weight
run_all_graphs  bp    "hve(min_fill)      "  ve   min_fill
run_all_graphs  bp    "hve(w_min_fill)    "  ve   weighted_min_fill
run_all_graphs  bp    "bp(seq_fixed)      "  bp   seq_fixed
run_all_graphs  bp    "bp(max_residual)   "  bp   max_residual
run_all_graphs  bp    "cbp(seq_fixed)     "  cbp  seq_fixed
run_all_graphs  bp    "cbp(max_residual)  "  cbp  max_residual
run_all_graphs  gibbs "gibbs              "
echo "************************************************************************" >> "$OUT_FILE_NAME"
echo "results for solver ve" >> "$OUT_FILE_NAME"
echo "************************************************************************" >> "$OUT_FILE_NAME"
run_solver  missing5   ve  missing5   $3  $4  $5
run_solver  missing10  ve  missing10  $3  $4  $5
run_solver  missing20  ve  missing20  $3  $4  $5
run_solver  missing30  ve  missing30  $3  $4  $5
run_solver  missing40  ve  missing40  $3  $4  $5
#run_solver  missing50  ve  missing50  $3  $4  $5 #+24h!
echo "************************************************************************" >> "$OUT_FILE_NAME"
echo "results for solver jt" >> "$OUT_FILE_NAME"
echo "************************************************************************" >> "$OUT_FILE_NAME"
run_solver  missing5   jt  missing5   $3  $4  $5
run_solver  missing10  jt  missing10  $3  $4  $5
run_solver  missing20  jt  missing20  $3  $4  $5
#run_solver  missing30  jt  missing30  $3  $4  $5 #+24h!
#run_solver  missing40  jt  missing40  $3  $4  $5 #+24h!
#run_solver  missing50  jt  missing50  $3  $4  $5 #+24h!
exit

