
if [ $1 ] && [ $1 == "clear" ]; then
  rm *~
  rm -f school/*.log school/*~
  rm -f city/*.log city/*~
  rm -f workshop_attrs/*.log workshop_attrs/*~
fi

function run_solver
{
  constraint=$1
  solver_flag=true
  if [ -n "$2" ]; then
    if [ $SOLVER = hve ]; then
      extra_flag=clpbn_horus:set_horus_flag\(elim_heuristic,$2\)
    elif [ $SOLVER = bp ]; then
      extra_flag=clpbn_horus:set_horus_flag\(schedule,$2\)
    elif [ $SOLVER = cbp ]; then
      extra_flag=clpbn_horus:set_horus_flag\(schedule,$2\)
    else
      echo "unknow flag $2"
    fi
  fi
  /usr/bin/time -o $LOG_FILE -a -f "real:%E\tuser:%U\tsys:%S" \
      $YAP << EOF >> $LOG_FILE 2>> ignore.$LOG_FILE
[$NETWORK].
[$constraint].
clpbn_horus:set_solver($SOLVER).
clpbn_horus:set_horus_flag(use_logarithms, true).
$solver_flag.
$QUERY.
open("$LOG_FILE", 'append', S), format(S, '$constraint: ~15+ ', []), close(S).
EOF
}

