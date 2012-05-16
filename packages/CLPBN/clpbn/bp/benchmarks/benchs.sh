

function prepare_new_run
{
  YAP=~/bin/$SHORTNAME-$SOLVER

  LOG_FILE=$SOLVER.log
  #LOG_FILE=results`date "+ %H:%M:%S %d-%m-%Y"`.

  rm -f $LOG_FILE
  rm -f ignore.$LOG_FILE

  cp ~/bin/yap $YAP
}



function run_solver
{
  constraint=$1
  solver_flag=true
  if [ -n "$2" ]; then
    if [ $SOLVER = hve ]; then
      solver_flag=clpbn_horus:set_horus_flag\(elim_heuristic,$2\)
    elif [ $SOLVER = bp ]; then
      solver_flag=clpbn_horus:set_horus_flag\(schedule,$2\)
    elif [ $SOLVER = cbp ]; then
      solver_flag=clpbn_horus:set_horus_flag\(schedule,$2\)
    else
      echo "unknow flag $2"
    fi
  fi
  /usr/bin/time -o $LOG_FILE -a -f "real:%e\tuser:%U\tsys:%S\tmem:%MkB" \
      $YAP << EOF >> $LOG_FILE &>> ignore.$LOG_FILE
nogc.
[$NETWORK].
[$constraint].
clpbn_horus:set_solver($SOLVER).
clpbn_horus:set_horus_flag(use_logarithms, true).
clpbn_horus:set_horus_flag(verbosity, 1).
$solver_flag.
$QUERY.
open("$LOG_FILE", 'append', S), format(S, '$constraint: ~15+ ', []), close(S).
EOF
}



function clear_log_files
{
  rm -f *~
  rm -f ../*~
  rm -f school/*.log school/*~
  rm -f ../school/*.log ../school/*~
  rm -f city/*.log city/*~
  rm -f ../city/*.log ../city/*~
  rm -f workshop_attrs/*.log workshop_attrs/*~
  rm -f ../workshop_attrs/*.log ../workshop_attrs/*~
  echo all done!
}



function write_header
{
  echo -n "****************************************" >> $LOG_FILE
  echo    "****************************************" >> $LOG_FILE
  echo    "results for solver $1"                >> $LOG_FILE
  echo -n "****************************************" >> $LOG_FILE
  echo    "****************************************" >> $LOG_FILE
}


if [ $1 ] && [ $1 == "clean" ]; then
  clear_log_files
fi


