
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
  echo $LOG_FILE
  CONSTRAINT=$1
  solver_flag=true
  if [ -n "$2" ]; then
    if [ $SOLVER = hve ]; then
      SOLVER_FLAG=set_horus_flag\(hve_elim_heuristic,$2\)
    elif [ $SOLVER = bp ]; then
      SOLVER_FLAG=set_horus_flag\(bp_msg_schedule,$2\)
    elif [ $SOLVER = cbp ]; then
      SOLVER_FLAG=set_horus_flag\(bp_msg_schedule,$2\)
    elif [ $SOLVER = lbp ]; then
      SOLVER_FLAG=set_horus_flag\(bp_msg_schedule,$2\)
    else
      echo "unknow flag $2"
    fi
  fi
  /usr/bin/time -o $LOG_FILE -a -f "%U\t%S\t%e\t%M" \
      $YAP << EOF >> ignore.$LOG_FILE 2>> ignore.$LOG_FILE
nogc.
[$NETWORK].
[$CONSTRAINT].
set_solver($SOLVER).
set_horus_flag(verbosity, 1).
set_horus_flag(use_logarithms, true).
$SOLVER_FLAG.
$QUERY.
open("$LOG_FILE", 'append', S), format(S, "$CONSTRAINT ~15+ ", []), close(S).
EOF
}



function clear_log_files
{
  rm -f *~
  rm -f ../*~
  rm -f workshop_attrs/*.log workshop_attrs/*~
  rm -f ../workshop_attrs/*.log ../workshop_attrs/*~
  rm -f comp_workshops/*.log comp_workshops/*~
  rm -f ../comp_workshops/*.log ../comp_workshops/*~
  rm -f city/*.log city/*~
  rm -f ../city/*.log ../city/*~
  rm -f social_network2/*.log social_network2/*~
  rm -f ../social_network2/*.log ../social_network2/*~
  rm -f social_network2_evidence/*.log social_network2_evidence/*~
  rm -f ../social_network2_evidence/*.log ../social_network2_evidence/*~
  echo all done!
}



function write_header
{
  echo -n "****************************************" >> $LOG_FILE
  echo    "****************************************" >> $LOG_FILE
  echo    "results for solver $1 user(s) sys(s) real(s), mem(kB)" >> $LOG_FILE
  echo -n "****************************************" >> $LOG_FILE
  echo    "****************************************" >> $LOG_FILE
}


if [ $1 ] && [ $1 == "clean" ]; then
  clear_log_files
fi


