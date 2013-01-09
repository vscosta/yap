#!/bin/bash

source ../benchs.sh

SHORTNAME="school"
SOLVER="school"


function learn_params
{
  NETWORK="'./../../examples/School/school_32'"
  CONSTRAINT=$2
  SOLVER=$1
  echo $NETWORK
  /usr/bin/time -o $LOG_FILE -a -f "%U\t%S\t%e\t%M" \
      $YAP << EOF >> ignore.$LOG_FILE 2>> ignore.$LOG_FILE
use_module(library(pfl)).
use_module(library(clpbn/learning/em)).
[$NETWORK].
[$CONSTRAINT].
set_em_solver($SOLVER).
graph(L),
%	em(L, 0.01, 10, _, Lik),
	open("$LOG_FILE", 'append', S),
        format(S, "$CONSTRAINT: ~15+ Lik = ~3f\t", [Lik]),
	close(S).
EOF
}


prepare_new_run

write_header hve
learn_params  hve  missing5
learn_params  hve  missing10
learn_params  hve  missing20
#learn_params  hve  missing30
#learn_params  hve  missing40
#learn_params  hve  missing50

write_header ve
learn_params  ve  missing5
learn_params  ve  missing10
learn_params  ve  missing20
#learn_params  ve  missing30
#learn_params  ve  missing40
#learn_params  hve  missing50

write_header bp
learn_params  bp  missing5
learn_params  bp  missing10
learn_params  bp  missing20
#learn_params  bp  missing30
#learn_params  bp  missing40
#learn_params  bp missing50

write_header cbp
learn_params  cbp  missing5
learn_params  cbp  missing10
learn_params  cbp  missing20
#learn_params  cbp  missing30
#learn_params  cbp  missing40
#learn_params  cbp missing50

