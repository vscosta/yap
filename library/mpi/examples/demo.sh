#!/bin/sh
# demo.sh
#
# This file was originally created on 3/7/2003
# by Stasinos Konstantopoulos konstant@let.rug.nl
# as part of the YAP Prolog distribution.
#
# This file is in the Public Domain.

# Arguments: $1 will be the Prolog programme to load.

PLFILE=$1
shift 1

YAP=${HOME}/opt/yap-cur/mpich-gm-gcc-nodebug/bin/yap

# accumulator for stuff before the --
before=""
# accumulator for stuff after the --
after=""
# the previous parameter is expecting an argument
argument=""
# i'm still busy with the "before" stuff
flag=yes

while test -n "$1"; do
    #echo "DEBUG: $*"
    #echo "DEBUG: flag = $flag"
    if test -n "$flag"; then
	#echo "DEBUG: argument = $argument"
	if test -n "$argument"; then
	    argument=""
	    before="$before $1"
	#elif test "$1" = "-l" -o "$1" = "-h" -o "$1" = "-s" -o "$1" = "-t"; then
	#    argument=yes
	#    before="$before $1"
	else
	    flag=""
	fi
    fi
    if test -z "$flag"; then
	after="$after $1"
    fi
    shift
done

#echo "${YAP} $before -- $after"

QUOTE="'"

echo "consult(${QUOTE}${PLFILE}${QUOTE}). start(0,20). halt."
echo "consult(${QUOTE}${PLFILE}${QUOTE}). start(0,20). halt." | ${YAP} $before -- $after

