#!/bin/awk
# mk_gmiconf.awk
#
# This file was originally created on 3/7/2003
# by Stasinos Konstantopoulos konstant@let.rug.nl
# as part of the YAP Prolog distribution.
#
# This file is in the Public Domain.


! /^$/ { 
  if( $0 in BUFF ) {
    BUFF[$0] = BUFF[$0] + 2;
  }
  else {
    BUFF[$0] = 2;
  }
  print $0 " " BUFF[$0];
}
