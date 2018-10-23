/* File : example.i */
%module(directors="1") streamer
%{
#include "streamer.h"

%}

%include "std_string.i"

/* A base class for callbacks from C++ to output text on the Java side */
%feature("director") AndroidStreamer;

%include "streamer.h"
