%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ProbLog program describing a probabilistic graph
% (running example from ProbLog presentations)
% $Id: learn_graph.pl 4875 2010-10-05 15:28:35Z theo $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% example for parameter learning with LeProbLog
%
% training and test examples are included at the end of the file
% query ?- do_learning(20).
% will run 20 iterations of learning with default settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(problog)).
:- use_module(library(problog_learning)).

:- include(graph_bk).


