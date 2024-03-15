%%% -*- mode: Prolog; -*-

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
:- reexport(library(problog)).
:- reexport(library(problog_lbfgs)).




%%%%
% background knowledge
%%%% 
% definition of acyclic path using list of visited nodes

:- lbfgs:lbfgs_set_parameter(epsilon,0.01).


:- include(graph_bk).

%%%%
