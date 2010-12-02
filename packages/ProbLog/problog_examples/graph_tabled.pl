%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ProbLog program describing a probabilistic graph using tabling
% (running example from ProbLog presentations)
% $Id: graph_tabled.pl 4875 2010-10-05 15:28:35Z theo $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(problog)).

% New trie method ensures Probibilistic Cycle Handling needed for tabling that handles loops
:- set_problog_flag(use_db_trie, true).
:- set_problog_flag(use_old_trie, false).

%%%%
% background knowledge
%%%%
% definition of acyclic path using list of visited nodes

% to table a predicate you first need to define it as a dynamic one
:- dynamic path/2.

path(X,X).
path(X,Y) :-
  X\==Y,
  edge(X,Z),
  path(Z,Y).

:- problog_table path/2.
% after all predicate definitions have appeared you need to state that the predicate will be tabled

% using directed edges in both directions
edge(X,Y) :- dir_edge(Y,X).
edge(X,Y) :- dir_edge(X,Y).


%%%%
% probabilistic facts
%%%%
0.9::dir_edge(1,2).
0.8::dir_edge(2,3).
0.6::dir_edge(3,4).
0.7::dir_edge(1,6).
0.5::dir_edge(2,6).
0.4::dir_edge(6,5).
0.7::dir_edge(5,3).
0.2::dir_edge(5,4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% example queries about tabled path(1,4) useable only with problog_exact, problog_montecarlo currently
%
%%% success probability
%     ?- problog_exact(path(1,4),Prob,Status).
%  Prob = 0.53864,
%  Status = ok ?
%  yes
%%% approximation using monte carlo, to reach 95%-confidence interval width 0.01
%     ?- problog_montecarlo(path(1,4),0.01,Prob).
%  Prob = 0.537525 ?
%  yes
%%% success probability of negation
%     ?- problog_exact(problog_neg(path(1,4)),Prob,Status).
%  Prob = 0.46136,
%  Status = ok ?
%  yes
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
