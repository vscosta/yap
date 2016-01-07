%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ProbLog program describing modelling a simplified version of the ALARM network
% (running example used in the paper [Gutmann et. al, ECML 2011])
% $Id: alarm.pl 6416 2011-06-10 14:38:44Z bernd $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% example for parameter learning with LFI-ProbLog
%
% training examples are included at the end of the file
% query ?- do_learning(20).
% will run 20 iterations of learning with default settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




:- use_module('../problog').
:- use_module('../problog_lfi').

% uncomment to see what is happening
:- set_problog_flag(verbosity_learning,5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Probabilistic Facts  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the t(_) identifies them as tunable, that is,
% the probabilities are to be learned

t(_) :: burglary.
t(_) :: earthquake.
t(_) :: hears_alarm(_Person).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Background Knowledge %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the background knowledge, read as myclause(Head,Body)
% clauses are assumed to be range-restricted

myclause(person(mary), true).
myclause(person(john), true).
myclause(alarm, burglary).
myclause(alarm, earthquake).
myclause(calls(Person), (person(Person),alarm,hears_alarm(Person))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Training examples    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%l

example(1).
example(2).

%%%% Example 1
known(1,alarm,true).

%%%% Example 2
known(2,earthquake,false).
known(2,calls(mary),true).
