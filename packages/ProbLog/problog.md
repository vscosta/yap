@defgroup problog The Problog-I Language and Learning System 

@{

This document is intended as a user guide for the users of ProbLog-I.
ProbLog is a probabilistic Prolog, a probabilistic logic programming
language, which is integrated in YAP-Prolog. Most of the work in ProbLog is now based on(Prolog-II), but we still maintain ProbLog-I in order to experiment with close integration of probabilistic nd logical systems.

[TOC]

@section InstallingProbLog Installing ProbLog


You will need the CUDD binary decision daagram generator. CUDD is available in Fedora Linux, MacPorts and other Linux distributions. If it is not available in your system, please fetch it from:

- [git@github.com:vscosta/cudd.git]

To compile CUDD you will need to run:

- `./configure --enable-dynamic=true`
- `make`
- `make -j install`

@section RunningProbLog Running ProbLog

To run ProbLog, go your ProbLog folder (eg. $\sim$/problog), and start
YAP (eg. $\sim$/yap/yap). This will start YAP with ProbLog
functionality.

To use ProbLog, the ProbLog module has to be loaded at the top of your
Prolog programs. This is done with the following statement:

> *:- use_module('../path/to/problog').*

where '../path/to/problog' represents the path to the problog.yap module
(ie. without the extension) from the current folder from where YAP was
started.



Similarly, to use the ProbLog learning module, use:

> *:- use_module('../path/to/problog_learning').*

@section EncodingProbs Encoding Probabilistic Facts

A probabilistic fact is encoded in ProbLog by preceding a predicate with
a probability value. For example:

> *0.5::heads(_).*

encodes the fact that there's 50% chance of getting heads when tossing
an unbiassed coin.

@subsection EncodingPars  Encoding Parameter Learning Facts

Instead of probabilities every fact has a t( ) prefix. The t stands for
tunable and indicate that ProbLog should learn the probability. The
number between the parentheses indicates the ground truth probability.
It is ignored by the learning algorithm and if you do not know the
ground truth, you can write t(_). The ground truth is used after
learning to estimate the distance of the learned model parameters to the
ground truth model parameters. For example:

> t(0.5)::heads(1).


