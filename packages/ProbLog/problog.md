@defgroup Problog1 The Problog-I Language and Learning System 
@ingroup YAPPackages
@{

This document is intended as a user guide for the users of ProbLog-I.
ProbLog is a probabilistic Prolog, a probabilistic logic programming
language, which is integrated in YAP-Prolog. Most of the work in ProbLog is now based on(Prolog-II), but we still maintain ProbLog-I in order to experiment with close integration of probabilistic nd logical systems.


 ProbLog  assumes probabilistic facts as Prob::Fact and clauses in normal Prolog format

 provides following inference modes (16/12/2008):
 - approximation with interval width Delta (IJCAI07): problog_delta(+Query,+Delta,-Low,-High,-Status)
 - bounds based on single probability threshold: problog_threshold(+Query,+Threshold,-Low,-High,-Status)
 - as above, but lower bound only: problog_low(+Query,+Threshold,-Low,-Status)
 - lower bound based on K most likely proofs: problog_kbest(+Query,+K,-Low,-Status)
 - explanation probability (ECML07): problog_max(+Query,-Prob,-FactsUsed)
 - exact probability: problog_exact(+Query,-Prob,-Status)
 - sampling: problog_montecarlo(+Query,+Delta,-Prob)


 angelika.kimmig@cs.kuleuven.be


@}

@defgroup InstallingProbLog Installing ProbLog
@ingroup Problog1


@{
You will need the CUDD binary decision daagram generator. CUDD is available in Fedora Linux, MacPorts and other Linux distributions. If it is not available in your system, please fetch it from:

- [git@github.com:vscosta/cudd.git]

To compile CUDD you will need to run:

- `./configure --enable-dynamic=true`
- `make`
- `make -j install`

@defgroup RunningProbLog Running ProbLog
@ingroup Problog1

To run ProbLog, go your ProbLog folder (eg. $\sim$/problog), and start
YAP (eg. $\sim$/yap/yap). This will start YAP with ProbLog
functionality.

To use ProbLog, the ProbLog module has to be loaded at the top of your
Prolog programs. This is done with the following statement:

> *:- use_module('../path/to/problog').*

where '../path/to/problog' represents the path to the problog.yap module
(ie. without the extension) from the current folder from where YAP was

@}


