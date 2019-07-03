%%% -*- Mode: Prolog; -*-

%% @file lbfgs.pl

%  This file is part of YAP-LBFGS.
%  Copyright (C) 2009 Bernd Gutmann
%
%  YAP-LBFGS is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  YAP-LBFGS is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with YAP-LBFGS.  If not, see <http://www.gnu.org/licenses/>.



:- module(lbfgs,[lbfgs_initialize/2,
		 lbfgs_initialize/4,
		 lbfgs_run/3,

		 lbfgs_fx/1,
		 lbfgs_finalize/1,
		 lbfgs_alloc/2,
		 lbfgs_free/1,

		 lbfgs_set_parameter/2,
		 lbfgs_get_parameter/2,
		 lbfgs_parameters/0]).

% switch on all the checks to reduce bug searching time
% :- yap_flag(unknown,error).
% :- style_check(single_var).

/**

@defgroup YAP-LBFGS Interface to LibLBFGS
@ingroup packages

@short What is YAP-LBFGS? YAP-LBFGS is an interface to call [libLBFG](http://www.chokkan.org/software/liblbfgs/), from within
YAP. libLBFGS is a C library for Limited-memory
Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) solving the under-constrained
minimization problem:

~~~~~~~~~~~~~~~~~~~~~~~~
+  minimize `F(X), X=(x1,x2,..., xN)`
~~~~~~~~~~~~~~~~~~~~~~~~


### Contact YAP-LBFGS has been developed by Bernd Gutmann. In case you
publish something using YAP-LBFGS, please give credit to me and to
libLBFGS. And if you find YAP-LBFGS useful, or if you find a bug, or
if you port it to another system, ... please send me an email.



### License
+  YAP-LBFGS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

+ YAP-LBFGS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.


### Usage</h2>
The module lbfgs provides the following predicates after you loaded
it by
~~~~
:-use_module(library(lbfgs)).
~~~~

+ use lbfgs_set_paramater(Name,Value) to change parameters
+ use lbfgs_get_parameter(Name,Value) to see current parameters
+ use lbfgs_parameters to print this overview



### Demo

The following Prolog program, ex1.pl, searches for minimas of the
function `f(x0)=sin(x0)`. In order to do so, it provides the
call back predicate <span class="code">evaluate` which
calculates `f(x0)` and the gradient `d/dx0 f=cos(x0)`.

~~~~~
:- use_module(lbfgs).

% This is the call back function which evaluates F and the gradient of F
evaluate(FX,X,G,_N,_Step,_User) :-
	X0 <== X[0],
F is sin(X0),
	FX[0] <== F,
	G0 is cos(X0),
	G[0] <== G0.

% This is the call back function which is invoked to report the progress
% if the last argument is set to anything else than 0, the lbfgs will
% stop right now
progress(FX,X,X_Norm,G_Norm,Step,_N,Iteration,Ls,0) :-
	X0 <== X[0],
	format('~d. Iteration : x0=~4f  f(X)=~4f  |X|=~4f
                |X\'|=~4f  Step=~4f  Ls=~4f~n',
                [Iteration,X0,FX,X_Norm,G_Norm,Step,Ls]).



demo :-
	format('Optimizing the function f(x0) = sin(x0)~n',[]),
	lbfgs_initialize(1,X,0,Solver),


	StartX is random*10,
	format('We start the search at the random position x0=~5f~2n',[StartX]),
	X[0] <== StartX,

	lbfgs_run(Solver,BestF,Status),
	BestX0 <== X[0],
	lbfgs_finalize(Solver),
	format('~2nOptimization done~nWe found a minimum at
	f(~f)=~f~2nLBFGS Status=~w~n',[BestX0,BestF,Status]).
~~~~~
The output of this program is something like:

~~~~~
   ?- demo.
Optimizing the function f(x0) = sin(x0)
We start the search at the random position x0=7.24639

1. Iteration : x0=5.0167  f(X)=-0.9541  |X|=5.0167  |X'|=0.2996  Step=3.9057  Ls=3.0000
2. Iteration : x0=4.7708  f(X)=-0.9983  |X|=4.7708  |X'|=0.0584  Step=0.0998  Ls=2.0000
3. Iteration : x0=4.7113  f(X)=-1.0000  |X|=4.7113  |X'|=0.0011  Step=1.0000  Ls=1.0000
4. Iteration : x0=4.7124  f(X)=-1.0000  |X|=4.7124  |X'|=0.0000  Step=1.0000  Ls=1.0000


Optimization done
We found a minimum at f(4.712390)=-1.000000

LBFGS Status=0
yes
   ?-
~~~~~


@{

*/

:- load_foreign_files(['libLBFGS'],[],'init_lbfgs_predicates').

/** @pred lbfgs_initialize(+N, -SolverInfo)

Do initial memory allocation and a reference to a descriptor.
~~~~
lbfgs_initialize(1, Block)
~~~~~
*/
lbfgs_initialize(N,X) :-
    integer(N),
    N>0,
    lbfgs_grab(N,X).

	% install call back predicates in the user module which call
	% the predicates given by the arguments


/** @pred  lbfgs_finalize(+State)

Clean up the memory.
*/
lbfgs_finalize(N) :-
    lbfgs_release(N).

/** @pred  lbfgs_run(+State, -FinalOutput)

run the algorithm. output the final score of the function being optimised
*/
lbfgs_run(N,X,FX) :-
    lbfgs(N,X),
    lbfgs_fx(FX).



/** @pred  lbfgs_parameters/0
Prints a table with the current parameters. See the <a href="http://www.chokkan.org/software/liblbfgs/structlbfgs__parameter__t.html#_details">documentation
of libLBFGS</a> for the meaning of each parameter.

~~~~
   ?- lbfgs_parameters(State).
==========================================================================================
Type      Name               Value          Description
==========================================================================================
int       m                  6              The number of corrections to approximate the inverse hessian matrix.
float     epsilon            1e-05          Epsilon for convergence test.
int       past               0              Distance for delta-based convergence test.
float     delta              1e-05          Delta for convergence test.
int       max_iterations     0              The maximum number of iterations
int       linesearch         0              The line search algorithm.
int       max_linesearch     40             The maximum number of trials for the line search.
float     min_step           1e-20          The minimum step of the line search routine.
float     max_step           1e+20          The maximum step of the line search.
float     ftol               0.0001         A parameter to control the accuracy of the line search routine.
float     gtol               0.9            A parameter to control the accuracy of the line search routine.
float     xtol               1e-16          The machine precision for floating-point values.
float     orthantwise_c      0.0            Coefficient for the L1 norm of variables
int       orthantwise_start  0              Start index for computing the L1 norm of the variables.
int       orthantwise_end    -1             End index for computing the L1 norm of the variables.
==========================================================================================
~~~~
*/
lbfgs_parameters  :-
	lbfgs_get_parameter(m,M ),
	lbfgs_get_parameter(epsilon,Epsilon ),
	lbfgs_get_parameter(past,Past ),
	lbfgs_get_parameter(delta,Delta ),
	lbfgs_get_parameter(max_iterations,Max_Iterations ),
	lbfgs_get_parameter(linesearch,Linesearch ),
	lbfgs_get_parameter(max_linesearch,Max_Linesearch ),
	lbfgs_get_parameter(min_step,Min_Step ),
	lbfgs_get_parameter(max_step,Max_Step ),
	lbfgs_get_parameter(ftol,Ftol ),
	lbfgs_get_parameter(gtol,Gtol ),
	lbfgs_get_parameter(xtol,Xtol ),
	lbfgs_get_parameter(orthantwise_c,Orthantwise_C ),
	lbfgs_get_parameter(orthantwise_start,Orthantwise_Start ),
	lbfgs_get_parameter(orthantwise_end,Orthantwise_End ),

	format('/******************************************************************************************~n',[] ),
	print_param('Name','Value','Description','Type' ),
	format('******************************************************************************************~n',[] ),
	print_param(m,M,'The number of corrections to approximate the inverse hessian matrix.',int ),
	print_param(epsilon,Epsilon,'Epsilon for convergence test.',float ),
	print_param(past,Past,'Distance for delta-based convergence test.',int ),
	print_param(delta,Delta,'Delta for convergence test.',float ),
	print_param(max_iterations,Max_Iterations,'The maximum number of iterations',int ),
	print_param(linesearch,Linesearch,'The line search algorithm.',int ),
	print_param(max_linesearch,Max_Linesearch,'The maximum number of trials for the line search.',int ),
	print_param(min_step,Min_Step,'The minimum step of the line search routine.',float ),
	print_param(max_step,Max_Step,'The maximum step of the line search.',float ),
	print_param(ftol,Ftol,'A parameter to control the accuracy of the line search routine.',float ),
	print_param(gtol,Gtol,'A parameter to control the accuracy of the line search routine.',float ),
	print_param(xtol,Xtol,'The machine precision for floating-point values.',float ),
	print_param(orthantwise_c,Orthantwise_C,'Coefficient for the L1 norm of variables',float ),
	print_param(orthantwise_start,Orthantwise_Start,'Start index for computing the L1 norm of the variables.',int ),
	print_param(orthantwise_end,Orthantwise_End,'End index for computing the L1 norm of the variables.',int ),
	format('******************************************************************************************/~n',[]),
	format(' use lbfgs_set_parameter(Name,Value) to change parameters~n',[]),
	format(' use lbfgs_get_parameter(Name,Values) to see current parameters~n',[]),
	format(' use lbfgs_parameters to print this overview~2n',[]).


print_param(Name,Value,Text,Dom) :-
	format(user,'~w~10+~w~19+~w~15+~w~30+~n',[Dom,Name,Value,Text]).


%% @}
