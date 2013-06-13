%%% -*- Mode: Prolog; -*-

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



:- module(lbfgs,[optimizer_initialize/3,
		 optimizer_initialize/4,
		 optimizer_run/2,
		 optimizer_get_x/2,
		 optimizer_set_x/2,

		 optimizer_get_g/2,
		 optimizer_set_g/2,

		 optimizer_finalize/0,

		 optimizer_set_parameter/2,
		 optimizer_get_parameter/2,
		 optimizer_parameters/0]).

% switch on all the checks to reduce bug searching time
% :- yap_flag(unknown,error).
% :- style_check(single_var).

:- dynamic initialized/0.
:- dynamic user:'$lbfgs_callback_evaluate'/3.
:- dynamic user:'$lbfgs_callback_progress'/8.

:- load_foreign_files(['yap_lbfgs'],[],'init_lbfgs_predicates').


optimizer_initialize(N,Call_Evaluate,Call_Progress) :-
	optimizer_initialize(N,user,Call_Evaluate,Call_Progress).
optimizer_initialize(N,Module,Call_Evaluate,Call_Progress) :-
	\+ initialized,

	integer(N),
	N>0,

	% check whether there are such call back functions
	current_module(Module),
	current_predicate(Module:Call_Evaluate/3),
	current_predicate(Module:Call_Progress/8),

	optimizer_reserve_memory(N),

	% install call back predicates in the user module which call
	% the predicates given by the arguments		
	EvalGoal =.. [Call_Evaluate,E1,E2,E3],
	ProgressGoal =.. [Call_Progress,P1,P2,P3,P4,P5,P6,P7,P8],
	assert( (user:'$lbfgs_callback_evaluate'(E1,E2,E3) :- once(call(Module:EvalGoal))) ),
	assert( (user:'$lbfgs_callback_progress'(P1,P2,P3,P4,P5,P6,P7,P8) :- once(call(Module:ProgressGoal))) ),

	assert(initialized).

optimizer_finalize :-
	initialized,
	optimizer_free_memory,
	retractall(user:'$lbfgs_callback_evaluate'(_,_,_)),
	retractall(user:'$lbfgs_callback_progress'(_,_,_,_,_,_,_,_)),
	retractall(initialized).

optimizer_parameters :-
	optimizer_get_parameter(m,M),
	optimizer_get_parameter(epsilon,Epsilon),
	optimizer_get_parameter(past,Past),
	optimizer_get_parameter(delta,Delta),
	optimizer_get_parameter(max_iterations,Max_Iterations),
	optimizer_get_parameter(linesearch,Linesearch),
	optimizer_get_parameter(max_linesearch,Max_Linesearch),
	optimizer_get_parameter(min_step,Min_Step),
	optimizer_get_parameter(max_step,Max_Step),
	optimizer_get_parameter(ftol,Ftol),
	optimizer_get_parameter(gtol,Gtol),
	optimizer_get_parameter(xtol,Xtol),
	optimizer_get_parameter(orthantwise_c,Orthantwise_C),
	optimizer_get_parameter(orthantwise_start,Orthantwise_Start),
	optimizer_get_parameter(orthantwise_end,Orthantwise_End),

	format('==========================================================================================~n',[]),
	print_param('Name','Value','Description','Type'),
	format('==========================================================================================~n',[]),
	print_param(m,M,'The number of corrections to approximate the inverse hessian matrix.',int),
	print_param(epsilon,Epsilon,'Epsilon for convergence test.',float),
	print_param(past,Past,'Distance for delta-based convergence test.',int),
	print_param(delta,Delta,'Delta for convergence test.',float),
	print_param(max_iterations,Max_Iterations,'The maximum number of iterations',int),
	print_param(linesearch,Linesearch,'The line search algorithm.',int),
	print_param(max_linesearch,Max_Linesearch,'The maximum number of trials for the line search.',int),
	print_param(min_step,Min_Step,'The minimum step of the line search routine.',float),
	print_param(max_step,Max_Step,'The maximum step of the line search.',float),
	print_param(ftol,Ftol,'A parameter to control the accuracy of the line search routine.',float),
	print_param(gtol,Gtol,'A parameter to control the accuracy of the line search routine.',float),
	print_param(xtol,Xtol,'The machine precision for floating-point values.',float),
	print_param(orthantwise_c,Orthantwise_C,'Coefficient for the L1 norm of variables',float),
	print_param(orthantwise_start,Orthantwise_Start,'Start index for computing the L1 norm of the variables.',int),
	print_param(orthantwise_end,Orthantwise_End,'End index for computing the L1 norm of the variables.',int),
	format('==========================================================================================~n',[]),
	format(' use optimizer_set_paramater(Name,Value) to change parameters~n',[]),
	format(' use optimizer_get_parameter(Name,Value) to see current parameters~n',[]),
	format(' use optimizer_parameters to print this overview~2n',[]).


print_param(Name,Value,Text,Dom) :-
	format(user,'~w~10+~w~19+~w~15+~w~30+~n',[Dom,Name,Value,Text]).


