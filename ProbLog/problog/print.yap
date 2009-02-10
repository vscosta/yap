%%% -*- Mode: Prolog; -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% printing functions used for problog_help and problog_flags
% collected here to have formatting at one place
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(print, [print_param/4,
	print_sep_line/0,
	print_inference/2]).

print_param(Keyword,Value,Function,Legal) :-
	format(user,'~w~55+~q~15+~w~30+~w~25+~n',[Keyword,Value,Function,Legal]).
print_sep_line :-
	sep_line(125).
sep_line(0) :- 
	!,
	format('~n',[]).
sep_line(N) :-
	format('-',[]),
	NN is N-1,
	sep_line(NN).

print_inference(Call,Description) :-
	format(user,'~w~65+~w~60+~n',[Call,Description]).
