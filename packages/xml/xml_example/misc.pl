% Utility Predicates

% The following predicates are used in the puzzle solutions. 
% unique_solution( +Goal ) holds when Goal has one ground solution. Operationally,
% Goal may produce several solutions, ("don't care" non-deterministically), but they
% must all be identical (==). 

unique_solution( Goal ) :-
	findall( Goal, Goal, [Solution|Solutions] ),
	same_solution( Solutions, Solution ),
	Solution = Goal.

same_solution( [], _Solution ).
same_solution( [Solution0|Solutions], Solution ) :-
	Solution0 == Solution,
	same_solution( Solutions, Solution ).

% forall( +Enumerator, +Test ) is true if Enumerator and Test are goals and Test holds everywhere
% that Enumerator does. NB: does not further instantiate arguments.

%% forall( Enumerator, Test ) :-
%% 	\+ (call(Enumerator), \+ call(Test)).

% member( ?Element, ?List ) holds when Element is a member of List. 
member( H, [H|_] ).
member( H, [_|T] ):-
	member( H, T ).

% select( ?Element, ?List0, ?List1 ) is true if List1 is equal to List1 with Element removed.

select( H, [H|T], T ).
select( Element, [H|T0], [H|T1] ):-
	select( Element, T0, T1 ).

% memberchk( +Element, +List ) succeeds (once) if Element is a member of List. 
memberchk( Element, List ):-
	member( Element, List ),
	!.

% between( +Lower, +Upper, ?Index ) is true if Lower =< Index =< Upper. Two valid cases are
% possible:
% - Index is already instantiated to an integer so the checks on order are applied (test).
% - Index is a logical variable so a series of alternative solutions is generated as the
%   monotonic sequence of values between Lower and Upper (non-deterministic generator).

%% between( Lower, Upper, Index ):-
%% 	integer( Lower ),
%% 	integer( Upper ),
%% 	Lower =< Upper,
%% 	( integer( Index ) ->   % Case 1: "test"
%% 		Index >= Lower,
%% 		Index =< Upper
%% 	; var( Index ) ->	  % Case 2: "generate".
%% 		generate_between( Lower, Upper, Index )
%% 	).

generate_between( Lower, Upper, Index ) :-
	( Lower =:= Upper ->
		Index = Lower
	;   Index = Lower
	;   Next is Lower + 1,
		Next =< Upper,
		generate_between( Next, Upper, Index )
	).

% sum( +List, ?Sum ) holds when the List of numbers sum to Sum.

sum( [H|T], Sum ) :-
	sum1( T, H, Sum ).

sum1( [], Sum, Sum ).
sum1( [H|T], Sum0, Sum ):-
	Sum1 is Sum0 + H,
	sum1( T, Sum1, Sum ).

% put_chars( +Chars ) if Chars is a (possibly empty) list of character codes and the
% corresponding characters are written to the current output stream.

put_chars( [] ).
put_chars( [Char|Chars] ) :-
	put( Char ),
	put_chars( Chars ).

% get_chars( ?Chars ) if Chars is a (possibly empty) list of character codes read
% from the current input stream.

get_chars( Input ) :-
	get0( Char ),
	( Char > -1 ->
		Input = [Char|Chars],
		get_chars( Chars )
	; otherwise ->
		Input = []
	).
   
