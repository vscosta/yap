%
% We use macros because because the
% bb operations are module specific.
% Thus the names are relative to the module
% loading this file
%

:- module( getval, []).

:- multifile
	user:goal_expansion/3.

:- dynamic
	user:goal_expansion/3.

user:goal_expansion( setval(Name,Value), _, bb_put(Name,Value)).
user:goal_expansion( getval(Name,Value), _, bb_get(Name,Value)).
user:goal_expansion( incval(Name,New),	 _, Exp) :-
	Exp = (
		bb_get( Name, Old),
		New is Old+1,
		bb_put( Name, New)
	      ).

user:goal_expansion( decval(Name,New),	 _, Exp) :-
	Exp = (
		bb_get( Name, Old),
		New is Old-1,
		bb_put( Name, New)
	      ).

end_of_file.

setval( Name, Value) :- bb_put( Name, Value).

getval( Name, Value) :- bb_get( Name, Value).

%
% ++i
%
incval( Name, New) :-
	bb_get( Name, O),
	New is O+1,
	bb_put( Name, New).

%
% --i
%
decval( Name, New) :-
	bb_get( Name, O),
	New is O-1,
	bb_put( Name, New).
