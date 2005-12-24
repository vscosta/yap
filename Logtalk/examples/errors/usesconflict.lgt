
% conflict between uses/2 directive and a predicate definition

:-object(usesconflict).

	:- uses(list, [member/2]).

	member(H, [H| _]).		% an object (or category) cannot define a
	member(H, [_| T]) :-	% predicate referenced on a uses/2 directive
		member(H, T).

:- end_object.
