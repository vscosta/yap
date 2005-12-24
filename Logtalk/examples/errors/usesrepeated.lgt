
% conflict between uses/2 directives

:-object(usesrepeated).

	:- uses(list, [member/2]).	% a predicate cannot be referenced
	:- uses(set, [member/2]).	% in more than one uses/2 directive

:- end_object.
