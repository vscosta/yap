:-use_module(library(xml2yap)).
:-use_module(library(lists)).

:- initialization(main).


main :-
     absolute_file_name('../../xml/group__Builtins.xml',Y,[solutions(all),expand(true)]),
          load_xml(Y,Xml),
	  open('Builtins.md', write, _, [alias(out)]),
	  format(out, '# Core YAP Built-Ins~n~n', []), 
	  fetch_subgroups(Xml),
	  fail.
main :-
     absolute_file_name('../../xml/group__InputOutput.xml',Y,[solutions(all),expand(true)]),
          load_xml(Y,Xml),
	  open('InputOutput.md', write, _, [alias(out)]),
	  format(out, '# IO in YAP ~n~n', []), 
	  fetch_subgroups(Xml),
	  fail.

main :-
     absolute_file_name('../../xml/group__YAPLibrary.xml',Y,[solutions(all),expand(true)]),
          load_xml(Y,Xml),
	  close(out),
	  open('YAPLibrary.md', write, _, [alias(out)]),
	  format(out, '# The YAP Library~n~n', []), 
	  fetch_subgroups(Xml),
     fail.
main :-
     absolute_file_name('../../xml/group__YapExtensions.xml',Y,[solutions(all),expand(true)]),
          load_xml(Y,Xml),
	  close(out),
	  open('YapExtensions.md', write, _, [alias(out)]),
	  format(out, '# YAP Extensions to the core language.~n~n', []), 
fetch_subgroups(Xml),
     fail.
main :-
     absolute_file_name('../../xml/group__*.xml',Y,[solutions(all),expand(true)]),
          load_xml(Y,Xml),
	  fetch_spec(Xml,Name,_Title,Innergroups),
    findall(I-V,
	member(innergroup([[refid(I)],V]),Innergroups),
	Filtered),
	Filtered = [_|_],
	    format(user_error, '~n~n## ~s.~n', [Name]),
	    fetch_underlings(Filtered),
     fail.


main :-
     absolute_file_name('../../xml/group_*.xml',Y,[solutions(all),expand(true)]),
     load_xml(Y,Xml),
     Xml= [doxygen([_,compounddef(L)])],
     L=[L0|R0],
     L0=[id(Id)|_],
     R0=[compoundname([_,Name]),title([_,Title])|_],
     assert(g(Id,Name,Title)),
%    format('~s|~s|"~s"|~n',[Id,Name,Title]),
     fail.


fetch_subgroups(Xml) :-
	Xml= [doxygen([_,compounddef(L)])],
     L=[_L0|R0],
     R0=[compoundname([_,_Name]),title([_,_Title])|Innergroups],
     member(innergroup([[refid(I)],V]),Innergroups),
     format(out,'* [~s](/dox/group/~s.rst)~n',[V,I]).

fetch_spec(Xml,Name,Title,Innergroups) :-
	Xml= [doxygen([_,compounddef(L)])],
     L=[_L0|R0],
     R0=[compoundname([_,Name]),title([_,Title])|Innergroups].


fetch_underlings(Innergroups) :-
     member(I-V,Innergroups),
     format(user_error,'* [~s](/dox/group/~s.rst)~n',[V,I]).

