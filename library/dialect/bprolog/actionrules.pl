/*

    Author:        Bart Demoen, Phuong-Lan Nguyen
    E-mail:        Bart.Demoen@cs.kuleuven.be, nguyen@uco.fr
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, K.U. Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    
    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


/* What is this module for ... see bottom of the file */

:- module(actionrules,[op(1200,xfx,=>),
		       op(1200,xfx,?=>),
		       op(1000,xfy,:::),
		       op(900,xfy,<=),
		       post/1,
		       post_event/2,
		       post_event_df/2,
		       post_event_df/3,
		       register_event/2
		       ]).

:- use_module(library(lists)).

:- dynamic ar_term/2, extra_ar_term/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  the built-ins and the preds needed in the transformation    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_event(event(X,_),G) :- add_attr(X,'$$event',G).
register_event(ins(X),G) :- add_attr(X,'$$ins',G).
register_event(generated,_).    % ignore

add_attr(X,Mod,A) :-
	(get_attr(X,Mod,Old) ->
	    New = [A|Old]
	;
	    New = [A]
	),
	put_attr(X,Mod,New).

post(event(X,Mes)) :- !,
	(get_attr(X,'$$event',Gs) ->
	    activate_agents_rev(Gs,Mes)
	;
	    (var(X) ->
		true
	    ;
		throw(actionrule(event/2,illegalfirstargument))
	    )
	).
post(ins(X)) :- !,
	(get_attr(X,'$$ins',Gs) ->
	    call_list_rev(Gs)
	;
	    (var(X) ->
		true
	    ;
		throw(actionrule(ins/1,illegalfirstargument))
	    )
	).
post(Event) :-
	throw(actionrule(Event,illegalpost)).

post_event(X,Mes) :-
	get_attr(X,'$$event',Gs), !, activate_agents_rev(Gs,Mes).
post_event(X,_) :-
	(var(X) ->
	    true
	;
	    throw(actionrule(post_event/2,illegalfirstargument))
	).
	
post_event_df(X,Mes) :-
	get_attr(X,'$$event',Gs), !, activate_agents1(Gs,Mes).
post_event_df(_,_).

post_event_df(X,Alive,Mes) :-
	get_attr(X,'$$event',Gs), !, activate_agents(Gs,Alive,Mes).
post_event_df(_,_,_).

'$$ins':attr_unify_hook(AttrX,Y) :-
	(var(Y) ->
	    (get_attr(Y,'$$ins',AttrY) ->
		append(AttrX,AttrY,NewAttr)
	    ;
		NewAttr = AttrX
	    ),
	    put_attr(Y,ins,NewAttr)
	;
	    true
	),
	call_list_rev(AttrX).

'$$event':attr_unify_hook(_,_).

call_list_rev(Goals) :-
	reverse(Goals,Gs),
	call_list(Gs).

call_list([]).
call_list([G|Gs]) :-
	call(G),
	call_list(Gs).

activate_agents_rev(Goals,M) :-
	reverse(Goals,Gs),
	activate_agents(Gs,M).

activate_agents([],_).
activate_agents([G|Gs],Mes) :-
	G =.. [N,_|R],
	NewG =.. [N,Mes|R],
	call(NewG),
	activate_agents(Gs,Mes).

activate_agents([],_,_).
activate_agents([G|Gs],Alive,Mes) :-
	(var(Alive) ->
	    G =.. [N,_|R],
	    NewG =.. [N,Mes|R],
	    call(NewG),
	    activate_agents(Gs,Alive,Mes)
	;
	    true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ar_translate and helper predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ars2p(ARs,Det,Head,Program,Errors,TailProgram,TailErrors) :-
	copyskel(Head,Skel),
	cleanheads(ARs,NewARs,Skel),
	Skel =.. [N|Args],
	makeagentname(N,AgentName),
	NewSkel =.. [AgentName,Mes,Alive|Args],
	findmess(NewARs,Mes),
	genfirstclause(NewARs,Det,NewSkel,Skel,Program,Errors,TailProgram1,TailErrors1),
	gensecondclause(NewARs,Det,NewSkel,Alive,TailProgram1,TailErrors1,TailProgram,TailErrors).

genfirstclause(NewARs,Det,NewSkel,Skel,Program,Errors,TailProgram,TailErrors) :-
	Clause = (Skel :- (Closure = NewSkel), Body),
	makefirstbody(NewARs,Det,Closure,Body,Errors,TailErrors),
	Program = [Clause | TailProgram].


build_conditional(det, Guard, B, (Guard -> B)).
build_conditional(nondet, Guard, B, (Guard, B)).

makefirstbody([ar(Head,Guard,Events,B)|R],Det,Closure,Bodys,Errors,TailErrors) :-
	(Events == [] ->
	    build_conditional(Det, Guard, B, Body),
	    Errors = Errors1
	;
	    check_events(Events,Head,Errors,Errors1),
	    mkregistergoals(Events,Register,Closure),
	    (member(generated,Events) ->
	        build_conditional(Det, Guard, (Register,B), Body)
	    ;
	        build_conditional(Det, Guard, Register, Body)
	    )
	),
	(R == [] ->
	    Bodys = Body,
	    Errors1 = TailErrors
	;
	    Bodys = (Body ; MoreBody),
	    makefirstbody(R,Det,Closure,MoreBody,Errors1,TailErrors)
	).

gensecondclause(NewARs,Det,NewSkel,Alive,Program,Errors,TailProgram,Errors) :-
	Clause = (NewSkel :- (var(Alive) -> Body ; true)),
        makesecondbody(NewARs,Det,NewSkel,Body,Alive),
	Program = [Clause | TailProgram].

makesecondbody([ar(_,Guard,Events,B)|R],Det,NewSkel,Bodys,Alive) :-
	(Events == [] ->
	    build_conditional(Det, Guard, (Alive = no, B), Body)
	;
	    build_conditional(Det, Guard, B, Body)
	),
	(R == [] ->
	    Bodys = Body
	;
	    Bodys = (Body ; MoreBody),
	    makesecondbody(R,Det,NewSkel,MoreBody,Alive)
	).

check_events([],_,E,E).
check_events([Event|R],S,E,TailE) :-
	(nonvar(Event), okevent(Event) ->
	    E = E1
	;
	    E = [illegalevent(Event,S)|E1]
	),
	check_events(R,S,E1,TailE).

okevent(ins(X)) :- !, var(X).
okevent(event(X,M)) :- !, var(X), var(M).
okevent(generated).

findmess([],_).
findmess([ar(_,_,Events,_)|R],Mes) :-
	findmess2(Events,Mes),
	findmess(R,Mes).

findmess2([],_).
findmess2([A|R],Mes) :-
	(A = event(_,Mes) ->
	    true
	;
	    true
	),
	findmess2(R,Mes).
	
copyskel(T1,T2) :-
	functor(T1,N,A),
	functor(T2,N,A).

cleanheads([],[],_).
cleanheads([ar(Head,Conds,Events,Body)|R],[ar(NewHead,NewConds,Events,Body)|S],Skel) :-
	makenewhead(Head,NewHead,Unies),
	Skel = NewHead,
	append(Unies,Conds,LNewConds),
        conds_to_goals(LNewConds, NewConds0),
        removetrue(NewConds0, NewConds),
	cleanheads(R,S,Skel).

conds_to_goals([], true) :- !.
conds_to_goals(C.LNewConds, (C,NewConds0)) :- !,
        conds_to_goals(LNewConds, NewConds0).
conds_to_goals(C,C).

makenewhead(Head,NewHead,Unies) :-
	Head =.. [_|Args],
	functor(Head,N,A),
	functor(NewHead,N,A),
	NewHead =.. [_|NewArgs],
	makeunies(Args,NewArgs,Unies).

makeunies([],_,[]).
makeunies([X|R],[Y|S],Us) :-
	(var(X) ->
	    X = Y,
	    Us = Us2
	;
	    Us = [X=Y|Us2]  % this should be matching instead of unification
	),
	makeunies(R,S,Us2).


get_arinfo(AR,ARInfo,Head) :-
	AR = (Something => Body),
	(Something = (Head,Rest) ->
	    findcondevents(Rest,Conds,Events)
	;
	    Something = Head, Conds = true, Events = []
	),
	ARInfo = ar(Head,Conds,Events,Body).
get_arinfo(AR,ARInfo,Head) :-
	AR = (Something ?=> Body),
	(Something = (Head,Rest) ->
	    findcondevents(Rest,Conds,Events)
	;
	    Something = Head, Conds = true, Events = []
	),
	ARInfo = ar(Head,Conds,Events,Body).
get_arinfo(AR,ARInfo,Head) :-
	AR = (Head :- Rest ::: Body),
        Conds = Rest, Events = [],
	ARInfo = ar(Head,Conds,Events,Body).

findcondevents((A,B),(A,As),Ts) :- !,
	findcondevents(B,As,Ts).
findcondevents({Trs},true,Ts) :- !,
	makeevents(Trs,Ts).
findcondevents(A,A,[]).

makeevents((A,B),[A|R]) :- !, makeevents(B,R).
makeevents(A,[A]).

samehead(A,B) :-
	functor(A,X,Y),
	functor(B,X,Y).

makeagentname(N,Out) :-
	name(N,NL),
	name('$$suspended_',A),
	append(A,NL,ANL),
	name(Out,ANL).

mkregistergoals([],true,_).
mkregistergoals([X|R],Register,Skel) :-
	(X == generated ->
	    mkregistergoals(R,Register,Skel)
	;
	    Register = (register_event(X,Skel),S),
	    mkregistergoals(R,S,Skel)
	).

removetrue(true,true) :- !.
removetrue((true,A),AA) :- !, removetrue(A,AA).
removetrue((A,true),AA) :- !, removetrue(A,AA).
removetrue((A,B),(AA,BB)) :- !, removetrue(A,AA), removetrue(B,BB).
removetrue((A->B),(AA->BB)) :- !, removetrue(A,AA), removetrue(B,BB).
removetrue((A;B),(AA;BB)) :- !, removetrue(A,AA), removetrue(B,BB).
removetrue(X,X).


ar_translate([],_,[],[]).
ar_translate([AR|ARs],Module,Program,Errors) :-
	get_head(AR,ARHead),
	collect_ars_same_head(ARs,ARHead,ActionPredRest,RestARs),
	ars2p([AR|ActionPredRest],det,ARHead,Program,Errors,TailProgram,TailErrors),
        extra_ars(AR, TailProgram, NTailProgram),
	ar_translate(RestARs,Module,NTailProgram,TailErrors).

nondet_ar_translate([],_,Program,Program,[]).
nondet_ar_translate([AR|ARs],Module,Program,EndProgram,Errors) :-
	get_head(AR,ARHead),
	collect_ars_same_head(ARs,ARHead,ActionPredRest,RestARs),
	ars2p([AR|ActionPredRest],nondet,ARHead,Program,Errors,TailProgram,TailErrors),
	nondet_ar_translate(RestARs,Module,TailProgram, EndProgram,TailErrors).

collect_ars_same_head([],_,[],[]).
collect_ars_same_head([AR1|ARs],Head,SameHeadARs,RestARs) :-
	get_head(AR1,Head1),
	(same_head(Head1,Head) ->
	    SameHeadARs = [AR1|SameHeadARsRest],
	    collect_ars_same_head(ARs,Head,SameHeadARsRest,RestARs)
	;
	    RestARs = [AR1|RestARsRest],
	    collect_ars_same_head(ARs,Head,SameHeadARs,RestARsRest)
	).

get_head(ar(Head,_Conds,_Events,_Body),Head).

same_head(T1,T2) :-
	functor(T1,N,A),
	functor(T2,N,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ar_expand(Term, []) :-
	Term = (_ => _), !,
	prolog_load_context(file,File),
	get_arinfo(Term,ARInfo,_),
	assert(ar_term(File,ARInfo)).
ar_expand(Term, []) :-
	Term = (_ :- _ ::: _), !,
	prolog_load_context(file,File),
	get_arinfo(Term,ARInfo,_),
	assert(ar_term(File,ARInfo)).
ar_expand(Term, []) :-
	Term = (_ ?=> _ ), !,
	prolog_load_context(file,File),
	get_arinfo(Term,ARInfo,_),
	assert(nondet_ar_term(File,ARInfo)).
ar_expand(Term, []) :-
	Term = (Head :- Body ),
	prolog_load_context(file,File),
        functor(Head, Na, Ar),
        functor(Empty, Na, Ar),
        ar_term(File,ar(Empty,_,_,_)), !,
	assert(extra_ar_term(File,ar(Head, Body))).
ar_expand(Head, []) :-
	prolog_load_context(file,File),
        functor(Head, Na, Ar),
        functor(Empty, Na, Ar),
        ar_term(File,ar(Empty,_,_,_)), !,
	assert(extra_ar_term(File,ar(Head, true))).

ar_expand(end_of_file, FinalProgram) :-
	prolog_load_context(file,File),
        compile_ar(File, DetProgram),
        compile_nondet_ar(File, FinalProgram, DetProgram),
	FinalProgram = [_|_].

compile_ar(File, FinalProgram) :-
	findall(T, retract(ar_term(File,T)), ARs),
	ARs \== [],
	prolog_load_context(module, Module),
	ar_translate(ARs, Module, FinalProgram, Errors),
	!, % just to make sure there are no choice points left
	% vsc: also, allow for nondet rules.
	(Errors == [] ->
	    true
	;
	    report_errors(Errors)
	).
compile_ar(_File, []).

compile_nondet_ar(File, FinalProgram, StartProgram) :-
	findall(T, retract(nondet_ar_term(File,T)), ARs),
	ARs \== [],
	prolog_load_context(module, Module),
	nondet_ar_translate(ARs, Module, FinalProgram, StartProgram, Errors),
	!, % just to make sure there are no choice points left
	(Errors == [] ->
	    true
	;
	    report_errors(Errors)
	).
compile_nondet_ar(_File, FinalProgram, FinalProgram).


report_errors(Errors) :- throw(action_rule_error(Errors)). % for now

extra_ars(ar(Head,_,_,_), LF, L0) :-
       functor(Head, N, A),
       functor(Empty, N, A),
       findall((Empty :- B), extra_ar_term(_,ar(Empty, B)), LF, L0).


/*******************************
*         MUST BE LAST!        *
*******************************/

:- multifile user:term_expansion/2.
:- dynamic   user:term_expansion/2.

user:term_expansion(In, Out) :-
	ar_expand(In, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  What this file is for .... %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

Action Rules were defined and implemented first in the context of
B-Prolog and the TOAM by Neng-Fa Zhou - see http://www.probp.com/

See http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW456.abs.html
for an explanation what this file is based on.

Use_module-ing this file will give you an implementation of Action Rules
functionality related to the event patterns ins/1, generated/0 and
event/2.

It is not a fast implementation in SWI-Prolog, because there isn't any
low-level support.

If you need more functionality, please contact the authors.

*/
