%#!/home/vsc/.local/bin/yap -L --

/** @file filter.yap
 *
 * This filter extends doxygen with YAP documentation support.
 *
 */

:- include(docutils).

:-dynamic pred_found/3, exported/3, defines_module/1.

defines_module(prolog).

:- initialization(main).



:- set_prolog_flag(double_quotes, string).

valid_suffix('.yap').
valid_suffix('.pl').
valid_suffix('.prolog').
valid_suffix('.P').
valid_suffix('.ypp').

/**
 * @pred main
 *
 * Call the filter.
*/

main :-
    retractall(pred_found(_,_,_)),
    unix(argv([File])),
    absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first)]),
    
    %    valid_suffix(ValidSuffix),
    %    sub_atom(Y,_,_,0,ValidSuffix),
    !,
    file_directory_name(File, Dir),
    working_directory(OldD,Dir),
    open(Y,read,S,[alias(loop_stream)]),
    script(S),
    findall(O, user:entry(S,O), Info),
%spy trl,
     predicates(Info, true/0, Preds, _),
     insert_module_header,
     maplist(output,Preds),
     working_directory(_,OldD),
     insert_module_tail,
     halt.
 main.

 /*
 atom_concat('cat ',File,Command),
 unix(system(Command)).


     read_stream_to_string(S,Text),
     format('~s',[Text]).
 */

 %%
 %% @pred script(+S)
 %%
 script(S) :-
     peek_char(S,'#'),
     !,
      readutil:read_line_to_string(S,_),
     script(S).
 script(_).

 /**
  * @pred entry(+Stream, -Units).
  * Obtain text units
  */
 user:entry(S,O) :-
     repeat,
     read_clause(S,T,[comments(Comments),variable_names(Vs)]),
     (
	T == end_of_file
       ->
       O = directive(end_of_file, Comments, Vs),
       !
       ;
       T = ( :- Directive )
       ->
       (
	 user:dxpand(Directive),
	 fail
	 ;
	 O = directive(Directive, Comments, Vs)
       )
       ;

       T = (( Grammar --> Expansion ))
       ->
       functor(Grammar,Name,Arity),      
       A is Arity+2,
       O = clause(Name/A, Grammar, Expansion, Comments, Vs)
       ;
       T = (( Head :- Body ))
       ->
       functor(Head,Name,Arity),
       O = clause(Name/Arity,Head,Body,Comments,Vs)
       ;
       functor(T,Name,Arity),
       O=clause(Name/Arity,Head,true,Comments,Vs)
     ).

 %% initial directive
  % predicates([H|_],_,_,_) :-
  %     writeln(H),
  %     fail.
% predicates([H|_],Ctx,_,_) :- writeln(Ctx:H),fail.
 predicates([],_,[],[]).
 predicates([directive(Directive,Comments,Vs)|More],Ctx,
	    [command([comments(Comments),  body(Directive, Vs,true/0),[]])|Preds], L0) :-
     !,
     predicates(More,Ctx,Preds,L0).
 % change of predicate
 predicates([clause(N/A,Head,Body,Comments,Vs)|More],Ctx,
	    [predicate(N/A,[comments(Comments), head_body(Head, Body, Vs)|L0])|Preds], L0) :-
     Ctx \= N/A,
     !,
     predicates(More,N/A,Preds,L0).
 % new predicate
 predicates([clause(N/A,Head,Body,Comments,Vs)|More],[],

	    [predicate(N/A,[comments(Comments), head_body(Head, Body, Vs)|L0])|Preds], L0) :-
    !,
    predicates(More,N/A,Preds,L0).
% predicate continuation
predicates([clause(N/A,Head,Body,Comments,Vs)|More],N/A,
	   Preds, [comments(Comments), head_body(Head, Body, Vs)|L0]) :-
                        predicates(More,N/A,Preds,L0).

output(command([comments(Comments) |_])) :-
    maplist(out_comment,Comments ).
output(predicate(N/A,[comments(Comments) |_Clauses])) :-
    encode(N/A,S1),
    atom_string(NA,S1),
    maplist(out_comment,Comments),
    addcomm(N/A,S1,_Found),
    findall(I,between(1,A,I),Is),
    maplist(atomic_concat('int ARG'),Is,NIs),
    (
      is_exported(N,A)
      ->
      (
      A==0 ->
      format(' class  ~s {        ~w();~n};~n~n~n',[ S1,NA])
      ;
      T =.. [NA|NIs],
      format(' class  ~s {        ~w();~n};~n~n~n',[ S1,T])
      );
    true
    ) .

insert_module_header :-
    %    format('class Predicate;~n~n',[]),
    defines_module(M),
    !,
    format('namespace ~s~n{~n',[M]).
insert_module_header.

insert_module_tail :-
    defines_module(_M),
    !,
    format('}~n',[]).
insert_module_tail.

out_comment(C) :-
    trl(C,Cf), 
    !,
    string_concat(Cf,S),
    format('~s~n',[S]).

trl( C,["/**<",Space|NC]) :-
    sub_string(C,0,4,_,"/**<"),
    sub_string(C,4,1,_,Space),
    sp(Space),
    !,
    sub_string(C,5,_,0,Comm),
    trl_comment(Comm,star,NC).
trl( C,["/**",Space|NC]) :-
    sub_string(C,0,3,_,"/**"),
    sub_string(C,3,1,_,Space),
    sp(Space),
    !,
    sub_string(C,4,_,0,Comm),
    trl_comment(Comm,star,NC).
trl( C,["///<",Space|NC]) :-
    sub_string(C,0,3,_,"%%<"),
    sub_string(C,3,1,_,Space),
    sp(Space),
    !,
    sub_string(C,4,_,0,Comm),
    trl_comment(Comm,star,NC).
trl( C,["///",Space|NC]) :-
    sub_string(C,0,2,_,"%%"),
    sub_string(C,2,1,_,Space),
    sp(Space),
    !,
    sub_string(C,3,_,0,Comm),
    trl_comment(Comm,star,NC).
trl( C,["//!",Space|NC]) :-
    sub_string(C,0,2,_,"%!"),
    sub_string(C,2,1,_,Space),
    sp(Space),
    !,
    sub_string(C,3,_,0,Comm),
    trl_comment(Comm,star,NC).
trl( _C,[ ""]).

sp(" ").
sp("\t").
sp("\n").

ws(" ").
ws("\t").

trl_comment(L,_,NL) :-
    trl_lines(L,NL,[]).

trl_lines( "", L, L) :-
    !.
trl_lines(Lines, O, O0) :-
    sub_string(Lines,Left,1,Right,"\n"),
    Right>0,
    !,
    sub_string(Lines,0,Left,_,C),
    trl_line(C,O,["\n"|O1]),
    sub_string(Lines,_,Right,0, R),
    trl_lines(R,O1,O0).
trl_lines( Lines, O, O0) :-
    trl_line( Lines, O, O0).

trl_line(  "",O,O) :-
    !.
trl_line(  L,NL,NL3) :-
    ((
    trl_prefix(L,L1,NL,NL1),
    trl_pred(L1,L2, NL1, NL2),
    trl_pi(L2,NL2,NL3)
    )).


trl_prefix(C,RC,["///"|NC],NC) :-
    sub_string(C,0,2,_Len,Pref),
    (Pref == "%%"
     ->
     true ;Pref == "%!"), 
     sub_string(C,2,1,_Comm,Space),
    sp(Space),
    !,
    sub_string(C,3,_,0,RC). 
trl_prefix(C,C,NC,NC).

% arity > 0
trl_pred(L,RL,NL,NRL) :-
    % EL = Bef+"@pred"+After
    (
      sub_string(L,Bef,5,After,"@pred")
      ->
      true
      ;
      sub_string(L,Bef,5,After,"@Pred")
      ),
    After1 is After-1,

    sub_string(L,_,1,After1,WS),
    ws(WS),
    sub_string(L,_,After1,0,Line0),
    strip_whitespace(Line0,0,Line),
   sub_string(Line,_,1,_,"("),
    detect_name(Line,Name,Args,Arity,RL),
    !,
    number_string(Arity,A),
    atom_string(At,Name),
    defines_module(M),
    assert(pred_found(M,At,Arity)),
    sub_string(L,0,Bef,_, Prefix),
    string_concat([Name,"/",A],PI),
    encode(PI,DoxName),
    NL=[Prefix,"@class ",DoxName,"\n       *",Name,Args,"* "|NRL].
% arity == 0
trl_pred(L,RL,NL,NRL) :-
    (
      sub_string(L,Bef,5,After,"@pred")
      ->
      true
      ;
      sub_string(L,Bef,5,After,"@Pred")
      ),
    sub_string(L,_,After,0,L1),
    strip_whitespace(L1,0,L2),
    detect_name(L2,Name,Args,Arity,RL),
    !,
    number_string(Arity,A),
    atom_string(At,Name),
    defines_module(M),
    assert(pred_found(M,At,Arity)),
    sub_string(L,0,Bef,_, Prefix),
    string_concat([Name,"/",A],PI),
    encode(PI,DoxName),
    NL=[Prefix,"@class ",DoxName,"\n       *",Name,Args,"* "|NRL].
trl_pred(L,RL,NL,NRL) :-
    sub_string(L,Bef,10,_After,"@infixpred"),
    A0 is Bef+10,
    skip_whitespace(A0,L,A1),
    A1>A0,
    block(A1,L,B1),
	       skip_whitespace(B1,L,A2),
	A2>A1,	
		block(A2,L,B2),
		skip_whitespace(B2,L,A3),
		A3>A2,
		block(A3,L,B3),
			   !,
			    L2 is B2-A2,
			    L1 is B3-A1,
			    sub_string(L,A2,L2,_,Name),
			    sub_string(L,A1,L1,_,NameArgs),
			    sub_string(L,B3,_,0,RL),
			    string_concat([Name,"/2"],PI),
			    encode(PI,DoxName),
			    atom_string( At, Name),
			    defines_module(Mod),
			    assert(pred_found(Mod,At,2)),
    sub_string(L,0,Bef,_, Prefix),
    NL =  [Prefix,"@class ",DoxName,"\n       *",NameArgs,"* "|NRL].
trl_pred(L,L,NL,NL).

    strip_whitespace(Line0,I0,Line) :-
    sub_string(Line0,I0,1,_,WS),
    ws(WS),
    !,
    I is I0+1,
    strip_whitespace(Line0,I,Line).
    strip_whitespace(Line,0,Line) :-
    !.
    strip_whitespace(Line0,I0,Line) :-
    sub_string(Line0,I0,_,0,Line).

    skip_whitespace(I0,Line,IF) :-
    sub_string(Line,I0,1,_,WS),
    ws(WS),
    !,
    I is I0+1,
    skip_whitespace(I,Line,IF).
    skip_whitespace(I,_Line,I).

  
    block(I0,Line,IF) :-
    sub_string(Line,I0,1,_,WS),
\+    ws(WS),
    !,
    I is I0+1,
    block(I,Line,IF).
    block(I,_Line,I).

  
detect_name(Line,Name,Args,Arity,Extra) :-
    sub_string(Line,Bef,1,_,"("),
    !,
    sub_string(Line,_,1,After,")"),
    sub_string(Line,0,Bef,_,Name),
    sub_string(Line,Bef,_Sz,After,Args),
    sub_string(Line,_,After,0,Extra),
    findall(I,sub_string(Args,I,1,_,","),Is),
    length([_|Is],Arity).

detect_name(Line,Name,"",0,Extra) :-
    sub_string(Line,Bef,1,After,WS),
    ws(WS),
    !,
    sub_string(Line,0,Bef,_,Name),
    sub_string(Line,_,After,0,Extra).
detect_name(Name,Name,"",0,"").

trl_pi(L,NL,NL0) :-
    sub_string(L,Left,1,Right,"/"),
    Left > 0,
    Right > 0,
    Left1 is Left+1,
    sub_string(L,Left1,1,_Right,D),
    digit(D),
    back(Left,L,NPrefix),
NPrefix \= Left,
!,
    sub_string(L,0,NPrefix,_,Prefix),
    sub_string(L,NPrefix,_,Right,Name),
    sub_string(L,_,Right,0,RL),
    string_concat([Name,"/",D],PI),
    encode(PI,DoxName),
    NL=[Prefix,"@ref ",DoxName," \"",PI,"\""|NRL],
    trl_pi(RL,NRL,NL0).
trl_pi(S,[S|C],C).
    

back(0,_L,0) :-
    !.
back(I0,S,I0) :-
    I is I0-1,
    sub_string(S,I,1,_,WS),
    ws(WS),    !.
    back(I0,S,P) :-
     I is I0-1,
    back(I,S,P).
   

digit("0").
digit("1").
digit("2").
digit("3").
digit("4").
digit("5").
digit("6").
digit("7").
digit("8").
digit("9").
    

alphanum(A) :-
    char_type(A,csym),
    !.
alphanum(':').

addcomm(N/A,S,false) :-
    is_exported(N,A),
    \+ pred_found(_M,N,A),
    !,
    length(L,A),
    maplist(=('?'),L),
    T =.. [N|L],
    format('~n~n/**   @class ~s	 **~w**\n     (undocumented)  **/~n~n',[S,T]).
addcomm(_,_,_).


:- initialization(main).



dxpand(module(M,Gs)) :-
    assert(defines_module(M)),
    maplist(pxpand(M),Gs).
dxpand(module(M)) :-
    retractall(defines_module(_)),
    assert(defines_module(M)).
dxpand(use_module(_M,_Gs)).

pxpand(Mod,op(M,Gs,Y)) :-
    op(M,Gs,Mod:Y),
    op(M,Gs,Y).
pxpand(Mod,A/B) :-
    assert(exported(Mod,A,B)).
pxpand(Mod,_ as A/B) :-
    assert(exported(Mod,A,B)).
pxpand(Mod,A//B):-
    B2 is B+2,
    assert(exported(Mod,A,B2)) .

is_exported(N,_) :-
    string(N),
    string_concat(`$`,_,N),
    !,
    fail.
is_exported(N,_) :-
    atom(N),
    sub_atom(N,0,1,_,'$'),
    !,
    fail.
is_exported(_,_) :-
    defines_module(user),
    !.
is_exported(N,A) :-
    defines_module(Mod),
    exported(Mod,N,A).


