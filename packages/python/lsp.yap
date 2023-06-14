/**
 * Language Server support
 *
 */

:- module(lsp, [
	      validate_file/2,
	      scan_file/2
	  ]).

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- reexport(library(python)).
:- use_module(library(yapi)).
:- use_module(library(hacks)).
:- use_module(library(completions)).

:- dynamic ( tt/2, modifier/2 ) .

user:validate_uri(URI,Obj):-
    string_concat(`file://`,S,URI),
    atom_string(File,S),
    validate_file(File,Obj).

symbol(Text, I, PL) :-
    open(string(Text), read, S),
    scan_stream(S, Tks),
    lookup(Tks, I, PL).

lookup([t(atom(M),_L,P1	,_Sz1),
	t(atom(':'),_L2,P2,1),
      	t(atom(A),_L3,P3,_Sz12),
      	t('l',_L4,P4,1)|_Ts], J ,t(F,L,NA)) :-
	( J >= P1, J < P2 ->
	  !,
	  module_property(M,file(F)), L = 1,
	  atom_length(M,NA)
	;
	  J >= P3, J < P4 ->
	  !,
	  current_predicate(M:A/Ar),
	  Ar>0,
	  functor(G,A,Ar),
	  predicate_property(M:G,file(F)),
	  predicate_property(M:G,line_count(L)),
	  atom_length(A,NA)
	  ).
lookup([
	t(atom(A),_L3,P3,_Sz3),
      	t('l',_L4,P4,1)|_Ts], J ,t(F,L,NA)) :-
	 J >= P3, J < P4,
	 !,
	  current_predicate(A/Ar),
	  Ar>0,
	  functor(G,A,Ar),
	  predicate_property(G,file(F)),
	  predicate_property(G,line_count(L)),
	  atom_length(A,NA).
lookup([
	t(atom(A),_L3,P3,Sz3)|_Ts], J ,t(F,L,NA)) :-
	 J >= P3, J < P3+Sz3,
	 !,
	 (
		Ar=0
		;
		current_op(_,_,A), (Ar=1,qAr=2)
		),
	 current_predicate(A/Ar),
	  functor(G,A,Ar),
	  predicate_property(M:G,file(F)),
	  predicate_property(M:G,line_count(L)),
	  atom_length(A,NA).
lookup([t(_,_,P,_)|Ts], J) :-
	J =< P,
	lookup(Ts,J).

user:pred_def(URI,Text,Ch,Obj) :-
    string_concat(`file://`, FS, URI),
    atom_string(F,FS),
    (
	module_property(M, file(F))
	->
	true
	;
	M = user
	),
    current_source_module(Old, M),
    findall(P, symbol(Text,Ch,P), LFs),
    current_source_module(_, Old),
    Obj.items := LFs.


user:complete(Line,Pos,Obj) :-
    completions(Line,Pos,L),
    Obj.items := L. 


user:validate_text(URI,S,Obj):-
    string_concat(`file://`, FS, URI),
    atom_string(F,FS),
    open(string(S), read, Stream, [file_name(F),alias(data)]),
    validate_stream(Stream,Obj).

validate_stream(Stream,Self) :-
self := Self,
    assert((user:portray_message(Sev,Msg) :- q_msg(Sev, Msg)),Ref),
    warnings := [none],
    ignore( load_files(data,[stream(Stream)]) ),
    erase(Ref).

validate_file(File,Self) :-
self := Self,
    assert((user:portray_message(Sev,Msg) :- q_msg(Sev, Msg)),Ref),
    load_files(File,[]),
    erase( Ref ).


q_msg(Sev, error(Err,Inf)) :-
    Err =.. [_F|As],
    '$messages':error_descriptor(Inf, Desc),
    '$messages':query_exception(parserLine, Desc, LN),
    nonvar(LN),
    LN1 is LN-1,
    '$messages':query_exception(parserPos, Desc, Pos),
    q_msgs(As,Sev,S),
writeln(user_error,t(S,LN1,Pos)),
    self.errors.append(t(S,LN1,Pos)).
     

q_msgs([], N,S) :-      
    format(string(S ),'~s.',[N]).
q_msgs([A1], N, S) :-      
    format(string(S),'~s: ~w.',[N,A1]).
q_msgs([A1,A2], N, S) :-      
    format(string(S),'~s: ~w ~w.',[N,A1,A2]).
q_msgs([A1,A2,A3], N, S) :-      
    format(string(S),'~s: ~w ~w ~w.',[N,A1,A2,A3]).

init_codes(L) :-
    retractall(tt(_)),
    foldl(init_code,L,0,_).

init_code(TokType, Id, Id1) :-
    Id1 is Id+1,
    assert(tt(TokType,Id)).
  
init_modifiers(L) :-
    retractall(modifier(_,_)),
    foldl(init_modifier,L,1,_).

init_modifier(TokType, Id, Id1) :-
    Id1 is Id<<1,
    assert(modifier(TokType,Id)).
   

user:scan_uri(URI,Self):-
    string_concat('file://',S,URI),   
    atom_string(File,S),
     open(File,read,_,[alias(data)]),
   scan_and_convert_stream(Self).

scan_file(File,Self) :-
    open(File,read,_,[alias(data)]),
    scan_and_convert_stream(Self).


user:scan_text(Text,Self):-
    open(string(Text),read,_,[alias(data)]),
    scan_and_convert_stream(Self).


scan_and_convert_stream(Self) :-
    scan_stream(data, Ts),
    close(data),
    ins(Ts,1,0,0,LTs,[]),
    tt(method,Mod),
    modifier(definition,Def),
    (
     LTs=[TokP,TokL,TokS,Mod,0|LTs0]
     ->
     LTsf=[TokP,TokL,TokS,Mod,Def|LTs0]
     ;
     LTsf =  LTs
     ),
    (var(Self)
    ->
	Self = LTsf
    ;
    Self == show
    ->
	foldl(showt,Ts,LTsf   , _)
    ;	
	Self.data := (LTsf)
    ).


showt(T, [A,B,C,D,E|Ls],Ls) :-
    format(user_error,'~3d ~3d ~3d ~3d ~3d ~w~n',[A,B,C,D,E,T]).


%    ins([T|_] ,_L0,_P0,_Lvl) --> { writeln(T),
%	       fail }.
ins([] ,_L0,_P0,_Lvl) --> [].

ins([ t(var(_,_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    {
	
	tt(parameter,V),
	DL is L-L0,
      (DL>0->DP=P;DP is P-P0)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).

/*
ins( [t(atom(_M),L,P1,Sz1),
      t(atom(':'),L2,P2,1),
      t(atom(_A),L3,P3,Sz12),
      t('l',L4,P4,1)|Ts] ,L0,P0,0) -->
    !,
    { 	tt(method,V),
     	tt(namespace,V1),
     	tt(operator,V1),
	DL is L-L0,
	DL2 is L2-L,
	DL3 is L3-L2,
	DL4 is L4-L3,
	(DL->DP=P1
	;
	DP is P1-P0
	)
      },
    [DL,DP,Sz1,V1,0,
     D2L,DP,Sz1,V2,0,
     DL3,DP,Sz1,V,0,
     DL,DP,Sz1,V1,0,
     ],
    Lins([t('(',L,P,Sz)|Ts],L,P1,0).
*/
ins( [t(atom(_A),L,P1,Sz1),t('EOT',L,P,Sz)|Ts] ,L0,P0,0) -->
    !,
    { 	tt(method,V),
	DL is L-L0,
	(DL>0->DP=P1
	;
	DP is P1-P0
	)
      },
    [DL,DP,Sz1,V,0],
    ins([t('EOT',L,P,Sz)|Ts],L,P1,0).
ins( [t(atom(_A),L,P1,Sz1),t('l',L,P,1)|Ts] ,L0,P0,0) -->
    !,
    { tt(method,V),
      DL is L-L0,
 	(
	L>0->DP=P1
	;
	DP is P-P0
	)
      },
    [DL,DP,Sz1,V,0],
    ins([t('(',L,P,1)|Ts],L,P1,0).




ins( [t(atom(_A),L,P1,Sz1),t('l',L,P,1)|Ts] ,L0,P0,Lvl0) -->
    {Lvl0>0},
    !,
    { tt(struct,V),
      DL is L-L0,
 	(DL>0->DP=P1
	;
	DP is P-P0
	)
      },
    [DL,DP,Sz1,V,0],
    ins([t('(',L,P,1)|Ts],L,P1,Lvl0).



ins( [t(atom(_A),L,P,Sz),t(atom('-->'),L1,P1,Sz1)|Ts] ,L0,P0,0) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(classs,V)
      },
    [DL,DP,Sz,V,0],
    ins([t(atom('-->'),L1,P1,Sz1)|Ts],L,P,1).



ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { unlift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1,
      tt(operator,V)
      },
    [DL,DP,1,V,0],
    ins(Ts,Sz,P,Lvl1).

ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { lift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1,
      tt(operator,V)
      },
      [DL,DP,Sz,V,0],
      ins(Ts,L,P,Lvl1).


ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
{ operator(Op),
!,
DL is L-L0,
(DL>0->DP=P;DP is P-P0),
tt(operator,V)
},
[DL,DP,Sz,V,0],
ins(Ts,L,P,Lvl).

ins([ t(atom(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(variable,V)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).
ins([ t(comment(A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(comment,V),
      check_doc(A,Doc)
      },
    [DL,DP,Sz,V,Doc],
    ins(Ts,L,P,Lvl).

ins([ t(string(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(string,V)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).

ins([ t(number(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(number,V)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).


ins( [t('EOT',L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
       tt(keyword,V)
    },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).

ins( [t(end_of_file,_L,_P,_Sz)|_Ts] ,_L0,_P0,_Lvl) --> [].



ins( [t(Op,L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { unlift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl1).

ins( [t(Op,L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { 		    
      lift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl1).

ins( [t(Op,L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).
ins( [t(error,_,_,_Sz)|Ts] ,L,P,Lvl) -->
    !,
    ins(Ts,L,P,Lvl).
ins( [_|Ts] ,L,P,Lvl) -->
    ins(Ts,L,P,Lvl).

lift_operator(('(')).
lift_operator(('[')).
lift_operator(('{')).
unlift_operator((')')).
unlift_operator(('}')).
unlift_operator((']')).
operator((':-')).
operator(('<--')).
operator((':')).
operator(('|')).
operator((',')).
operator((';')).


:- initialization
       init_codes(
	   [namespace,
	    (class),
            struct,
            parameter,
            variable,
            method,
            keyword,
            modifier,
            comment,
            string,
            number,
            operator]
		   ).

:- initialization
       init_modifiers([
declaration,
	definition,
	readonly,
	static,
	deprecated,
	abstract,
	async,
	modification,
	documentation,
	defaultLibrary
]).


check_doc(S,D) :-
sub_string(`/** `,0,_,_,S),
!,
modifier(documentation,D).
check_doc(S,D) :-
sub_string(`/**>`,0,_,_,S),
!,
modifier(documentation,D).
check_doc(S,D) :-
sub_string(`%%  `,0,_,_,S),
!,
modifier(documentation,D).
check_doc(S,D) :-
sub_string(`%%> `,0,_,_,S),
!,
modifier(documentation,D).
check_doc(_,0).
