
:- module(scanner, [
    mkgraph/1,
    init_scanner_helper/1,
    close_scanner_helper/1,
    use/10,
    def/8,
    ws/3
]).

:- use_module(library(maplist)).

:- dynamic( [compiling/1,  use/10, def/8, ws/3] ).
:- multifile( [compiling/1,  use/10, def/8, ws/3] ).

mkgraph(F) :-
    init_scanner_helper(Loop),
    compile(F),    
    close_scanner_helper(Loop).
    
init_scanner_helper(Loop) :-
    prolog_flag(compiler_top_level, Loop),
    set_prolog_flag(compiler_top_level, scanner:scanner_loop).

close_scanner_helper(Loop) :-
    set_prolog_flag(compiler_top_level, Loop).


scanner_loop(Stream,Status) :-
       stream_property(Stream,file_name(File)),
    retractall(scanner:use(_,_F,_MI,_F0,_Mod,File,_S0,_E0,_S1,_E1)),
retractall(scanner:def(_,_,_,File,_,_,_,_)),
retractall(scanner:dec(_Dom,_F,_M,File,_B,_E,_BT,_FiET)),
retractall(scanner:ws(File,_,_)),
	retractall(compiling(_)),
    repeat,
    catch(
	 c_clause(Stream,Status,File),
	 _Error,
	prolog:error_handler),
    !.

c_clause(Stream,Status,File) :- 
    prompt1(': '), prompt(_,'     '),
    Options = [syntax_errors(dec10),variable_names(Vars),term_position(Pos),scan(L)],
    read_clause(Stream, Clause, Options),
    (
	Clause    == end_of_file
    ->
    !
    ;
    preprocess_tokens(L,File),
    compilation_steps(Clause, Status, Vars, Pos, L),
    fail
    ).

preprocess_tokens([],_) :- !.
preprocess_tokens([t(_,L,_,_),t(_,L1,C,_)|_],File) :-
integer(L1),
	L < L1,
     assert(ws(File,L1,C)),
     fail.
preprocess_tokens([_H|T],File) :-
	preprocess_tokens(T,File).




compilation_steps(Clause, _Status, _Vars, _Pos, _L) :-
    '$conditional_compilation_skip'(Clause),
    !.
compilation_steps(Clause, _Status, _Vars, _Pos, L) :-
    (
	sync(Clause,Domains,L, [t('EOT',_,_,_)])
    ->
       process_domains(Clause,Domains)
           ;
    writeln(bad_match(Clause))
    ),
    fail.
compilation_steps(Clause, Status, Vars, Pos, _L) :-
    prolog:call_compiler(Clause, Status,Vars,Pos).

sync(V,t(V,[],L-C,L-CF,L-C,L-CF)) -->
    {var(V)},
    !,
    [t(var(V,_),L,C,Sz)],
    {CF is C+Sz}.

sync(T,t(T,[NA1],EL-EF,EL-EFL,Start,EL-EFL)) -->
    {functor(T,A,1),
     current_op(_,Type,A),
    (Type == xf ->true;
     Type == yf),
     T=..[A,A1],
     NA1=t(_,_,_,_,Start,_)},
    sync(A1,NA1),
    [t(atom(A),EL,EF,Sz)],
{EFL is EF+Sz}.
sync((A1,A2),t((A1,A2) ,[NA1,NA2],BL-BF,BL-BFL,Start,End)) -->
    sync(A1,NA1),
    [t( ',',BL,BF,Sz)],
	{BFL is BF+Sz,
	NA1=t(_,_,_,_,Start,_),
     	NA2=t(_,_,_,_,_,End)
      },
	sync(A2,NA2).
sync((A1;A2),t((A1;A2) ,[NA1,NA2],BL-BF,BL-BFL,Start,End)) -->
    sync(A1,NA1),
    [t( ';',BL,BF,Sz)],
	{BFL is BF+Sz,
	NA1=t(_,_,_,_,Start,_),
     	NA2=t(_,_,_,_,_,End)
      },
	sync(A2,NA2).
sync(T,t(T,[NA1,NA2],BL-BF,BL-BFL,Start,End)) -->
    {functor(T,A,2),
    current_op(_,Type,A),
    (Type == xfx ->true;
      Type == yfx -> true;
      Type == xfy -> true;
      Type == yfy
      ),
      T=..[A,A1,A2]
      },
    sync(A1,NA1),
    [t(atom(A),BL,BF,Sz)],
    {BFL is BF+Sz,
	NA1=t(_,_,_,_,Start,_),
     NA2=t(_,_,_,_,_,End)
     },
    sync(A2,NA2).
sync(T,t(T,[NA1],StartL-StartC,StartL-StartF,StartL-StartC,End)) -->
    [t(atom(A),StartL,StartC,Sz)],
    {
    current_op(_,Type,A),
    StartF is StartC+Sz,
(Type == fx ->true;Type == fy),
	T=..[A,A1],
	NA1=t(A1,_,_,_,_,End)
    },
    sync(A1,NA1).
sync(T,t(T,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    [t(atom(-),SL0,_SC0,_Sz),t(number(NT),SL0,SC1,Sz)],
    {number(T),
     NT is -T,
    SCF is SC1+Sz}.
sync(T,t(T,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    {atom(T)},
    [t(atom(T),SL0,SC0,Sz)],
    {SCF is SC0+Sz}.
sync(T,t(T,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    {number(T)},
    !,
    [t(number(T),SL0,SC0,Sz)],
    {SCF is SC0+Sz}.

sync(T,NT) -->
    [t('(',_,_,_)],
    !,
    sync(T,NT),
    [t(')',_,_,_)].
sync({T},t({T},[NT],SL-SC,SL-SC1,SL-SC,EL-EC)) -->
    [t('{',SL,SC,Sz)],
    sync(T,NT),
    [t('}',EL,EC,_)],
    !,
    {SC1 is SL+Sz}.

sync(N/A,t(N/A,[],SL0-SC0,SLF-SClF,SL0-SC0,SLF-SCF)) -->
    [t(atom(N),SL0,SC0,Sz),
    SC1 is SC0+Sz,-
    [t(atom(/),_,_,_),
     t(number(A),SLF,SC1,Sz1)],
    !,
    {SCl is SC1+Sz1}.
sync([H|T],t([H|T],[NH|NT],BL-BF,BL-BFL,Start,End)) -->
    sync(H,NH),
    [t( atom('.'),BL,BF,Sz)],
	{BFL is BF+Sz,
	NH =t(_,_,_,_,Start,_),
     	NT=t(_,_,_,_,_,End)
      },
	sync(T,NT).
sync('()'(A),t('()'(A),[NA],R0,Rf,Start,EBL-EC)) -->
    sync(A,NA),
[t( atom('()'),EBL,BF,Sz)],
    EC is BF+Sz,
    {NA=t(_,_,R0,Rf,Start,_)}.
sync( '[]'(B,A),t('[]'(B,A),[NB,NA],R0,Rf,Start,End)) -->
    sync(A,NA),
    [t( atom('['))],
    sync(B,NB),
    {NA=t(_,_,R0,Rf,Start,End)}.
sync([H|T],t([H|T],NAs,SL-SC,SL-SC1,SL-SC,End)) -->
    [t('[',SL,SC,Sz)],
    !,
    {SC1 is SC+Sz},
    sync_list([H|T],NAs,End).
sync(T,t(T,NT,StartL-StartC,StartL-StartF,StartL-StartC,End)) -->
	  [t(atom(A),StartL,StartC,Sz),
     t(l,_,_,_)],
    {functor(T,A,N), N > 0 },
    !,
    { StartF is StartC+Sz},
    sync_args(1,N,T,NT,End). 


sync(_T,t(T,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    !,
    [t((T),SL0,SC0,Sz)],
    {SCF is SC0+Sz}.
    

sync_list([H|T],[NH|NTs],End) -->
    sync(H,NH),
    sync_tail(T,NTs,End).

sync_tail([],[],EL-ECF) -->
[t(']',EL,EC,Sz)],
{ECF is EC+Sz},
!.
sync_tail(L,NL,End) -->
[t(',',_,_,_)],
!,
sync_list(L,NL,End).
sync_tail(L,NL,EL-ECF) -->
[t('|',_,_,_)],
!,
sync(L,NL),
[t(']',EL,EC,Sz)],
{ECF is EC+Sz}.

sync_args(N,N,T,[NA],EL-ECF)-->
    !,
{arg(N,T,AT)},
sync(AT,NA),
[t(')',EL,EC,Sz)],
{ECF is EC+Sz}.

sync_args(N,NF,T,[NA|NT],End)-->
{arg(N,T,AT),
N1 is N+1},
sync(AT,NA),
[t(',',_EL,_EC,_)],
 sync_args(N1,NF,T,NT,End).

process_domains((:- Directive),t(_,[Child],_,_,_,_)):-
		    Child = t(Directive,Domains,B0,E0,Begin,End ),
    decs(Directive, Domains, B0, E0, Begin, End).                                                                                                                                                                           
process_domains((H0:-B),t(_,[t(H0,_,A0,B0,_,_), BGs],_,_,BT,ET)) :-
!,
stream_property(loop_stream,file_name(File)),		 
     strip_module(H0,Mod,H),	
     functor(H,N,A),
    def_predicate(N/A,Mod,File,A0,B0,BT,ET),
    uses(B,BGs,Mod,N/A).	
process_domains((H0),t(_,[t(H0,_,A0,B0,_,_)],_,_,BT,ET)) :-
     stream_property(loop_stream,file_name(File)),		 
     strip_module(H0,Mod,H),	
    functor(H,N,A),
( system_predicate(N/A) -> OMod = prolog ; OMod = Mod),
     def_predicate(N/A,OMod,File,A0,B0,BT,ET).
     
def_predicate(N/A,Mod,File,_A0,_B0,_BT,ET):-
	compiling(Mod:N/A),
	!,
	retract(def(predicate,N/A,Mod,File,A0,B0,BT,_)),
	assert(def(predicate,N/A,Mod,File,A0,B0,BT,ET)).
def_predicate(N/A,Mod,File,A0,B0,BT,ET):-
    assert(def(predicate,N/A,Mod,File,A0,B0,BT,ET)),
	retractall(compiling(_)),
 assert(	compiling(Mod:N/A)).


decs(module(Mod,_Exports),[t(Mod,_,A0,B0,_,_), t(_,ExportLists,_,_,_,_)],_,_,BT,ET) :-
     stream_property(loop_stream,file_name(File)),
    assert(def(module,export,Mod,File,A0,B0,BT,ET)),
    current_source_module(_,Mod),
    maplist(add_dec(export,Mod,File,BT,ET),ExportLists).
decs(system_module(_Mod,_Exports,_),[t(Mod,_,A0,B0,_,_), t(_,ExportLists,_,_,_,_), t(SysPreds,_,_,_)],_,_,BT,ET) :-
     stream_property(loop_stream,file_name(File)),
    assert(def(module,export,Mod,File,A0,B0,BT,ET)),
    current_source_module(_,Mod),
    maplist(add_dec(export,Mod,File,BT,ET),ExportLists),
    maplist(add_dec(export,prolog,File,BT,ET),SysPreds).

add_dec(export,Mod,File,BT,ET,t(N/A,_,_,_,B,E)) :-
    !,
    assert(dec(predicate_export,N/A,Mod,File,B,E,BT,ET)).
add_dec(_,_,_,_,_,_).

uses(G,_,_,_) :-
		  var(G),			
!.	
uses((G1,G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
 !,
 uses(G1,BG1,Mod,N/A),
 uses(G2,BG2,Mod,N/A).
uses(Mod:G,t(_,[_,BG2],_,_,_,_),_Mod,N/A) :-
 !,
 uses(G,BG2,Mod,N/A).
uses((G1->G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
 !,
 uses(G1,BG1,Mod,N/A),
 uses(G2,BG2,Mod,N/A).
 uses((G1*->G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
 !,
 uses(G1,BG1,Mod,N/A),
 uses(G2,BG2,Mod,N/A).										
 uses((G1;G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
 !,
 uses(G1,BG1,Mod,N/A),
 uses(G2,BG2,Mod,N/A).
uses(G0,BG,Mod,N/A) :-
fail,
strip_module(Mod:G0,M,G),
    predicate_property(M:G,meta_predicate(GM)),
    G=.. [_|As],
    GM =.. [_|Ms],
    maplist(add_use( Mod, N/A, M), Ms,As, BG),
    fail.
uses(G0,t(G0,_,S0,E0,S1,E1),Mod,N0/Ar0) :-
    strip_module(Mod:G0,M,G),
    functor(G,N,Ar),
     stream_property(loop_stream,file_name(File)),
     (system_predicate(N/Ar)
->
MI = prolog
;
follow_imports(Mod,G,MI)
),
    assert(use(predicate,N/Ar,MI,N0/Ar0,M,File,S0,E0,S1,E1)),
    fail.
 uses(_,_,_,_).	

follow_imports(_M,G,prolog) :-
     predicate_property(G,built_in),
     !.
follow_imports(M,G,MI) :-
     predicate_property(M:G,imported_from(M1)),
     !,
     follow_imports(M1,G,MI).
follow_imports(M,_,M).

add_use(Mod, N0/Ar0,M0,M,G,t(G,_,S0,E0,S1,E1)) :-
  number(M),
  !,
  strip_module(M0:G,M1,G1),
  functor(G1,N,Ar),
  ArM is Ar+M,
       stream_property(loop_stream,file_name(File)),
system_predicate(N/Ar) -> MI = prolog ;  follow_imports(M1,G,MI),
     assert(use(predicate,N/ArM,MI,N0/Ar0,Mod,File,S0,E0,S1,E1)).

add_use(_Mod, _,_,_,_,_,_).

