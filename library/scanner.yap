
:- module(scanner, [
    mkgraph/1
]).

:- use_module(library(maplist)).

mkgraph(F) :-
    prolog_flag(compiler_top_level, Loop),
    set_prolog_flag(compiler_top_level, scanner:scanner_loop),
    compile(F),
    set_prolog_flag(compiler_top_level, Loop).

scanner_loop(Stream,Status) :-
    repeat,
    catch(
	 scanner:c_clause(Stream,Status),
	 _Error,
	prolog:error_handler),
    !.

c_clause(Stream,Status) :-
    prompt1(': '), prompt(_,'     '),
    Options = [syntax_errors(dec10),variable_names(Vars), term_position(Pos),scan(L)],
    read_clause(Stream, Clause, Options),
    (
	Clause == end_of_file
    ->
    !
    ;
    compilation_steps(Clause, Status, Vars, Pos, L),
    fail
    ).


compilation_steps(Clause, _Status, _Vars, _Pos, _L) :-
    '$conditional_compilation_skip'(Clause),
    !.
compilation_steps(Clause, Status, Vars, Pos, L) :-
    (
	sync(Clause,Domains,L, L0)
    ->
       process_domains(Clause,Domains)
    ;
    writeln(bad_match(Clause))
    ),
    prolog:call_compiler(Clause, Status,Vars,Pos).

sync(V,[t(V,L,C,_)]) -->
    {var(V)},
    !,
    [t(var(V,_),L,C,_)].
sync(T,NT) -->
    [t('(',_,_,_)],
    !,
    sync(T,NT),
    [t(')',_,_,_)].
sync({T},t({T},NT,SL-SC,EL-EC)) -->
    [t('{',SL,SC,_)],
    sync(T,NT),
    [t('}',EL,EC,_)],
    !,
    sync(T,NT),
    [t(')',_,_,_)].
sync(T,t(T,NT,StartL-StartC,End)) -->
    [t(atom(A),StartL,StartC,_),
     t(l,_,_,_)],
    !,
    {functor(T,A,N)},
    sync_args(1,N,T,NT,End).
sync(N/A,t(N/A,[],SL0-SC0,SLF-SCF)) -->
    [t(atom(N),SL0,SC0,_Sz),
     t(atom(/),_,_,_),
     t(number(A),SLF,SC1,Sz1)],
    !,
    {SCF is SC1+Sz1}.
sync([H|T],t([H|T],NAs,SL-SC,End)) -->
    [t('[',SL,SC,_)],
    !,
    sync_list([H|T],NAs,End).

sync(T,t(T,[NA1],StartL-StartC,End)) -->
    [t(atom(A),StartL,StartC,_)],
    {
	(current_op(_,fx,A)->true;current_op(_,fy,A)),
	T=..[A,A1],
	NA1=t(A1,_,_,End)
    },
    sync(A1,NA1).

sync(T,t(T,[NA1],Start,EL-EFL)) -->
    {functor(T,A,1),
     (current_op(_,xf,A)->true;current_op(_,yf,A)),
     T=..[A,A1],
     NA1=t(_,_,Start,_)},
    sync(A1,NA1),
    [t(atom(A),EL,EF,Sz)],
{EFL is EF+Sz}.
sync(T,t(T,[NA1,NA2],Start,End)) -->
    {functor(T,A,2),
     current_op(_,_,A),
     T=..[A,A1,A2],
     NA1=t(_,_,Start,_),
     NA2=t(_,_,_,End)},
    sync(A1,NA1),
    [t(atom(A),_,_,_)],
    sync(A2,NA2).
sync(T,t(T,[], SL0-SC0,SL0-SCF)) -->
    !,
    [t(atom(T),SL0,SC0,Sz)],
    {SCF is SC0+Sz}.
sync(T,t(T,[], SL0-SC0,SL0-SCF)) -->
    !,
    [t(number(T),SL0,SC0,Sz)],
    {SCF is SC0+Sz}.
sync(_T,t(T,[], SL0-SC0,SL0-SCF)) -->
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

process_domains((:- Directive), t(_,t(Directive,Domains,Begin,End ),_,_)) :-
    decs(Directive, Domains, Begin, End).


decs(module(Mod,_Exports),t(_,[t(Mod,_,B,E), t(_,ExportLists,_,_)]),BT,ET) :-
    stream_property(loop_stream,file_name(File)),
    assert(dec(module,export,Mod,File,B,E,BT,ET)),
    maplist(dec(export,Mod,BT,ET),ExportLists).

dec(export,Mod,BT,ET,t(N/A,_,B,E)) :-
    assert(dec(predicate_export,N/A,Mod,B,E,BT,ET)).
dec(export,Mod,BT,ET,t(op(_,_,_A),_,B,E)) :-
    assert(use(predicate_export,op/3,Mod,B,E,BT,ET)).

