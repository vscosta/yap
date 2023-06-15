
:- module(scanner, [
    scanner_loop/2
]).


scanner_loop(Stream,Status) :-
    repeat,
    catch(
	 compile_clause(Stream,Status),
	 _Error,
	prolog:error_handler),
    !.

compile_clause(Stream,Status) :-
    prompt1(': '), prompt(_,'     '),
    Options = [syntax_errors(dec10),variable_names(Vars), term_position(Pos),scan(L)],
    read_clause(Stream, Clause, Options),
    (
	Clause == end_of_file
    ->
    !
    ;
    '$conditional_compilation_skip'(Clause)
    ->
    fail
    ;
    (
      sync(Clause,Domains,L, [])
    ->
       process_domains(Clause,Domains)
;
    writeln(bad_match(Clause))
),
    prolog:call_compiler(Clause, Status,Vars,Pos),
    fail
	).

sync(V,[t(V,L,C,_)]) -->
{var(V)},
!,
[t(var(V,_),L,C)].
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
{functor(T,A,N)},
[t(A,StartL,StartC,_),
 t(l,_,_,_)],
 !,
{functor(NT,A,N)},
 sync_args(1,N,T,NT,End).

sync(T,t(T,[NA1],StartL-StartC,End)) -->
[t(A,StartL,StartC,_)],
{(current_op(fx,_,A)->true;current_op(fy,_,A)),
 T=..[A,A1],
 NA1=t(A1,_,_,End)},
 sync(A1,NA1).
sync(T,t(T,[NA1],Start,EL-EF)) -->
{functor(T,A,1),
 (current_op(xf,_,A)->true;current_op(yf,_,A)),
 T=..[A,A1],
 NA1=t(_,_,Start,_)},
 sync(A1,NA1),
 [t(atom(A),EL,EF,_)].
sync(T,t(T,[NA1,NA2],Start,End)) -->
{functor(T,A,2),
 current_op(_,_,A),
 T=..[A,A1,A2],
 NA1=t(_,_,Start,_),
 NA2=t(_,_,_,End)},
 sync(A1,NA1),
 [t(atom(A),_,_,_)],
 sync(A2,NA2).
sync([H|T],t([H|T],NAs,SL-SC,End)) -->
 [t('[',SL,SC,_)],
 sync_list([H|T],NAs,End).

sync_list([H|T],[NH|NTs],End) -->
sync(H,NH),
sync_tail(T,NTs,End).

sync_tail([],[],EL-EC) -->
[t(']',EL,EC,_)],
!.
sync_tail(L,NL,End) -->
[t(',',_,_,_)],
!,
sync_list(L,NL,End).
sync_tail(L,NL,EL-EC) -->
[t('|',_,_,_)],
!,
sync(L,NL),
[t(']',EL,EC,_)].

sync_args(N,N,T,[NA],EL-EC)-->
{arg(N,T,A)},
sync(A,NA),
[t(')',EL,EC,_)].

sync_args(N,NF,T,[NA|NT],End)-->
{arg(N,T,A),
N1 is N+1},
sync(A,NA),
[t(',',_EL,_EC,_)],
 sync_args(N1,NF,T,NT,End).

      process_domains((:- _, t(, )
