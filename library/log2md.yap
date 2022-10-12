/**
 * @File.yap
 * @author Vitor Santos Costa
 *
 *
 */

:- module( log2md,
	   [log/1,
	    logger/0,
	    logger/1,
     log_goal/1,
     log_message/1,
log_table/2] ).

:- multifile user:event_hook/3.

:- initialization logger.

/**
 * @defgroup Log2MD Log Output of Tests in Markdown format.
 *
 * @ingroup YAPLibrary
 * @{
 *  These primitives support writing a user-specified log of execution to an
 *  output file. The output file can be used for testing or debugging.
 *
 *  - Primitives include the ability to write a title, a Prolog clause or
 *  goal, and hooks for tracing calls. The log_goal/2 can be used to
 *  start a goal. Arguments of the form `<--/2` and `*->/2` can be used to
 *   track calls.
 *
 * - The output format is markdown.
 */

:- use_module( library( maplist) ).
:- use_module( library( lists) ).
:- use_module( library( system ) ).
:- use_module( library( gensym ) ).

:- meta_predicate goal(0,?,?), log(0).

:- dynamic level/1, logging/3.

start_logging(F) :-
    statistics(walltime,[_,W]),
    statistics(runtime,[_,T]),
    retractall(logging(_,_,_,_)),
    assert(logging(log,F,W,T)),
    open( F, write, _Out, [alias(log)]).

restart_logging :-
    stream_property(_,alias(log)),
    !.
restart_logging :-
    logging(log,F,_,_),
    open( F, append, _Out, [alias(log)]).

  /**
   * @pred log_title( +String ) is det
   *
   * @param [in] S is a Prolog atom or string describing a title.

   */
logger :-
    logger([output('log.md')]).

logger( LogFlags ) :-
    member(output(F), LogFlags),
    !,
    start_logging(F),
    retractall(level(_)),
    assert(level(2)),
    working_directory(Dir,Dir),
    datime( datime(Y,M,D,H,Mi,Sec) ),
    prolog_load_context( file, Name ),
    outln( '# YAP Execution LOG~n~nInfo:~n~n- Started at ~a~n~n- called from ~a;~n~n- logging to ~a;~n~n- run at ~d/~d/~d ~d:~d:~d.~n~n', [Dir,Name,F,Y,M,D,H,Mi,Sec]).

log_message(Format) :- event(message(Format),[],_).

log_table(Table,Headers) :-
    event(table(Table),Headers,_).

log_run_goal( Goal ) :-
    gated_call(
	goal(Goal, Source, Id),
    	Goal,
	Event,
	event(port(Event), Source, Id)
    ).

goal(G0, Source , Id) :-
    (G0 =call(G) -> true ; G0 = G),
    strip_module(G,_,Goal),
    functor(Goal,A, _N),
    atomic_concat(['goal-',A,'-'],Prefix),
    gensym(Prefix,Id),
    level(Level),
    Lev is min(6,Level),
    atom_concat(Prefix,I,Id),
    LA is "#",
    atomic_concat([A,' ',I],Source),
    outln('~n~*c Goal ~s',[Lev,LA, Source]),
    outln( '```'),
    portray_clause( user_error , Goal ),
    portray_clause(        log , Goal ),
    outln( '```'),
    event(port(call),Source, Id).

timestamp(Time) :-
    logging(log,_,W0,T0),
    statistics(walltime,[_,W]),
    statistics(runtime,[_,T]),
    DT is T-T0,
    DW is W-W0,
    format(string(Time), 'walltime=~w runtime=~w',[DW,DT]).

event(message(Message), _Name, _Id) :-
    timestamp(Time),
    X is "#",
    level(Level),
    L is Level+1,
    outln('~n~*c ~s  @~s',[L,X,Message, Time]).
event(table(Data), Headers, _Id) :-
    new_row(Headers),
    border_row(Headers),
    maplist(new_row,Data).
event(port(Port), Name, Id) :-
    timestamp(Time),
    outln('~n~n~w: [~s](#~s) @ ~s',[Port,Name, Id, Time]),
%    ignore( user:event_hook(port(Port), Type, Id) ),
    across_port(Port).

new_row(Row) :-
    out('|'),
    maplist(cell,Row),
    outln.

border_row(Headers) :-
    out('|'),
    maplist(border_cell,Headers),
    outln.

border_cell(_) :-
    out('___|').

cell(El) :-
    out('~q', [El]),
    out('|').

across_port(call) :- push_level.
across_port(exit) :- pop_level.
across_port(fail) :- pop_level.
across_port(redo) :- push_level.
across_port(answer) :- pop_level.
across_port(!) :- pop_level.
across_port(exception(_)) :- pop_level.
across_port(external_exception(_)) :- pop_level.

pop_level :-
    retract(level(Level)),
    L1 is Level-1,
    assert(level(L1)).
push_level :-
    retract(level(Level)),
    L1 is Level+1,
    assert(level(L1)).


  /**
   * @pred out(+Format, +Args)
   *
   * @param [in] format the string given Args . The output is sent to
   * user_error and to a stream with alias `log`;
   *
   */
out(Format, Args) :-
     format(        log, Format, Args),
     format( user_error, Format, Args).

out(Format) :-
    format(        log, Format, []),
    format( user_error, Format, []).

outln(Format, Args) :-
    out(Format, Args), out('~n').
outln(Format) :-
    out(Format), out('~n').
outln :-
    out('~n').

log_goal(Goal) :-
    assert(user:goal_expansion(Goal,log2md:log_run_goal(Goal))).

%% @}

:- if(true).

:- log_goal( append(_,_,_L) ).

:- log_goal( app([_,_,_|_],_,_L) ).

main :-
    catch(
	run_test(I),
	Error,
	format('~w: error ~w', [I,Error])
    ),
    fail.
main.

app([],L,L).
app([H|L],L2,[H|NL]) :- app(L,L2,NL).

run_test(I) :-
    ( log2md:main(I) *-> format('~w OK~n' , [I]) ; format('~w BAD~n' , [I]) ), fail.

main(0) :- logger.

main(2) :- length(L2,3), append([1,2,3],L2,_).

main(2) :- length(L2,2), append([1,2],_,L2).

main(2) :- length(L2,10), append(L2,[1,2,3,4],L2).

main(3) :- log_message("Hello" ).

main(4) :- log_table([[1,2,3],[2,3,1],[3,1,2]],[first,second,third]).

main(5) :- length(L2,10), app(_,_,L2).

:- initialization(main).


:-endif.
