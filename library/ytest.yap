
:- module( ytest, [run_test/1,
                   run_tests/0,
                   test_mode/0,
                   op(1150, fx, test),
                   op(995, xfx, given),
                   op(990, xfx, returns)] ).

:- use_module( library(clauses) ).
:- use_module( library(maplist) ).
:- use_module( library(gensym) ).
:- use_module( library(lists) ).

:- multifile test/1.

:- dynamic error/3, failed/3.

test_mode.

user:term_expansion( test( (A, B) ), ytest:test( Lab, Cond, Done ) ) :-
    info((A,B), Lab, Cond , Done ).

run_tests :-
    source_module(M),
    run_test(_Lab,M),
    fail.
run_tests :-
    show_bad.

run_test(Lab, M) :-
    test(Lab, (G returns Sols given Program ), Done),
    ensure_ground( Done),
    format('~w  :    ',[ Lab ]),
    reset( Streams ),
    assertall(Program, Refs),
    conj2list( Sols, LSols ),
%    trace,
    catch( do_returns(M:G, LSols, Lab), Ball,  end( Ball ) ),
    shutdown( Streams, Refs ).
run_test(Lab,M) :-
    test(Lab, (G returns Sols ), Done),
    ensure_ground( Done),
    format('~w  :    ',[ Lab ]),
    reset( Streams ),
    conj2list( Sols, LSols ),
%    trace,
    catch( do_returns(M:G, LSols, Lab), Ball,  end( Ball ) ),
    shutdown( Streams, _ ).

info((A,B), Lab, Cl, G) :- !,
    info(A, Lab, Cl, G),
    info(B, Lab, Cl, G).
info(A, _, _, _) :- var(A), !.
info(A returns B, _, (A returns B), g(_,ok)) :- !.
info(A, A, _, g(ok,_)) :- primitive(A), !.
info(_A, _, _, _).

do_returns(G0 ,  Sols0, Lab ) :-
    counter(I),
    fetch(I, Sols0, Pattern0, Next),
     Pattern0 = ( V0 =@= Target0 ),
     copy_term(G0-V0, G-VGF),
     catch( answer(G, VGF, Target0, Lab, Sol) , Error, Sol = error(G, Error) ),
     step( _I, Sols, G0, Sol, Lab ),
    !.

answer(G, V, Target0, Lab, answer(G)) :-
    call(G),
    ( V =@= Target0
    ->
      success(Lab, V)
    ;
      failure(V, Target0, Lab)
    ).

step( I, Sols , G0, Sol, Lab ) :-
    inc(I),
    fetch(I, Sols, Pattern, Next),
    ( Sol = answer(_)
    ->
      true
    ;
      Sol = error(_, Error)
    ->
      (
       nonvar(Pattern ) ,
       Pattern = ( Error -> G),
       G
      ->
       success
      ;
       error(I, G0, Error, Pattern, Lab )
      )
    ),
    (
     Next == ...  -> throw( done )
    ;
      Next == []  -> throw( done )
    ).
    % fail

success( _, _) :-
    write('.'),
    flush_output.

error(_, G, E, _ , Lab) :-
    write('X'),
    flush_output,
    assert( error(Lab,E,G) ).

failure( G, Pattern, Lab) :-
    write('X'),
    flush_output,
    assert(failed(Lab, G, Pattern)).


reset( _ ) :-
    nb_setval( counter,( 0 ) ).

inc( I ) :-
    nb_getval( counter,( I ) ),
    I1 is I+1,
    nb_setval( counter,( I1 ) ).

counter( I ) :-
    nb_getval( counter,( I ) ).


shutdown( _Streams, Refs ) :-
                                %    close_io( Streams ).
    maplist( erase, Refs ).

test_error( Ball, e( Ball ) ).

fetch( 0, [ A  ], A, []) :-
    !.
fetch( 0, [ A, B | _ ], A, B) :-
    !.
fetch( I0, [ _ | L ] , A, B) :-
    I0 > 0,
    I is I0-1,
    fetch( I, L, A, B ).

show_bad :-
    error( Lab, E , B),
    numbervars(E, 1, _),
    format(  '~n~n~w: error, error  ~w.~n', [Lab, E] ),
    writeln( error: E: B ),
    fail.
show_bad :-
    failed( Lab, V, P ),
    numbervars(V, 1, _),
    numbervars(P, 1, _),
    format(  '~n~n~w: failed,  ~w =\\@= ~w.~n', [Lab, V, P] ),
    fail.
show_bad.

end(done) :-
    !,
    nl,
    fail.
end(Ball) :-
    writeln( bad:Ball ).

assertall(Cls, Refs) :-
    conj2list(Cls, LCls),
    maplist( assert, LCls, Refs).

ensure_ground( g(Lab,Ok)) :-
        ground(Ok),
        gensym( tmp_, Lab ).
ensure_ground( g(Lab,Ok)) :-
        ground(Ok),
        ground(Lab).
