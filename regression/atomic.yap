:- use_module(library(lists)).

main :- 
    preds(Goals),
    member(P, Goals),
    main(P).
    main.

main(P) :-
    terms(Terms),
    member(A1,Terms),
    member(A2, Terms),
    G =.. [P,A1,A2],
    term_variables(G, Vs),
    writeln(user_error,G-Vs),
    catch(G,E,(writeln(user_error,error=E),fail)),
    writeln(user_error,Vs),
    fail.

preds([name,
    string_to_atomic,
    atomic_to_string,
    string_to_atom,
    string_to_list, 
    atom_string
]).

terms([
    X,
    _Y,
    a(X),
    11,
    -11,
    111111111111111111111111111111111,
    3.3,
    3.0,
    "aaaaa",
    "",
    [a,a,a,a,a,a,a],
    `aaaaaaaaaaaaa`,
    ``,
    aaaaaaaaaaaaaaa,
    ''
]).
