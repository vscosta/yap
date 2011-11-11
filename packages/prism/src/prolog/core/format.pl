%%--------------------------------
%%  Entry Point

$pp_format_message(Format,Args) :-
    $pp_format_message_loop(Format,Args).


%%--------------------------------
%%  Main Loop

$pp_format_message_loop([],_) :- !.
$pp_format_message_loop(Format,Args) :-
    Format = [Next|Format0],
    ( Next == 0'{ -> % '
      $pp_format_message_loop(Format0,Format1,Args)
    ; Next == 0'~ -> % '
      Format0 = [Code|Format1], $pp_format_message_char(Code)
    ; %% else
      Format0 = Format1, $pp_format_message_char(Next)
    ), !,
    $pp_format_message_loop(Format1,Args).

$pp_format_message_loop(Format0,Format1,Args) :-
    $pp_format_message_spec(Format0,Format1,N),
    nth(N,Args,Arg), !,
    $pp_format_message_term(Arg).
$pp_format_message_loop(Format0,Format0,_Args) :-
    $pp_format_message_char(0'{). % '


%%--------------------------------
%%  Format Spec Extraction

$pp_format_message_spec(Format0,Format1,N) :-
    $pp_format_message_spec(Format0,Format1,[],Codes),
    number_codes(N,Codes).

$pp_format_message_spec(Xs0,Xs1,Ys,Ys) :-
    Xs0 = [0'}|Xs1], !. % '
$pp_format_message_spec(Xs0,Xs1,Zs0,Ys) :-
    Xs0 = [X|Xs2],
    integer(X),
    X >= 48,
    X =< 57,
    Zs1 = [X|Zs0], !,
    $pp_format_message_spec(Xs2,Xs1,Zs1,Ys).


%%--------------------------------
%%  Output

$pp_format_message_char(Code) :-
    format("~c",[Code]).
$pp_format_message_term(Term) :-
    format("~w",[Term]).
