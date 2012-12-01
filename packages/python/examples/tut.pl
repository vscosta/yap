
:- use_module(library(python)).

:- initialization(main).

main :- 
	ex(X),
	flush_output,
	fail.
main.

%
% strings are atoms in the interface
% with usual properties
%
% variables must be used with $
%
% UNICODE is supposed to work (does in Linux).
%
ex(currency) :-
	(
	    currency := '€',
	    O := ord($currency),
	    X := $currency,
	    L := len($currency),
	    format('currency=~a ~d/~d~n',[X, O, L])
	->
	    true
	;
	    failed(currency)
	).

ex(home) :-
	(
	    filename := os:environ:get('HOME')
	->
	    X := $filename,
	    format('HOME=~a~n',[X])
	;
	    true
	).

ex(site) :-
	X := site:getusersitepackages(_),
	format('site packages=~a~n',[X]).

ex(arith) :-
	A := 2+2,
	B := (50-5*6)/4,
	C := 7/3,
	width := 20,
	height := 5*9,
	D := $width* $height,
	format('arith=~d ~d ~d ~d~n',[A,B,C,D]).

ex(undefined) :-
	format('undefined variable~n', []),
	X := $n,
	format('undefined=~d',[X]).

ex(fp) :-
	X := 3 * 3.75 / 1.5,
	Y := 7.0 / 2,
	format('fp=~f ~f~n',[X,Y]).

ex(complex) :-
	A := complex(0,1) * complex(0,1),
	B := complex(3,1)*3,
	a := complex(1.5,0.5),
	R := $a:real,
	I := $a:imag,
	format('complex=~w ~w ~w+~wj~n',[A,B,R,I]).

ex(floatint) :-
	A := float(22000),
	B := int(3.1),
	C := long(15000000.5),
	format('cast=~w ~w ~w~n',[A,B,C]).

ex(strings) :-
	S1 := 'spam eggs',
	S2 := 'doesn\'t',
	S3 := '"Yes," he said.',
	S4 := '"Isn\'t," she said.',
	format('s=~a ~a ~a ~a~n',[S1,S2,S3,S4]),
	hello := 'This is a rather long string containing\n\
several lines of text just as you would do in C.\n\
    Note that whitespace at the beginning of the line is\
 significant.',
	python_command('print hello'),
	X := $hello,
	format('s=~a~n',[X]).

ex(strings2) :-
	word := 'Help' + 'A',
	X := '<' + $word*5 + '>',
	Y := (str:strip) + ing,
	A1 := $word^[4],
	A2 := $word^[0:2],
	A3 := $word^[2:4],
	format('concat=~a ~a ~a ~a ~a~n',[X,Y,A1,A2,A3]).

ex(slices) :-
	s := 'supercalifragilisticexpialidocious',
	L := len($s),
	S1 := $s^[1:6],
	S2 := $s^[-6: -1],
	S3 := $s^[_:6],
	S4 := $s^[-6:_],
	format('slices=~d ~a ~a ~a ~a~n',[L,S1,S2,S3,S4]).


ex(lists) :-
	a := [66.25, 333, 333, 1, 1234.5],
        A1 := $a:count(333), A2 := $a:count(66.25), A3 := $a:count(x),
	format('counts=~d ~d ~d~n',[A1,A2,A3]),
	:= $a:insert(2, -1),
	:= $a:append(333),
	A := $a,
	format('a=~w~n', [A]),
	I := $a:index(333),
	:= $a:remove(333),
	B := $a,
	format('a=~w~n', [B]),
	:= $a:reverse(_),
	C := $a,
	format('a=~w~n', [C]),
	:= $a:sort(_),
	D := $a,
	format('a=~w~n', [D]).

ex(iter) :-
	it := iter(abc),
	format('iter= ', []),
	iterate(iter).

iterate(iter) :-
	repeat,
	( X1 := $it:next,
	  format('i ~a~n', [X1])
	->
	  fail
	;
	  !
	).

ex(range) :-
	r1 := range(1000),
	r2 := range(1000,2000),
	r3 := range(2000,10000,1),
	S := sum($r1+ $r2+ $r3),
	format('range=~d~n', [S]).


