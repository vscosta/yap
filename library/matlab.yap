/**
 * @file   matlab.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 22:51:48 2015
 * 
 * @brief  YAP Matlab interface.
 * 
 * 
*/


:- module(matlab,
	  [start_matlab/1,
	   close_matlab/0,
	   matlab_on/0,
	   matlab_eval_string/1,
	   matlab_eval_string/2,
	   matlab_cells/2,
	   matlab_cells/3,
	   matlab_initialized_cells/4,
	   matlab_zeros/2,
	   matlab_zeros/3,
	   matlab_zeros/4,
	   matlab_matrix/4,
	   matlab_vector/2,
	   matlab_vector/3,
	   matlab_set/4,
	   matlab_get_variable/2,
	   matlab_item/3,
	   matlab_item/4,
	   matlab_item1/3,
	   matlab_item1/4,
	   matlab_sequence/3,
	   matlab_call/2]).

/** @defgroup matlab MATLAB Package Interface
@ingroup library
@{

The MathWorks MATLAB is a widely used package for array
processing. YAP now includes a straightforward interface to MATLAB. To
actually use it, you need to install YAP calling `configure` with
the `--with-matlab=DIR` option, and you need to call
`use_module(library(lists))` command.

Accessing the matlab dynamic libraries can be complicated. In Linux
machines, to use this interface, you may have to set the environment
variable <tt>LD_LIBRARY_PATH</tt>. Next, follows an example using bash in a
64-bit Linux PC:

~~~~~
export LD_LIBRARY_PATH=''$MATLAB_HOME"/sys/os/glnxa64:''$MATLAB_HOME"/bin/glnxa64:''$LD_LIBRARY_PATH"
~~~~~
where `MATLAB_HOME` is the directory where matlab is installed
at. Please replace `ax64` for `x86` on a 32-bit PC.

*/

/*

 @pred start_matlab(+ _Options_) 


Start a matlab session. The argument  _Options_ may either be the
empty string/atom or the command to call matlab. The command may fail.

 
*/

/** @pred close_matlab 


Stop the current matlab session.

 
*/
/** @pred matlab_cells(+ _SizeX_, + _SizeY_, ? _Array_)

MATLAB will create an empty array of cells of size  _SizeX_ and
 _SizeY_, and if  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `cells`.

 
*/
/** @pred matlab_cells(+ _Size_, ? _Array_) 


MATLAB will create an empty vector of cells of size  _Size_, and if
 _Array_ is bound to an atom, store the array in the matlab
variable with name  _Array_. Corresponds to the MATLAB command `cells`.

 
*/
/** @pred matlab_eval_string(+ _Command_) 


Holds if matlab evaluated successfully the command  _Command_.

 
*/
/** @pred matlab_eval_string(+ _Command_, - _Answer_)

MATLAB will evaluate the command  _Command_ and unify  _Answer_
with a string reporting the result.

 
*/
/** @pred matlab_get_variable(+ _MatVar_, - _List_) 


Unify MATLAB variable  _MatVar_ with the List  _List_.

 
*/
/** @pred matlab_initialized_cells(+ _SizeX_, + _SizeY_, + _List_, ? _Array_) 


MATLAB will create an array of cells of size  _SizeX_ and
 _SizeY_, initialized from the list  _List_, and if  _Array_
is bound to an atom, store the array in the matlab variable with name
 _Array_.

 
*/
/** @pred matlab_item(+ _MatVar_, + _X_, + _Y_, ? _Val_)

Read or set MATLAB  _MatVar_( _X_, _Y_) from/to  _Val_. Use
`C` notation for matrix access (ie, starting from 0).

 
*/
/** @pred matlab_item(+ _MatVar_, + _X_, ? _Val_) 


Read or set MATLAB  _MatVar_( _X_) from/to  _Val_. Use
`C` notation for matrix access (ie, starting from 0).

 
*/
/** @pred matlab_item1(+ _MatVar_, + _X_, + _Y_, ? _Val_)

Read or set MATLAB  _MatVar_( _X_, _Y_) from/to  _Val_. Use
MATLAB notation for matrix access (ie, starting from 1).

 
*/
/** @pred matlab_item1(+ _MatVar_, + _X_, ? _Val_) 


Read or set MATLAB  _MatVar_( _X_) from/to  _Val_. Use
MATLAB notation for matrix access (ie, starting from 1).

 
*/
/** @pred matlab_matrix(+ _SizeX_, + _SizeY_, + _List_, ? _Array_) 


MATLAB will create an array of floats of size  _SizeX_ and  _SizeY_,
initialized from the list  _List_, and if  _Array_ is bound to
an atom, store the array in the matlab variable with name  _Array_.

 
*/
/** @pred matlab_on 


Holds if a matlab session is on.

 
*/
/** @pred matlab_sequence(+ _Min_, + _Max_, ? _Array_) 


MATLAB will create a sequence going from  _Min_ to  _Max_, and
if  _Array_ is bound to an atom, store the sequence in the matlab
variable with name  _Array_.

 
*/
/** @pred matlab_set(+ _MatVar_, + _X_, + _Y_, + _Value_) 


Call MATLAB to set element  _MatVar_( _X_,  _Y_) to
 _Value_. Notice that this command uses the MATLAB array access
convention.

 
*/
/** @pred matlab_vector(+ _Size_, + _List_, ? _Array_) 


MATLAB will create a vector of floats of size  _Size_, initialized
from the list  _List_, and if  _Array_ is bound to an atom,
store the array in the matlab variable with name  _Array_.

 
*/
/** @pred matlab_zeros(+ _SizeX_, + _SizeY_, + _SizeZ_, ? _Array_)

MATLAB will create an array of zeros of size  _SizeX_,  _SizeY_,
and  _SizeZ_. If  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `zeros`.




 */
/** @pred matlab_zeros(+ _SizeX_, + _SizeY_, ? _Array_)

MATLAB will create an array of zeros of size  _SizeX_ and
 _SizeY_, and if  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `zeros`.

 
*/
/** @pred matlab_zeros(+ _Size_, ? _Array_) 


MATLAB will create a vector of zeros of size  _Size_, and if
 _Array_ is bound to an atom, store the array in the matlab
variable with name  _Array_. Corresponds to the MATLAB command
`zeros`.

 
*/

:- ensure_loaded(library(lists)).

tell_warning :-
	print_message(warning,functionality(matlab)).

:- ( catch(load_foreign_files([matlab], ['eng','mx','ut'], init_matlab),_,fail) -> true ; tell_warning).

matlab_eval_sequence(S) :-
	atomic_concat(S,S1),
	matlab_eval_string(S1).

matlab_eval_sequence(S,O) :-
	atomic_concat(S,S1),
	matlab_eval_string(S1,O).

matlab_vector( Vec, L) :-
	length(Vec, LV),
	matlab_vector(LV, Vec, L).

matlab_sequence(Min,Max,L) :-
	mksequence(Min,Max,Vector),
	Dim is (Max-Min)+1,
	matlab_matrix(1,Dim,Vector,L).

mksequence(Min,Min,[Min]) :- !.
mksequence(Min,Max,[Min|Vector]) :-
	Min1 is Min+1,
	mksequence(Min1,Max,Vector).

matlab_call(S,Out) :-
	S=..[Func|Args],
	build_args(Args,L0,[]),
	process_arg_entry(L0,L),
	build_output(Out,Lf,['= ',Func|L]),
	atomic_concat(Lf,Command),
	matlab_eval_string(Command).

matlab_call(S,Out,Result) :-
	S=..[Func|Args],
	build_args(Args,L0,[]),
	process_arg_entry(L0,L),
	build_output(Out,Lf,[' = ',Func|L]),
	atomic_concat(Lf,Command),
	matlab_eval_string(Command,Result).

build_output(Out,['[ '|L],L0) :-
	is_list(Out), !,
	build_outputs(Out,L,[']'|L0]).
build_output(Out,Lf,L0) :-
	build_arg(Out,Lf,L0).

build_outputs([],L,L).
build_outputs([Out|Outs],[Out,' '|L],L0) :-
	build_outputs(Outs,L,L0).

build_args([],L,L).	
build_args([Arg],Lf,L0) :- !,
	build_arg(Arg,Lf,[')'|L0]).
build_args([Arg|Args],L,L0) :-
	build_arg(Arg,L,[', '|L1]),
	build_args(Args,L1,L0).

build_arg(V,_,_) :- var(V), !,
	throw(error(instantiation_error)).
build_arg(Arg,[Arg|L],L) :- atomic(Arg), !.
build_arg(\S0,['\'',S0,'\''|L],L) :-
	atom(S0), !.
build_arg([S1|S2],['['|L],L0) :-
	is_list(S2), !,
	build_arglist([S1|S2],L,L0).
build_arg([S1|S2],L,L0) :- !,
	build_arg(S1,L,['.'|L1]),
	build_arg(S2,L1,L0).
build_arg(S1:S2,L,L0) :- !,
	build_arg(S1,L,[':'|L1]),
	build_arg(S2,L1,L0).
build_arg(F,[N,'{'|L],L0) :- %N({A}) = N{A}
	F=..[N,{A}], !,
	build_arg(A,L,['}'|L0]).
build_arg(F,[N,'('|L],L0) :-
	F=..[N|As],
	build_args(As,L,L0).

build_arglist([A],L,L0) :- !,
	build_arg(A,L,[' ]'|L0]).
build_arglist([A|As],L,L0) :-
	build_arg(A,L,[' ,'|L1]),
	build_arglist(As,L1,L0).

build_string([],['\''|L],L).
build_string([S0|S],[C|Lf],L0) :-
	char_code(C,S0),
	build_string(S,Lf,L0).


process_arg_entry([],[]) :- !.
process_arg_entry(L,['('|L]).
/** @} */

