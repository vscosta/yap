
% interface to prism from YAP

:- ensure_loaded(library(dialect/bprolog)).

:- set_prolog_flag(dollar_as_lower_case,on).

% :- set_prolog_flag(tabling_mode, local).
:- load_foreign_files([prism], [], bp4p_register_preds). /* load prism stuff */
:- style_check(-discontiguous). /* load prism stuff */

:- include('prism/core/message.pl').
:- include('prism/core/error.pl').
:- include('prism/core/random.pl').
:- include('prism/core/format.pl').

:- include('prism/up/dynamic.pl').
:- include('prism/up/main.pl').
:- include('prism/up/switch.pl').
:- include('prism/up/learn.pl').
:- include('prism/up/prob.pl').
:- include('prism/up/viterbi.pl').
:- include('prism/up/hindsight.pl').
:- include('prism/up/expl.pl').
:- include('prism/up/sample.pl').
:- include('prism/up/dist.pl').
:- include('prism/up/list.pl').
:- include('prism/up/hash.pl').
:- include('prism/up/flags.pl').
:- include('prism/up/util.pl').
:- include('prism/up/bigarray.pl').

:- include('prism/trans/trans.pl').
:- include('prism/trans/dump.pl').
:- include('prism/trans/verify.pl').
:- include('prism/trans/bpif.pl').

%PL_BAT   = up/batch.pl

%PL_MP    = mp/mp_main.pl        \
%           mp/mp_learn.pl

:- include('prism/bp/eval.pl').

:- initialization(init).

init :-
	( $pc_mp_mode -> true ; print_copyright ),
	random_set_seed,
	reset_prism_flags.



