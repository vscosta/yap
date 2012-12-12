/*
total_professors(32).

total_courses(64).

total_students(256).

*/

:- use_module(library(pfl)).

:- source.

:- style_check(all).

:- yap_flag(unknown,error).

:- yap_flag(write_strings,on).

:- ensure_loaded('parschema.pfl').

:- ensure_loaded(school32_data).

:- set_solver(hve).

