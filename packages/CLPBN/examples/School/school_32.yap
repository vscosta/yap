

/*
total_professors(32).

total_courses(64).

total_students(256).

*/

:- source.

:- style_check(all).

:- yap_flag(write_strings,on).

:- ensure_loaded(parschema).

:- yap_flag(unknown,error).
%:- clpbn_horus:set_solver(fove).
%:- clpbn_horus:set_solver(hve).
:- clpbn_horus:set_solver(bp).
%:- clpbn_horus:set_solver(ve).
%:- clpbn_horus:set_solver(cbp).

:- ensure_loaded(school32_data).

