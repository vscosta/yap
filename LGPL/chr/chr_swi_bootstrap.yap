

:- multifile user:file_search_path/2.

:- add_to_path('.').
	
:- use_module(library(swi)).

:- yap_flag(unknown,error).

:- include('chr_swi_bootstrap.pl').


