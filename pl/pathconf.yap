/**
 * @file pathconf.yap
 * 
 */

/**
 @defgroup pathconf Configuration of the Prolog file search path
 @ingroup absf


 @{ 

Prolog systems search perform a complex search on order to track
   down files.  
*/

:- module(user).

/**
@pred user:library_directory(?Directory:atom) is nondet, dynamic

Dynamic, multi-file predicate that succeeds when _Directory_ is a
current library directory name. Asserted in the user module.

Library directories are the places where files specified in the form
`library( _File_ )` are searched by the predicates consult/1,
reconsult/1, use_module/1, ensure_loaded/1, and load_files/2.

This directory is initialized by a rule that calls  the system predicate
system_library/1.
*/
:- multifile user:library_directory/1.
:- discontiguous user:library_directory/1.
:- dynamic user:library_directory/1.
%%  Specifies the set of directories where
% one can find Prolog libraries.
%
user:library_directory(Home) :-
    current_prolog_flag(library_directory, Home),
    Home \= ''.
% 1. honor YAPSHAREDIR
user:library_directory( Dir ) :-
        getenv( 'YAPSHAREDIR', Dir).
%% 2. honor user-library
user:library_directory( '~/share/Yap' ).
%% 3. honor current directory
user:library_directory( '.' ).
%% 4. honor default location.
user:library_directory( Dir ) :-
	system_library( Dir ).

/**
  @pred commons_directory(? _Directory_:atom) is nondet, dynamic

  State the location of the Commons Prolog Initiative.

  This directory is initialized as a rule that calls the system predicate
  library_directories/2.
  */
:- dynamic user:commons_directory/1.
:- discontiguous user:commons_directory/1.
:- multifile user:commons_directory/1.


user:commons_directory( Path ):-
    system_commons( Path ).

/**
  @pred foreign_directory(? _Directory_:atom) is nondet, dynamic

  State the location of the Foreign Prolog Initiative.

  This directory is initialized as a rule that calls the system predicate
  library_directories/2.
*/

:- multifile foreign_directory/1.
:- discontiguous foreign_directory/1.
:- dynamic foreign_directory/1.

%foreign_directory( Path ):-
foreign_directory(Home) :-
    current_prolog_flag(prolog_foreign_directory, Home),
    Home \= ''.
foreign_directory(C) :-
    current_prolog_flag(windows, true),
    user:file_search_path(path, C).
foreign_directory( '.').
foreign_directory(yap('lib/Yap')).
%foreign_directory( Path ):-
%    system_foreign( Path ).

/**
  @pred prolog_file_type(?Suffix:atom, ?Handler:atom) is nondet, dynamic

  This multifile/dynamic predicate relates a file extension _Suffix_
  to a language or file type _Handler_. By
  default, it supports the extensions yap, pl, and prolog for prolog files and
  uses one of dll, so, or dylib for shared objects. Initial definition is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prolog
  user:prolog_file_type(yap, prolog).
  user:prolog_file_type(pl, prolog).
  user:prolog_file_type(prolog, prolog).
  user:prolog_file_type(qly, prolog).
  user:prolog_file_type(qly, qly).
  user:prolog_file_type(A, prolog) :-
    current_prolog_flag(associate, A),
    A \== prolog,
    A \==pl,
    A \== yap.
  user:prolog_file_type(A, executable) :-
    current_prolog_flag(shared_object_extension, A).
  user:prolog_file_type(pyd, executable).
~~~~~~~~~~~~~~~~~~~~~
*/

:- multifile user:prolog_file_type/2.
:- discontiguous user:prolog_file_type/2.
:- dynamic user:prolog_file_type/2.

user:prolog_file_type(yap, prolog).
user:prolog_file_type(pl, prolog).
user:prolog_file_type(prolog, prolog).
user:prolog_file_type(A, prolog) :-
	current_prolog_flag(associate, A),
	A \== prolog,
	A \== pl,
	A \== yap.
user:prolog_file_type(qly, qly).
user:prolog_file_type(A, executable) :-
	current_prolog_flag(shared_object_extension, A).
  	user:prolog_file_type(pyd, executable).

/**
  @pred user:file_search_path(+Name:atom, -Directory:atom) is nondet

  Allows writing file names as compound terms. The  _Name_ and
  _DIRECTORY_ must be atoms. The predicate may generate multiple
  solutions. The predicate is originally defined as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pl
user:file_search_path(library, Dir) :-
  user:library_directory(Dir).
user:file_search_path(commons, Dir) :-
  commons_directory(Dir).
user:file_search_path(swi, Home) :-
  current_prolog_flag(home, Home).
user:file_search_path(yap, Home) :-
        current_prolog_flag(home, Home).
user:file_search_path(system, Dir) :-
  prolog_flag(host_type, Dir).
user:file_search_path(foreign, Dir) :-
  foreign_directory(Dir).
user:file_search_path(executable, Dir) :-
  foreign_directory(Dir).
user:file_search_path(executable, Dir) :-
  foreign_directory(Dir).
user:file_search_path(path, C) :-
    (   getenv('PATH', A),
        (   current_prolog_flag(windows, true)
          ->  atomic_list_concat(B, ;, A)
        ;   atomic_list_concat(B, :, A)
        ),
        lists:member(C, B)
    ).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Thus, `compile(library(A))` will search for a file using
  user:library_directory/1 to obtain the prefix,
  whereas 'compile(system(A))` would look at the `host_type` flag.

*/
:- multifile user:file_search_path/2.

:- dynamic user:file_search_path/2.
:- discontiguous user:file_search_path/2.

user:file_search_path(library, Dir) :-
	user:library_directory(Dir).
user:file_search_path(commons, Dir) :-
	user:commons_directory(Dir).
user:file_search_path(swi, Home) :-
	current_prolog_flag(home, Home).
user:file_search_path(yap, Home) :-
    current_prolog_flag(home, Home).
user:file_search_path(system, Dir) :-
	prolog_flag(host_type, Dir).
user:file_search_path(foreign, Dir) :-
  foreign_directory(Dir).
user:file_search_path(executable, Dir) :-
  foreign_directory(Dir).
user:file_search_path(path, C) :-
    (   getenv('PATH', A),
	(   current_prolog_flag(windows, true)
	->  atomic_list_concat(B, ;, A)
	;   atomic_list_concat(B, :, A)
	),
	lists:member(C, B)
    ).

%% @}
