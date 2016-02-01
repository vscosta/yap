/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
*************************************************************************/

/** 



 @file absf.yap
 @author L.Damas, V.S.Costa

*/

:- system_module( absf, [absolute_file_name/2,
        absolute_file_name/3,
        add_to_path/1,
        add_to_path/2,
        path/1,
        remove_from_path/1], ['$full_filename'/3,
        '$system_library_directories'/2]).

/**

@defgroup AbsoluteFileName File Name Resolution
    @ingroup builtins

 Support for file name resolution through absolute_file_name/3 and
  friends. These utility built-ins describe a list of directories that
  are used by load_files/2 to search. They include pre-compiled paths
  plus user-defined directories, directories based on environment
  variables and registry information to search for files.

@{

*/

:- use_system_module( '$_boot', ['$system_catch'/4]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_lists', [member/2]).

/**

@pred absolute_file_name( -File:atom, +Path:atom, +Options:list) is nondet

_Options_ is a list of options to guide the conversion:

  -  extensions(+ _ListOfExtensions_)

     List of file-name suffixes to add to try adding to the file. The
     Default is the empty suffix, `''`.  For each extension,
     absolute_file_name/3 will first add the extension and then verify
     the conditions imposed by the other options.  If the condition
     fails, the next extension of the list is tried.  Extensions may
     be specified both with dot, as `.ext`, or without, as plain
     `ext`.

  -  relative_to(+ _FileOrDir_ )

     Resolve the path relative to the given directory or directory the
     holding the given file.  Without this option, paths are resolved
     relative to the working directory (see working_directory/2) or,
     if  _Spec_  is atomic and absolute_file_name/3 is executed
     in a directive, it uses the current source-file as reference.

  -  access(+ _Mode_ )

     Imposes the condition access_file( _File_ ,  _Mode_ ).   _Mode_  is one of `read`, `write`, `append`, `exist` or
     `none` (default).

     See also access_file/2.

  -  file_type(+ _Type_ )

     Defines suffixes matching one of several pre-specified type of files. Default mapping is as follows:

       1.  `txt` implies `[ '' ]`,

       2.  `prolog` implies `['.yap', '.pl', '.prolog', '']`,

       3.  `executable`  implies `['.so', ',dylib', '.dll']` depending on the Operating system,

       4.  `qly` implies `['.qly', '']`,

       5.  `directory` implies `['']`,

       6.  The file-type `source` is an alias for `prolog` designed to support compatibility with SICStus Prolog. See also prolog_file_type/2.

     Notice that this predicate only
     returns non-directories, unless the option `file_type(directory)` is
     specified, or unless `access(none)`.

  -  file_errors(`fail`/`error`)

     If `error` (default), throw  `existence_error` exception
     if the file cannot be found.  If `fail`, stay silent.

  -  solutions(`first`/`all`)

     If `first` (default), commit to the first solution.  Otherwise
     absolute_file_name will enumerate all solutions via backtracking.

  -  expand(`true`/`false`)

     If `true` (default is `false`) and _Spec_ is atomic, call
     expand_file_name/2 followed by member/2 on _Spec_ before
     proceeding.  This is originally a SWI-Prolog extension, but
     whereas SWI-Prolog implements its own conventions, YAP uses the
     shell's `glob` primitive.

     Notice that in `glob` mode YAP will fail if it cannot find a matching file, as `glob`
     implicitely tests for existence when checking for patterns.

  -  glob(`Pattern`)

     If  _Pattern_ is atomic, add the pattern as a suffix to the current expansion, and call
     expand_file_name/2 followed by member/2 on the result. This is originally  a SICStus Prolog exception.

     Both `glob` and `expand` rely on the same underlying
     mechanism. YAP gives preference to `glob`.

  -  verbose_file_search(`true`/`false`)

     If `true` (default is `false`) output messages during
     search. This is often helpful when debugging. Corresponds to the
     SWI-Prolog flag `verbose_file_search` (also available in YAP).


Compatibility considerations to common argument-order in ISO as well
as SICStus absolute_file_name/3 forced us to be flexible here.
If the last argument is a list and the second not, the arguments are
swapped, thus the call
~~~~~~~~~~~~
?- absolute_file_name( 'pl/absf.yap', [], Path)
~~~~~~~~~~~~
  is valid as well.
*/

absolute_file_name(File,TrueFileName,Opts) :-
    ( var(TrueFileName) ->
	  true ;
      atom(TrueFileName), TrueFileName \= []
    ), !,
    absolute_file_name(File,Opts,TrueFileName).
absolute_file_name(File,Opts,TrueFileName) :-
    '$absolute_file_name'(File,Opts,TrueFileName,absolute_file_name(File,Opts,TrueFileName)).

/**
  @pred absolute_file_name(+Name:atom,+Path:atom) is nondet

  Converts the given file specification into an absolute path, using default options. See absolute_file_name/3 for details on the options.
*/
absolute_file_name(V,Out) :- var(V), !,	% absolute_file_name needs commenting.
	'$do_error'(instantiation_error, absolute_file_name(V, Out)).
absolute_file_name(user,user) :- !.
absolute_file_name(File0,File) :-
	'$absolute_file_name'(File0,[access(none),file_type(txt),file_errors(fail),solutions(first)],File,absolute_file_name(File0,File)).

'$full_filename'(F0, F, G) :-
	'$absolute_file_name'(F0,[access(read),
                              file_type(prolog),
                              file_errors(fail),
                              solutions(first),
                              expand(true)],F,G).

'$absolute_file_name'(File, _Opts, _TrueFileName, G) :- var(File), !,
	'$do_error'(instantiation_error, G).
'$absolute_file_name'(File,LOpts,TrueFileName, G) :-
	current_prolog_flag(open_expands_filename, OldF),
	current_prolog_flag( fileerrors, PreviousFileErrors ),
	current_prolog_flag( verbose_file_search, PreviousVerbose ),
	abs_file_parameters(LOpts,Opts),
	get_abs_file_parameter( verbose_file_search, Opts, Verbose ),
	get_abs_file_parameter( expand, Opts, Expand ),
	set_prolog_flag( verbose_file_search,  Verbose ),
	get_abs_file_parameter( file_errors, Opts, FErrors ),
	(  FErrors == fail ->
	  set_prolog_flag( fileerrors, false )
	;
	  set_prolog_flag( fileerrors, true )
	),
	set_prolog_flag(file_name_variables, Expand),
	'$absf_trace'('search for ~w with options ~w', [File, LOpts] ),
	'$find_in_path'(File, Opts,TrueFileName,G),
	(
	  get_abs_file_parameter( solutions, Opts, first )
	 ->
	 '$absf_trace'('found solution ~a', [TrueFileName] ),
%	 	stop_lowxb(  _level_trace,
	 set_prolog_flag( fileerrors, PreviousFileErrors ),
	 set_prolog_flag( open_expands_filename, OldF),
	 set_prolog_flag( verbose_file_search, PreviousVerbose ),
	 '$absf_trace'('first solution only', [] ),
	 !
	 ;
	 (
	  '$absf_trace'('found solution ~a', [TrueFileName] ),
%	 	stop_low_level_trace,
	  set_prolog_flag( fileerrors, PreviousFileErrors ),
	  set_prolog_flag( file_name_variables, OldF),
	  set_prolog_flag( verbose_file_search, PreviousVerbose )
	;
	  set_prolog_flag( verbose_file_search, Verbose ),
	  get_abs_file_parameter( file_errors, Opts, FErrors ),
	  set_prolog_flag(file_name_variables, Expand),
	  fail
	 )
	;
				% no solution
%	 	stop_low_level_trace,
	 set_prolog_flag( fileerrors, PreviousFileErrors ),
	 set_prolog_flag( verbose_file_search, PreviousVerbose ),
	 set_prolog_flag(file_name_variables, OldF),
	 get_abs_file_parameter( file_errors, Opts, error ),
	 '$do_error'(existence_error(file,File),G)
	).

% This sequence must be followed:
% user and user_input are special;
% library(F) must check library_directories
% T(F) must check file_search_path
% all must try search in path
'$find_in_path'(user,_,user_input, _) :- !.
'$find_in_path'(user_input,_,user_input, _) :- !.
'$find_in_path'(S, Opts, NewFile, Call) :-
	S =.. [Name,File0],
	'$cat_file_name'(File0,File), !,
	'$absf_trace'('~w(~w) to ~w', [Name, File0, File] ),
	'$dir_separator'(D),
	atom_codes(A,[D]),
	'$extend_path_directory'(Name, A, File, Opts, NewFile, Call).
'$find_in_path'(File0,Opts,NewFile,_) :-
	'$cat_file_name'(File0,File), !,
	'$add_path'(File, Opts, PFile),
	'$get_abs_file'(PFile,Opts,AbsFile),
    '$absf_trace'('~w to ~w', [PFile, NewFile] ),
	'$search_in_path'(AbsFile,Opts,NewFile).
'$find_in_path'(File,_,_,Call) :-
	'$do_error'(domain_error(source_sink,File),Call).

% allow paths in File Name
'$cat_file_name'(File0,File) :-
	atom(File0), !,
	File = File0.
'$cat_file_name'(Atoms, File) :-
	'$to_list_of_atoms'(Atoms, List, []),
	atom_concat(List, File).

'$to_list_of_atoms'(V, _, _) :- var(V), !, fail.
'$to_list_of_atoms'(Atom, [Atom|L], L) :- atom(Atom), !.
'$to_list_of_atoms'(Atoms, L1, LF) :-
	Atoms =.. [A,As,Bs],
	atom_codes(A,[D]),
	'$dir_separator'(D),
	'$to_list_of_atoms'(As, L1, [A|L2]),
	'$to_list_of_atoms'(Bs, L2, LF).

'$get_abs_file'(File,Opts, ExpFile) :-
	'$control_for_expansion'(Opts, Expand),
	get_abs_file_parameter( relative_to, Opts, RelTo ),
	prolog_expanded_file_system_path( File, Expand, RelTo, ExpFile ),
	'$absf_trace'('Traditional expansion: ~w', [ExpFile] ).


'$control_for_expansion'(Opts, true) :-
	get_abs_file_parameter( expand, Opts, true ),
	!.
'$control_for_expansion'(_Opts, Flag) :-
	current_prolog_flag( open_expands_filename, Flag ).


'$search_in_path'(File,Opts,F) :-
	get_abs_file_parameter( extensions, Opts, Extensions ),
	'$absf_trace'('check extensions ~w?', [Extensions] ),
	'$add_extensions'(Extensions, File, F0),
	'$glob'( F0, Opts, FG),
	get_abs_file_parameter( file_type, Opts, Type ),
	get_abs_file_parameter( access, Opts, Access ),
	'$check_file'(FG,Type, Access, F),
	'$absf_trace'(' ~a  ok!', [Access]).
'$search_in_path'(File,Opts,F) :-
	get_abs_file_parameter( file_type, Opts, Type ),
	'$absf_trace'('check type ~w', [Type] ),
	'$add_type_extensions'(Type,File, F0),
	get_abs_file_parameter( access, Opts, Access ),
	'$glob'( F0, Opts, FG),
	'$check_file'(FG, Type, Access, F),
	'$absf_trace'('   ~w   ok!', [Access]).

'$glob'( File1, Opts, ExpFile) :-
	'$control_for_expansion'(Opts, Expand),
	get_abs_file_parameter( glob, Opts, Glob ),
	(Glob \== ''
	->
	'$dir_separator'(D),
	atom_codes(DA,[D]),
	 atom_concat( [File1, DA, Glob], File2 ),
	 expand_file_name(File2, ExpFiles),
     % glob is not very much into failing	
     %[File2] \== ExpFiles,
     '$enumerate_glob'(File2, ExpFiles, ExpFile)
	;
	 Expand == true
	->
	 expand_file_name(File1, ExpFiles),
     '$enumerate_glob'(File1, ExpFiles, ExpFile)
	 ;
	 File1 = ExpFile
	),
	'$absf_trace'(' With globbing (glob=~q;expand=~a): ~w', [Glob,Expand,ExpFile] ).


'$enumerate_glob'(_File1, [ExpFile], ExpFile) :-
    !.
'$enumerate_glob'(_File1, ExpFiles, ExpFile) :-
    lists:member(ExpFile, ExpFiles),
    file_base_name( ExpFile, Base ),
    Base \= '.',
    Base \='..'.


% always verify if a directory
'$check_file'(F, directory, _, F) :-
	!,
	exists_directory(F).
'$check_file'(F, _Type, none, F) :- !.
'$check_file'(F0, _Type, Access, F0) :-
	access_file(F0, Access),
	\+ exists_directory(F0). % if it has a type cannot be a directory..

'$add_extensions'([Ext|_], File,F) :-
	'$absf_trace'(' extension ~w', [Ext] ),
	'$mk_sure_true_ext'(Ext,NExt),
	atom_concat([File,NExt],F).
'$add_extensions'([_|Extensions],File,F) :-
	'$add_extensions'(Extensions,File,F).

'$mk_sure_true_ext'(Ext,NExt) :-
	atom_codes(Ext,[C|L]),
	C \= 0'.,
	!,
	atom_codes(NExt,[0'.,C|L]).
'$mk_sure_true_ext'(Ext,Ext).

'$add_type_extensions'(Type,File,F) :-
	( Type == source -> NType = prolog ; NType = Type ),
	user:prolog_file_type(Ext, NType),
	atom_concat([File,'.',Ext],F),
	'$absf_trace'('      extension ~w?', [F] ).
'$add_type_extensions'(_,File,File) :-
	'$absf_trace'('  wo  extension ~w?', [File] ).

'$add_path'(File, _, File) :-
	is_absolute_file_name(File), !.
'$add_path'(File, Opts, File) :-
    ( get_abs_file_parameter( relative_to, Opts, Dir ) -> 
      true
      ;
	   working_directory(Dir, Dir)
    ),
	'$dir_separator'( D ),
	atom_codes( DSep, [D] ),
	atomic_concat([Dir, DSep,File],PFile),
	'$absf_trace'(' try . or ~a: ~a', [Dir,PFile] ).
'$add_path'(File, PFile) :-
	recorded('$path',Path,_),
	atom_concat([Path,File],PFile),
	'$absf_trace'(' try ~a from path-data base: ~a', [Path, PFile] ).

'$system_library_directories'(library, Dir) :-
	user:library_directory( Dir ).
%	'$split_by_sep'(0, 0, Dirs, Dir).
'$system_library_directories'(foreign, Dir) :-
	foreign_directory( Dir ).
% compatibility with old versions
% search the current directory first.
'$system_library_directories'(commons, Dir) :-
	commons_directory( Dir ).

'$split_by_sep'(Start, Next, Dirs, Dir) :-
    current_prolog_flag(windows, true),
    '$split_by_sep'(Start, Next, Dirs, ';', Dir), !.
'$split_by_sep'(Start, Next, Dirs, Dir) :-
    '$split_by_sep'(Start, Next, Dirs, ':', Dir).

'$split_by_sep'(Start, Next, Dirs, Sep, Dir) :-
    sub_atom(Dirs, Next, 1, _, Let), !,
    '$continue_split_by_sep'(Let, Start, Next, Dirs, Sep, Dir).
'$split_by_sep'(Start, Next, Dirs, _Sep, Dir) :-
    Next > Start,
    Len is Next-Start,
    sub_atom(Dirs, Start, Len, _, Dir).


% closed a directory
'$continue_split_by_sep'(Sep, Start, Next, Dirs, Sep, Dir) :-
    Sz is Next-Start,
    Sz > 0,
    sub_atom(Dirs, Start, Sz, _, Dir).
% next dir
'$continue_split_by_sep'(Sep , _Start, Next, Dirs, Sep, Dir) :- !,
    N1 is Next+1,
    '$split_by_sep'(N1, N1, Dirs, Dir).
% same dir
'$continue_split_by_sep'(_Let, Start, Next, Dirs, Sep, Dir) :-
    N1 is Next+1,
    '$split_by_sep'(Start, N1, Dirs, Sep, Dir).


'$extend_path_directory'(_Name, _D, File, _Opts, File, _Call) :-
	is_absolute_file_name(File), !.
'$extend_path_directory'(Name, D, File, Opts, NewFile, Call) :-
	user:file_search_path(Name, IDirs),
	'$absf_trace'('file_search_path  ~a is ~w', [Name, IDirs] ),
	ground(IDirs),
	(
	 '$extend_path_directory'(IDirs, D, File, Opts, NewFile, Call)
	;
         atom(IDirs) ->
	 '$split_by_sep'(0, 0, IDirs, Dir)
	;
	   Dir = IDirs
	),
	'$extend_pathd'(Dir, D, File, Opts, NewFile, Call).

'$extend_pathd'(Dir, A, File, Opts, NewFile, Goal) :-
	atom(Dir), !,
	'$add_file_to_dir'(Dir,A,File,NFile),
	'$absf_trace'(' try  ~a', [NFile] ),
	'$find_in_path'(NFile, Opts, NewFile, Goal), !.
'$extend_pathd'(Name, A, File, Opts, OFile, Goal) :-
	nonvar(Name),
	Name =.. [N,P0],
	'$add_file_to_dir'(P0,A,File,NFile),
	NewName =.. [N,NFile],
	'$absf_trace'(' try  ~q', [NewName] ),
	'$find_in_path'(NewName, Opts, OFile, Goal).

'$add_file_to_dir'(P0,A,Atoms,NFile) :-
	atom_concat([P0,A,Atoms],NFile).

'$absf_trace'(Msg, Args ) :-
	current_prolog_flag( verbose_file_search, true ),
	!,
	print_message( informational, absolute_file_path( Msg, Args ) ).
'$absf_trace'(_Msg, _Args ).

/** @pred prolog_file_name( +File, -PrologFileaNme)

Unify _PrologFileName_ with the Prolog file associated to _File_.

*/
prolog_file_name(File, PrologFileName) :-
	var(File), !,
	'$do_error'(instantiation_error, prolog_file_name(File, PrologFileName)).
prolog_file_name(user, Out) :- !, Out = user.
prolog_file_name(File, PrologFileName) :-
	atom(File), !,
	system:true_file_name(File, PrologFileName).
prolog_file_name(File, PrologFileName) :-
	'$do_error'(type_error(atom,File), prolog_file_name(File, PrologFileName)).

/**
  @pred path(-Directories:list) is det,deprecated

 YAP specific procedure that returns a list of user-defined directories
 in the library search-path.We suggest using user:file_search_path/2 for
 compatibility with other Prologs.
*/
path(Path) :-
	findall(X,'$in_path'(X),Path).

'$in_path'(X) :-
	recorded('$path',Path,_),
	atom_codes(Path,S),
	( S = ""  -> X = '.' ;
	  atom_codes(X,S) ).

/**
  @pred add_to_path(+Directory:atom) is det,deprecated

  YAP-specific predicate to include directory in library search path.
  We suggest using user:file_search_path/2 for
  compatibility with other Prologs.
*/
add_to_path(New) :-
	add_to_path(New,last).

/**
  @pred add_to_path(+Directory:atom, +Position:atom) is det,deprecated

  YAP-specific predicate to include directory in front or back of
  library search path. We suggest using user:file_search_path/2 for
  compatibility with other Prologs and more extensive functionality.
*/
add_to_path(New,Pos) :-
	atom(New), !,
	'$check_path'(New,Str),
	atom_codes(Path,Str),
	'$add_to_path'(Path,Pos).

'$add_to_path'(New,_) :-
	recorded('$path',New,R),
	erase(R),
	fail.
'$add_to_path'(New,last) :-
	!,
	recordz('$path',New,_).
'$add_to_path'(New,first) :-
	recorda('$path',New,_).

/**  @pred   remove_from_path(+Directory:atom) is det,deprecated

@}

*/
remove_from_path(New) :- '$check_path'(New,Path),
			recorded('$path',Path,R), erase(R).

'$check_path'(At,SAt) :- atom(At), !, atom_codes(At,S), '$check_path'(S,SAt).
'$check_path'([],[]).
'$check_path'([Ch],[Ch]) :- '$dir_separator'(Ch), !.
'$check_path'([Ch],[Ch,A]) :- !, integer(Ch), '$dir_separator'(A).
'$check_path'([N|S],[N|SN]) :- integer(N), '$check_path'(S,SN).

/**
 @defgroup pathconf Configuration of the Prolog file search path
 @ingroup AbsoluteFileName

  Prolog systems search follow a complex search on order to track down files.

@{
**/

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

:- dynamic user:library_directory/1.
%%  Specifies the set of directories where
% one can find Prolog libraries.
%
% 1. honor YAPSHAREDIR
user:library_directory( Dir ) :-
        getenv( 'YAPSHAREDIR', Dir0),
        absolute_file_name( Dir0, [file_type(directory), expand(true),file_errors(fail)], Dir ).
%% 2. honor user-library
user:library_directory( Dir ) :-
        absolute_file_name( '~/share/Yap', [file_type(directory), expand(true),file_errors(fail)], Dir ).
%% 3. honor current directory
user:library_directory( Dir ) :-
        absolute_file_name( '.', [file_type(directory), expand(true),file_errors(fail)], Dir ).
%% 4. honor default location.
user:library_directory( Dir ) :-
	system_library( Dir ).

/**
  @pred user:commons_directory(? _Directory_:atom) is nondet, dynamic

  State the location of the Commons Prolog Initiative.

  This directory is initialized as a rule that calls the system predicate
  library_directories/2.
*/

:- multifile user:commons_directory/1.

:- dynamic user:commons_directory/1.


user:commons_directory( Path ):-
    system_commons( Path ).

/**
  @pred user:foreign_directory(? _Directory_:atom) is nondet, dynamic

  State the location of the Foreign Prolog Initiative.

  This directory is initialized as a rule that calls the system predicate
  library_directories/2.
*/

:- multifile user:foreign_directory/1.

:- dynamic user:foreign_directory/1.

user:foreign_directory( Path ):-
    system_foreign( Path ).

/**
  @pred user:prolog_file_type(?Suffix:atom, ?Handler:atom) is nondet, dynamic

  This multifile/dynamic predicate relates a file extension _Suffix_
  to a language or file type _Handler_. By
  default, it supports the extensions yap, pl, and prolog for prolog files and
  uses one of dll, so, or dylib for shared objects. Initial definition is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prolog
  prolog_file_type(yap, prolog).
  prolog_file_type(pl, prolog).
  prolog_file_type(prolog, prolog).
  prolog_file_type(qly, prolog).
  prolog_file_type(qly, qly).
  prolog_file_type(A, prolog) :-
    current_prolog_flag(associate, A),
    A \== prolog,
    A \==pl,
    A \== yap.
  prolog_file_type(A, executable) :-
    current_prolog_flag(shared_object_extension, A).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/

:- multifile user:prolog_file_type/2.

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


/**
  @pred user:file_search_path(+Name:atom, -Directory:atom) is nondet

  Allows writing file names as compound terms. The  _Name_ and
  _DIRECTORY_ must be atoms. The predicate may generate multiple
  solutions. The predicate is originally defined as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prolog
file_search_path(library, Dir) :-
  library_directory(Dir).
file_search_path(commons, Dir) :-
  commons_directory(Dir).
file_search_path(swi, Home) :-
  current_prolog_flag(home, Home).
file_search_path(yap, Home) :-
        current_prolog_flag(home, Home).
file_search_path,(system, Dir) :-
  prolog_flag(host_type, Dir).
file_search_path(foreign, Dir) :-
  foreign_directory(Dir).
file_search_path(path, C) :-
    (   getenv('PATH', A),
        (   current_prolog_flag(windows, true)
          ->  atomic_list_concat(B, ;, A)
        ;   atomic_list_concat(B, :, A)
        ),
        lists:member(C, B)
    ).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Thus, `compile(library(A))` will search for a file using
  library_directory/1 to obtain the prefix,
  whereas 'compile(system(A))` would look at the `host_type` flag.

*/

:- multifile user:file_search_path/2.

:- dynamic user:file_search_path/2.

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
    working_directory(Dir,Dir).
user:file_search_path(foreign, yap('lib/Yap')).
user:file_search_path(path, C) :-
    (   getenv('PATH', A),
	(   current_prolog_flag(windows, true)
	->  atomic_list_concat(B, ;, A)
	;   atomic_list_concat(B, :, A)
	),
	lists:member(C, B)
    ).


%% @}