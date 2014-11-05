/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
*************************************************************************/

/** @defgroup YAPAbsoluteFileName File Name Resolution

   @ingroup YAPBuiltins

  Support for file name resolution through absolute_file_name/3 and
  friends. These utility built-ins describe a list of directories that
  are used by load_files/2 to search. They include pre-compiled paths
  plus user-defined directories, directories based on environment
  variables and registry information to search for files.

  @{

 **/

:- system_module( absolute_file_name, [absolute_file_name/2,
        absolute_file_name/3,
        add_to_path/1,
        add_to_path/2,
        path/1,
        remove_from_path/1], ['$full_filename'/3,
        '$system_library_directories'/2]).

:- use_system_module( '$_boot', ['$system_catch'/4]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_lists', [member/2]).

/**
  @pred absolute_file_name(+File:atom, +Options:list, +Path:atom) is nondet
  @pred absolute_file_name(-File:atom, +Path:atom, +Options:list) is nondet

   _Options_ is a list of options to guide the conversion:

  -  extensions(+ _ListOfExtensions_)

     List of file-extensions to try.  Default is `''`.  For each
     extension, absolute_file_name/3 will first add the extension and then
     verify the conditions imposed by the other options.  If the condition
     fails, the next extension of the list is tried.  Extensions may be
     specified both with dot, as `.ext`, or without, as plain `ext`.

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

     If `error` (default), throw and `existence_error` exception
     if the file cannot be found.  If `fail`, stay silent.

  -  solutions(`first`/`all`)

     If `first` (default), the search cannot backtrack. leaves no choice-point.
     Otherwise a choice-point will be left and backtracking may yield
     more solutions.

  -  expand(`true`/`false`)

     If `true` (default is `false`) and  _Spec_  is atomic,
     call expand_file_name/2 followed by member/2 on  _Spec_  before
     proceeding.  This is originally a SWI-Prolog extension.

Compatibility considerations to common argument-order in ISO as well
as SICStus absolute_file_name/3 forced us to be flexible here.
If the last argument is a list and the second not, the arguments are
swapped, making the call
~~~~~~~~~~~prolog
  absolute_file_name(+ _Spec_ , - _Path_ ,+ _Options_ )
~~~~~~~~~~~
  valid as well.
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
    '$absolute_file_name'(F0,[access(read),file_type(prolog),file_errors(fail),solutions(first),expand(true)],F,G).

'$absolute_file_name'(File, _Opts, _TrueFileName, G) :- var(File), !,
	'$do_error'(instantiation_error, G).
'$absolute_file_name'(File,Opts,TrueFileName, G) :-
	'$process_fn_opts'(Opts,Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,G),
	/* our own local findall */
	nb:nb_queue(Ref),
	(
	    '$find_in_path'(File,opts(Extensions,RelTo,Type,Access,FErrors,Expand,Debug),TrueFileName,G),
	    nb:nb_queue_enqueue(Ref, TrueFileName),
	    fail
	;
	    nb:nb_queue_close(Ref, FileNames, [])
	 ),
	'$absolute_file_names'(Solutions, FileNames, FErrors, TrueFileName, File, G).

'$absolute_file_names'(_Solutions, [], error, _, File, G) :- !,
	'$do_error'(existence_error(file,File),G).
'$absolute_file_names'(Solutions, FileNames, _, TrueFileName, _, _) :-
        lists:member(TrueFileName, FileNames),
	(Solutions == first -> ! ; true).


'$process_fn_opts'(V,_,_,_,_,_,_,_,_,G) :- var(V), !,
	'$do_error'(instantiation_error, G).
'$process_fn_opts'([],[],_,txt,none,error,first,false,false,_) :- !.
'$process_fn_opts'([Opt|Opts],Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,G) :- !,
	'$process_fn_opt'(Opt,Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions0,RelTo0,Type0,Access0,FErrors0,Solutions0,Expand0,Debug0,G),
	'$process_fn_opts'(Opts,Extensions0,RelTo0,Type0,Access0,FErrors0,Solutions0,Expand0,Debug0,G).
'$process_fn_opts'(Opts,_Extensions,_RelTo,_Type,_Access,_FErrors,_Solutions,_Expand,_Debug,G) :- !,
	'$do_error'(type_error(list,Opts),G).

'$process_fn_opt'(Opt,Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,G) :- var(Opt), !,
	'$do_error'(instantiation_error, G).
'$process_fn_opt'(extensions(Extensions),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,_,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,G) :- !,
	'$check_fn_extensions'(Extensions,G).
'$process_fn_opt'(relative_to(RelTo),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,_,Type,Access,FErrors,Solutions,Expand,Debug,G) :- !,
	'$check_atom'(RelTo,G).
'$process_fn_opt'(access(Access),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,Type,_,FErrors,Solutions,Expand,Debug,G) :- !,
	'$check_atom'(Access,G).
'$process_fn_opt'(file_type(Type),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,_,Access,FErrors,Solutions,Expand,Debug,G) :- !,
	'$check_fn_type'(Type,G).
'$process_fn_opt'(file_errors(FErrors),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,Type,Access,_,Solutions,Expand,Debug,G) :- !,
	'$check_fn_errors'(FErrors,G).
'$process_fn_opt'(solutions(Solutions),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,Type,Access,FErrors,_,Expand,Debug,G) :- !,
	'$check_fn_solutions'(Solutions,G).
'$process_fn_opt'(expand(Expand),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,Type,Access,FErrors,Solutions,_,Debug,G) :- !,
	'$check_true_false'(Expand,G).
'$process_fn_opt'(verbose_file_search(Debug),Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,_,G) :- !,
	'$check_true_false'(Debug,G).
'$process_fn_opt'(Opt,Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,Extensions,RelTo,Type,Access,FErrors,Solutions,Expand,Debug,G) :- !,
	'$do_error'(domain_error(file_name_option,Opt),G).

'$check_fn_extensions'(V,G) :- var(V), !,
	'$do_error'(instantiation_error, G).
'$check_fn_extensions'([],_) :- !.
'$check_fn_extensions'([A|L],G) :- !,
	'$check_atom'(A,G),
	'$check_fn_extensions'(L,G).
'$check_fn_extensions'(T,G) :- !,
	'$do_error'(type_error(list,T),G).

'$check_atom'(V,G) :- var(V), !,
	'$do_error'(instantiation_error, G).
'$check_atom'(A,_G) :- atom(A), !.
'$check_atom'(T,G) :- !,
	'$do_error'(type_error(atom,T),G).

'$check_fn_type'(V,G) :- var(V), !,
	'$do_error'(instantiation_error, G).
'$check_fn_type'(txt,_) :- !.
'$check_fn_type'(prolog,_) :- !.
'$check_fn_type'(source,_) :- !.
'$check_fn_type'(executable,_) :- !.
'$check_fn_type'(qly,_) :- !.
'$check_fn_type'(directory,_) :- !.
'$check_fn_type'(T,G) :- atom(T), !,
	'$do_error'(domain_error(file_type,T),G).
'$check_fn_type'(T,G) :- !,
	'$do_error'(type_error(atom,T),G).

'$check_fn_errors'(V,G) :- var(V), !,
	'$do_error'(instantiation_error, G).
'$check_fn_errors'(fail,_) :- !.
'$check_fn_errors'(error,_) :- !.
'$check_fn_errors'(T,G) :- atom(T), !,
	'$do_error'(domain_error(file_errors,T),G).
'$check_fn_errors'(T,G) :- !,
	'$do_error'(type_error(atom,T),G).

'$check_fn_solutions'(V,G) :- var(V), !,
	'$do_error'(instantiation_error, G).
'$check_fn_solutions'(first,_) :- !.
'$check_fn_solutions'(all,_) :- !.
'$check_fn_solutions'(T,G) :- atom(T), !,
	'$do_error'(domain_error(solutions,T),G).
'$check_fn_solutions'(T,G) :- !,
	'$do_error'(type_error(atom,T),G).

'$check_true_false'(V,G) :- var(V), !,
	'$do_error'(instantiation_error, G).
'$check_true_false'(true,_) :- !.
'$check_true_false'(false,_) :- !.
'$check_true_false'(T,G) :- atom(T), !,
	'$do_error'(domain_error(boolean,T),G).
'$check_true_false'(T,G) :- !,
	'$do_error'(type_error(atom,T),G).

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
	'$dir_separator'(D),
	atom_codes(A,[D]),
	'$extend_path_directory'(Name, A, File, Opts, NewFile, Call).
'$find_in_path'(File0,Opts,NewFile,_) :-
	'$cat_file_name'(File0,File), !,
	'$add_path'(File,PFile),
	'$get_abs_file'(PFile,Opts,AbsFile),
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

'$get_abs_file'(File,opts(_,RelTo,_,_,_,Expand,_),AbsFile) :-
	'$swi_current_prolog_flag'(file_name_variables, OldF),
	'$swi_set_prolog_flag'(file_name_variables, Expand),
	(
	 '$absolute_file_name'(File,ExpFile)
	->
	'$swi_set_prolog_flag'(file_name_variables, OldF)
	;
	'$swi_set_prolog_flag'(file_name_variables, OldF),
	 fail
	),
	(
	 nonvar(RelTo)
	->
	 ( is_absolute_file_name(ExpFile) ->
	   AbsFile = ExpFile
	  ;
	   '$dir_separator'(D),
	   atom_codes(DA,[D]),
	   atom_concat([RelTo, DA, ExpFile], AbsFile)
	  )
	;
	  AbsFile = ExpFile
	).


'$search_in_path'(File,opts(Extensions,_,Type,Access,_,_,_),F) :-
	'$add_extensions'(Extensions, File, F0),
	'$check_file'(F0, Type, Access, F).
'$search_in_path'(File,opts(_,_,Type,Access,_,_,_),F) :-
	'$add_type_extensions'(Type, File, F0),
	'$check_file'(F0, Type, Access, F).

'$check_file'(F, _Type, none, F) :- !.
'$check_file'(F0, Type, Access, F0) :-
	access_file(F0, Access),
	(Type == directory ->
	 exists_directory(F0)
	;
	\+ exists_directory(F0) % if it has a type cannot be a directory.
	).

'$add_extensions'([Ext|_],File,F) :-
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
	atom_concat([File,'.',Ext],F).
'$add_type_extensions'(_,File,File).

'$add_path'(File,File) :-
	'$dir_separator'(D),
	atom_codes(DA,[D]),
	sub_atom(File, 0, 1, _, DA), !.
'$add_path'(File,File).
'$add_path'(File,PFile) :-
	recorded('$path',Path,_),
	atom_concat([Path,File],PFile).

'$system_library_directories'(library, Dir) :-
	getenv('YAPSHAREDIR', Dirs),
	'$split_by_sep'(0, 0, Dirs, Dir).
'$system_library_directories'(foreign, Dir) :-
	getenv('YAPLIBDIR', Dirs),
	'$split_by_sep'(0, 0, Dirs, Dir).
% compatibility with old versions
% search the current directory first.
'$system_library_directories'(commons, Dir) :-
	getenv('YAPCOMMONSDIR', Dirs),
	'$split_by_sep'(0, 0, Dirs, Dir).
% windows has stuff installed in the registry
'$system_library_directories'(Library, Dir) :-
	 '$swi_current_prolog_flag'(windows, true),
	( (
	  '$swi_current_prolog_flag'(address_bits, 64) ->
		( HKEY='HKEY_LOCAL_MACHINE/Software/YAP/Prolog64';
		  HKEY='HKEY_CURRENT_USER/Software/YAP/Prolog64' )
	      ;
		( HKEY='HKEY_LOCAL_MACHINE/Software/YAP/Prolog';
		  HKEY='HKEY_CURRENT_USER/Software/YAP/Prolog' )
	      ),  % do not use once/1
	% sanity check: are we running the binary mentioned in the registry?
        '$system_catch'(win_registry_get_value(HKEY,'bin', Bin), prolog,_,fail) ) -> true,
	'$swi_current_prolog_flag'(executable, Bin1),
	same_file(Bin, Bin1),
	'$system_catch'(win_registry_get_value(HKEY, Library, Dir), prolog,_,fail).
% not installed on registry
'$system_library_directories'(Library, Dir) :-
	'$yap_paths'(_DLLs, ODir1, _OBinDir ),
%	'$absolute_file_name'( OBinDir, BinDir ),
%	'$swi_current_prolog_flag'(executable, Bin1),
%	prolog_to_os_filename( Bin2, Bin1 ),
%	file_directory_name( Bin2, BinDir1 ),
%	same_file( BinDir, BinDir1 ),
	( Library == library ->
	  atom_concat( ODir1, '/Yap' , ODir )
	;
	  atom_concat( ODir1, '/PrologCommons' , ODir )
	),
	'$absolute_file_name'( ODir, Dir ),
	exists_directory( Dir ), !.
% desperation: let's check the executable directory
'$system_library_directories'(Library, Dir) :-
	'$swi_current_prolog_flag'(executable, Bin1),
	prolog_to_os_filename( Bin2, Bin1 ),
	file_directory_name( Bin2, Dir1 ),
	( Library == library ->
	  atom_concat( Dir1, '../share/Yap' , Dir )
	;
	  atom_concat( Dir1, '../share/PrologCommons' , Dir )
	),
	exists_directory( Dir ), !.



'$split_by_sep'(Start, Next, Dirs, Dir) :-
    '$swi_current_prolog_flag'(windows, true),
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
        ( atom(IDirs) ->
	  '$split_by_sep'(0, 0, IDirs, Dir)
	;
	   Dir = IDirs
	),
	'$extend_pathd'(Dir, D, File, Opts, NewFile, Call).

'$extend_pathd'(Dir, A, File, Opts, NewFile, Goal) :-
	atom(Dir), !,
	'$add_file_to_dir'(Dir,A,File,NFile),
	'$find_in_path'(NFile, Opts, NewFile, Goal), !.
'$extend_pathd'(Name, A, File, Opts, OFile, Goal) :-
	nonvar(Name),
	Name =.. [N,P0],
	'$add_file_to_dir'(P0,A,File,NFile),
	NewName =.. [N,NFile],
	'$find_in_path'(NewName, Opts, OFile, Goal).

'$add_file_to_dir'(P0,A,Atoms,NFile) :-
	atom_concat([P0,A,Atoms],NFile).

/** @pred prolog_file_name( +File, -PrologFileName)

Unify _PrologFileName_ with the Prolog file associated to _File_.

*/
prolog_file_name(File, PrologFileName) :-
	var(File), !,
	'$do_error'(instantiation_error, prolog_file_name(File, PrologFileName)).
prolog_file_name(user, Out) :- !, Out = user.
prolog_file_name(File, PrologFileName) :-
	atom(File), !,
	operating_system_support:true_file_name(File, PrologFileName).
prolog_file_name(File, PrologFileName) :-
	'$do_error'(type_error(atom,File), prolog_file_name(File, PrologFileName)).

/**
  @pred path(-Directories:list) is det,deprecated

 YAP specific procedure that returns a list of user-defined directories
 in the library search-path.We suggest using user:file_search_path/2 for
 compatibility with other Prologs.
*/
path(Path) :- findall(X,'$in_path'(X),Path).

'$in_path'(X) :- recorded('$path',Path,_),
		atom_codes(Path,S),
		( S = ""  -> X = '.' ;
		  atom_codes(X,S) ).

/**
  @pred add_to_path(+Directory:atom) is det,deprecated

  YAP-specific predicate to include directory in library search path.
  We suggest using user:file_search_path/2 for
  compatibility with other Prologs.
*/
add_to_path(New) :- add_to_path(New,last).

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

'$add_to_path'(New,_) :- recorded('$path',New,R), erase(R), fail.
'$add_to_path'(New,last) :- !, recordz('$path',New,_).
'$add_to_path'(New,first) :- recorda('$path',New,_).

/**  @pred   remove_from_path(+Directory:atom) is det,deprecated

*/
remove_from_path(New) :- '$check_path'(New,Path),
			recorded('$path',Path,R), erase(R).

'$check_path'(At,SAt) :- atom(At), !, atom_codes(At,S), '$check_path'(S,SAt).
'$check_path'([],[]).
'$check_path'([Ch],[Ch]) :- '$dir_separator'(Ch), !.
'$check_path'([Ch],[Ch,A]) :- !, integer(Ch), '$dir_separator'(A).
'$check_path'([N|S],[N|SN]) :- integer(N), '$check_path'(S,SN).

/**
  @pred user:library_directory(?Directory:atom) is nondet, dynamic

  Dynamic, multi-file predicate that succeeds when _Directory_ is a
  current library directory name. Asserted in the user module.

  Library directories are the places where files specified in the form
  `library( _File_ )` are searched by the predicates consult/1,
  reconsult/1, use_module/1, ensure_loaded/1, and load_files/2.

  This directory is initialized through the system predicate
  library_directories/2.
*/

:- multifile user:library_directory/1.

:- dynamic user:library_directory/1.

user:library_directory( Path ):-
    library_directories( Path, _ ).

/**
  @pred user:commons_directory(? _Directory_:atom) is nondet, dynamic

  State the location of the Commons Prolog Initiative.

  This directory is initialized through the system predicate
  library_directories/2.
*/

:- multifile user:commons_directory/1.

:- dynamic user:commons_directory/1.

user:commons_directory( Path ):-
    library_directories( _, Path ).

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
	A \==pl,
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
file_search_path(system, Dir) :-
  prolog_flag(host_type, Dir).
file_search_path(foreign, yap('lib/Yap')).
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

@}

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

