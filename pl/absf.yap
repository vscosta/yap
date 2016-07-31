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
 * @defgroup AbsoluteFileName File Name Resolution
 * @ingroup builtins

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
    ),
    !,
    absolute_file_name(File,Opts,TrueFileName).
absolute_file_name(File,Opts,TrueFileName) :-
    '$absolute_file_name'(File,Opts,TrueFileName,absolute_file_name(File,Opts,TrueFileName)).

/**
  @pred absolute_file_name(+Name:atom,+Path:atom) is nondet

  Converts the given file specification into an absolute path, using default options. See absolute_file_name/3 for details on the options.
*/
absolute_file_name(V,Out) :- var(V),
 !,	% absolute_file_name needs commenting.
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

'$absolute_file_name'(File,LOpts,TrueFileName, G) :-
				%   must_be_of_type( atom, File ),
	( var(File) -> instantiation_error(File) ; true),
	abs_file_parameters(LOpts,Opts),
	current_prolog_flag(open_expands_filename, OldF),
	current_prolog_flag( fileerrors, PreviousFileErrors ),
	current_prolog_flag( verbose_file_search, PreviousVerbose ),
	get_abs_file_parameter( verbose_file_search, Opts, Verbose ),
	get_abs_file_parameter( expand, Opts, Expand ),
	set_prolog_flag( verbose_file_search,  Verbose ),
	get_abs_file_parameter( file_errors, Opts, FErrors ),
	get_abs_file_parameter( solutions, Opts, First ),
	(  FErrors == fail -> FileErrors = false ; FileErrors = true ),
	set_prolog_flag( fileerrors, FileErrors ),
	set_prolog_flag(file_name_variables, Expand),
	'$absf_trace'(File),
	'$absf_trace_options'(LOpts),
    HasSol = t(no),
    (
     % look for solutions
         '$find_in_path'(File, Opts,TrueFileName),
        (     (First == first -> ! ; nb_setarg(1, HasSol, yes) ),
	           set_prolog_flag( fileerrors, PreviousFileErrors ),
	           set_prolog_flag( open_expands_filename, OldF),
	           set_prolog_flag( verbose_file_search, PreviousVerbose ),
 	          '$absf_trace'(' |------- found  ~a', [TrueFileName])
	       ;
                set_prolog_flag( fileerrors, FileErrors ),
                set_prolog_flag( verbose_file_search, Verbose ),
	           set_prolog_flag( file_name_variables, Expand ),
	           '$absf_trace'(' |------- restarted search for  ~a', [File]),
	           fail
         )
 	 ;
	 % finished
	 %	 	stop_low_level_trace,
	 '$absf_trace'(' !------- failed.', []),
	 set_prolog_flag( fileerrors, PreviousFileErrors ),
	 set_prolog_flag( verbose_file_search, PreviousVerbose ),
	 set_prolog_flag(file_name_variables, OldF),
     % check if no solution
     arg(1,HasSol,no),
     FileErrors = error,
	 '$do_error'(existence_error(file,File),G)
	).

% This sequence must be followed:
% user and user_input are special;
% library(F) must check library_directories
% T(F) must check file_search_path
% all must try search in path
'$find_in_path'(user,_,user_input) :- !.
'$find_in_path'(user_input,_,user_input) :- !.
'$find_in_path'(user_output,_,user_ouput) :- !.
'$find_in_path'(user_error,_,user_error) :- !.
'$find_in_path'(Name, Opts, File) :-
%    (	atom(Name) -> true ; start_low_level_trace ),
    get_abs_file_parameter( file_type, Opts, Type ),
    get_abs_file_parameter( access, Opts, Access ),
    get_abs_file_parameter( expand, Opts, Expand ),
    '$absf_trace'('start with ~w', [Name]),
    '$core_file_name'(Name, Opts, CorePath, []),
    '$absf_trace'('  after name/library unfolding: ~w', [Name]),
    '$variable_expansion'(CorePath, Opts,ExpandedPath),
    '$absf_trace'('   after environment variable expansion: ~s', [ExpandedPath]),
    '$prefix'(ExpandedPath, Opts, Path , []),
    '$absf_trace'('    after prefix expansion: ~s', [Path]),
    atom_codes( APath, Path ),
    (
	   Expand = true
     ->
	    expand_file_name( APath, EPaths),
	     '$absf_trace'('     after shell globbing: ~w', [EPaths]),
	      lists:member(EPath, EPaths)
     ;
         EPath = APath
    ),
    real_path( EPath, File),
    '$absf_trace'('      after canonical path name: ~a', [File]),
    '$check_file'( File, Type, Access ),
    '$absf_trace'('       after testing ~a for ~a and ~a', [File,Type,Access]).

% allow paths in File Name
'$core_file_name'(Name, Opts) -->
    '$file_name'(Name, Opts, E),
    '$suffix'(E, Opts),
    '$glob'(Opts).

%
% handle library(lists) or foreign(jpl)
%
'$file_name'(Name, Opts, E) -->
    { Name =.. [Lib, P0] },
    !,
    { user:file_search_path(Lib, IDirs) },
    { '$paths'(IDirs, Dir ) },
    '$absf_trace'('  ~w first', [Dir]),
    '$file_name'(Dir, Opts, _),
    '$dir',
    { '$absf_trace'('  ~w next', [P0]) },
    '$cat_file_name'(P0, E).
'$file_name'(Name, _Opts, E) -->
    '$cat_file_name'(Name, E).


'$cat_file_name'(A/B, E	) -->
    '$cat_file_name'(A, _),
    '$dir',
    '$cat_file_name'(B, E).
'$cat_file_name'(File, F) -->
    { atom(File), atom_codes(File, F) },
    !,
    F.
'$cat_file_name'(File, S) -->
    {string(File), string_to_codes(File, S) },
    !,
    S.


'$variable_expansion'( Path, Opts, APath ) :-
    get_abs_file_parameter( expand, Opts, true ),
    !,
    '$expand_file_name'( Path, APath ).
'$variable_expansion'( Path, _, Path ).


'$var'(S) -->
    "{", !, '$id'(S), "}".
'$var'(S) -->
    '$id'(S).

'$drive' -->
    '$id'(_),
    ":\\\\".

'$id'([C|S]) --> [C],
	     { C >= "a", C =< "z" ;  C >= "A", C =< "Z" ;
	       C >= "0", C =< "9" ;  C =:= "_" },
	     !,
	     '$id'(S).
'$id'([]) --> [].


% always verify if a directory
'$check_file'(F, directory, _) :-
	!,
	exists_directory(F).
'$check_file'(_F, _Type, none) :- !.
'$check_file'(F, _Type, exist) :-
    '$access_file'(F, exist). % if it has a type cannot be a directory..
'$check_file'(F, _Type, Access) :-
    '$access_file'(F, Access),
    \+ exists_directory(F). % if it has a type cannot be a directory..

'$suffix'(Last, _Opts) -->
    { lists:append(_, [0'.|Alphas], Last), '$id'(Alphas, _, [] ) },
    '$absf_trace'(' suffix in ~s', [Last]),
    !.
'$suffix'(_, Opts) -->
    {
 	(
	    get_abs_file_parameter( extensions, Opts, Exts ),
	    Exts \= []
	 ->
	     lists:member(Ext, Exts),
	    '$absf_trace'(' trying suffix ~a from ~w', [Ext,Exts])
	 ;
	 get_abs_file_parameter( file_type, Opts, Type ),
	 ( Type == source -> NType = prolog ; NType = Type ),
	 user:prolog_file_type(Ext, NType)
	),
	'$absf_trace'(' trying suffix ~a from type ~a', [Ext, NType]),
	atom_codes(Ext, Cs)
    },
    '$add_suffix'(Cs).
'$suffix'(_,_Opts) -->
    '$absf_trace'(' try no suffix', []).

'$add_suffix'(Cs) -->
    { Cs = [0'. |_Codes] }
	->
	    Cs
	;
	".", Cs.

'$glob'(Opts) -->
    {
	get_abs_file_parameter( glob, Opts, G ),
	G \= '',
	atom_codes( G, Gs )
    },
    !,
    '$dir',
    Gs.
'$glob'(_Opts) -->
	[].

'$enumerate_glob'(_File1, [ExpFile], ExpFile) :-
    !.
'$enumerate_glob'(_File1, ExpFiles, ExpFile) :-
    lists:member(ExpFile, ExpFiles),
    file_base_name( ExpFile, Base ),
    Base \= '.',
    Base \='..'.

'$prefix'( CorePath, _Opts) -->
    { is_absolute_file_name( CorePath ) },
    !,
    CorePath.
'$prefix'( CorePath, Opts) -->
    {  get_abs_file_parameter( relative_to, Opts, Prefix ),
       Prefix \= '',
       '$absf_trace'('    relative_to ~a', [Prefix]),
       sub_atom(Prefix, _, 1, 0, Last),
       atom_codes(Prefix, S)
    },
    !,
    S,
    '$dir'(Last),
    CorePath.
'$prefix'( CorePath,  _) -->
    {
	recorded('$path',Prefix,_),
       '$absf_trace'('    try YAP path database ~a', [Prefix]),
       sub_atom(Prefix, _, _, 1, Last),
	atom_codes(Prefix, S)    },
    S,
    '$dir'(Last),
    CorePath.
'$prefix'(CorePath, _ ) -->
    '$absf_trace'('    empty prefix', []),
    CorePath.


'$dir' --> { current_prolog_flag(windows, true) },
	   "\\",
	   !.
'$dir' --> "/".

'$dir'('/') --> !.
'$dir'('\\') --> { current_prolog_flag(windows, true) },
	   !.
'$dir'(_) --> '$dir'.

%
%
%
'$system_library_directories'(library, Dir) :-
	user:library_directory( Dir ).
%	'$split_by_sep'(0, 0, Dirs, Dir).
'$system_library_directories'(foreign, Dir) :-
	foreign_directory( Dir ).
% compatibility with old versions
% search the current directory first.
'$system_library_directories'(commons, Dir) :-
	commons_directory( Dir ).


% enumerate all paths separated by a path_separator.
'$paths'(Cs, C) :-
    atom(Cs),
    (	current_prolog_flag(windows, true) -> Sep = ';' ; Sep = ':' ),
    sub_atom(Cs, N0, 1, N, Sep),
    !,
    (
	     sub_atom(Cs,0,N0,_,C)
     ;
        sub_atom(Cs,_,N,0,RC),
        '$paths'(RC, C)
    ).
'$paths'(S, S).

'$absf_trace'(Msg, Args ) -->
    { current_prolog_flag( verbose_file_search, true ) },
    { print_message( informational, absolute_file_path( Msg, Args ) ) },
    !.
'$absf_trace'(_Msg, _Args ) --> [].

'$absf_trace'(Msg, Args ) :-
    current_prolog_flag( verbose_file_search, true ),
    print_message( informational, absolute_file_path( Msg, Args ) ),
    !.
'$absf_trace'(_Msg, _Args ).

'$absf_trace'( File ) :-
	current_prolog_flag( verbose_file_search, true ),
	print_message( informational, absolute_file_path( File ) ),
	!.
'$absf_trace'( _File ).

'$absf_trace_options'(Args ) :-
	current_prolog_flag( verbose_file_search, true ),
	print_message( informational, arguments( Args ) ),
	!.
'$absf_trace_options'( _Args ).

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

