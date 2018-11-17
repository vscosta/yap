/***********************************************************

  load a program into a graph, but do not actually consult it.

  ***********************************************************/

load( D, _OMAP ) :-
        working_directory(_, D),
        fail.
load( _, _Map ) :-
	 % from libraries outside the current directories
        assert( node( attributes, woken_att_do/4, 'library/atts.yap', prolog ) ),
        fail.
load( _ , Dirs ) :-
        dirs( Dirs ),
    %%% phase 1: find modules
        nb_setval( current_module, user ),
        nb_setval( private, false ),
        nb_setval( file_entry, user:user ),
        init_loop( Dirs ),
        maplist(scan_dir, Dirs).

scan_dir( Dir -user) :-
        pl_interfs(0, Dir-user ),
    %%% phase 2: find C-code predicates
        c_preds( Dir-user ).
% the c-builtins do not depend on prolog code.
scan_dir( Dir -prolog) :-
        c_preds( Dir-user ),
        pl_interfs(0, Dir-user ).
    %%% phase 2: find C-code predicates

dirs( Roots ) :-
    member( Root-_, Roots ),
    absolute_file_name( Root, FRoot ),
        rdir( FRoot ),
        fail.
dirs( _Roots ).

rdir( FRoot ) :-
        absolute_file_name( FRoot, [glob(*), solutions(all), file_errors(fail)], File ),
        \+ doskip( File ),
         (
             cat                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    zzch( file_property( File, type(directory) ), _, fail )
             ->
         assert_new( dir( File ) ),
         assert_new( sub_dir( FRoot, File ) ),
         rdir( File )
         ;
         file_base_name(File, B),
            assert( file(File, B) )
            ),
        fail.
rdir(_).

c_preds(Dir - Mod) :-
%	format('%~*| C ************* ~a\n', [1,Dir]),
    atom( Dir ),
        absolute_file_name( Dir, [glob(*), solutions(all), file_errors(fail)], File ),
	( ( sub_atom(File,_,_,0,'.c')
	  ;
	    sub_atom(File,_,_,0,'.i')
	  ;
	    sub_atom(File,_,_,0,'.C')
	  ;
	    sub_atom(File,_,_,0,'.cpp')
                                                                         	  ;
	    sub_atom(File,_,_,0,'.icc')
	  ;
	    sub_atom(File,_,_,0,'.h')
	  ) ->
	  \+ doskip( File ),
	  c_file( File , Mod )
	;
	  exists_directory( File ),
	  \+ doskip( File ),
	  c_preds( File - Mod )
	),
	fail.
c_preds(_).


c_file(F, _Mod) :-
	consulted( F, _ ),
	!.
c_file(F, Mod) :-
	assert( consulted( F, Mod ) ),
	nb_setval( current_module, Mod ),
	open(F, read, S, [alias(c_file)]),
	repeat,
	read_line_to_codes( S, Codes ),
	( Codes == end_of_file
	->
	  !,
	  close(S)
	;
	  append( _, "PL_extension", Codes),
                                %writeln(Fields),
	  c_ext(S, Mod, F),
	  fail
	;
	  split(Codes, ",; ()\t\"\'", Fields), %'
                                %writeln(Fields),
	  line_count(S, Lines),
	  c_line(Fields , Mod, F:Lines),
	  fail
	).

c_line(["}"],  Mod, _) :- !,
	nb_setval( current_module, Mod ).
c_line(Line, _Mod, _) :-
	append( _, [ "CurrentModule", "=", M|_], Line),
	system_mod(M, _Mod, Mod, _),
	nb_setval( current_module, Mod ).
c_line(Line,  Mod, F: LineP) :-
	break_line( Line, N/A, Fu),
	assert( node( Mod, N/A, F-LineP, Fu ) ),
	handle_pred( Mod, N, A, F ).

c_ext( S, Mod, F ) :-
	repeat,
    stream_property( S, position(Pos) ),
	read_line_to_codes( S, Codes ),
	(
     Codes == end_of_file
    ->
     !
    ;
    string_codes(String, Codes),
	 ( sub_string( Codes, _, _, _, `NULL` )
    ->
	 !
	;
	 split_string(String, `,; (){}\t\"\'`, [`FRG`, NS,AS,FS|_]),
	 atom_string(N,NS),
	 atom_string(Fu,FS),
	 catch( number_string(A, AS), Error, handle( String , Error ) ),
	 stream_position_data( line_count, Pos, Line ),
	 assert( node( Mod , N/A,  F-Line, Fu ) ),
	 handle_pred( Mod, N, A, F )
	;
	 split_string(String , `,; (){}\t\"\'`, [NS,AS,FS|_]),
	 atom_string(N,NS),
	 atom_string(Fu,FS),
	 catch( number_string(A, AS),  Error, handle( String , Error ) ),
     break_line( Line, N/A, swi(Fu)) ,
	 assert( node( Mod, N/A, F-Line, Fu ) ),
	 handle_pred( Mod, N, A, F )
	)
    ).


break_line( Line, N/A, c(Fu)) :-
	take_line( Line, NS, AS, FS ), !,
	atom_codes(N,NS),
	atom_codes(Fu,FS),
	catch( number_codes(A, AS), Error, handle( Line, Error ) ).
break_line( Line, N/A, swi(Fu)) :-
	take_line( Line, NS, AS, FS ), !,
	atom_codes(N,NS),
	catch( number_codes(A, AS), Error, handle( Line, Error ) ),
	append(["pl_",FS,"_",A,"_va"], FuS),
    atom_codes(Fu,FuS).
break_line( Line, N/A, bp(Fu)) :-
	take_line( Line, NS, AS, FS ), !,
	atom_codes(N,NS),
	catch( number_codes(A, AS), Error, handle( Line, Error ) ),
	append(["pc_",FS,"_",A], FuS),
    atom_codes(Fu,FuS).
break_line( Line, N/A, c(FuE, FuB)) :-
	take_line( Line, NS, AS, FSE, FSB ), !,
	atom_codes(N,NS),
	atom_codes(FuE,FSE),
	atom_codes(FuB,FSB),
	atom_codes(A, AS).

take_line( Line, NS, AS, FS ) :-
	append( _, [ "Yap_InitCPred", NS, AS, FS|_], Line), !.
take_line( Line, NS, AS, FS ) :-
	append( _, [ "Yap_InitAsmPred", NS, AS, _, FS|_], Line), !.
take_line( Line, NS, AS, FS ) :-
	append( _, [ "Yap_InitCmpPred", NS, AS, FS|_], Line), !.
take_line( Line, NS, AS, FS ) :-
	append( _, [ "Yap_InitCmpPred", NS, AS, FS|_], Line), !.
take_line( Line, NS, AS, FS ) :-
	append( _, [ "YAP_UserCPredicate", NS, FS, AS|_], Line), !.
take_line( Line, NS, AS, FS ) :-
	append( _, [ "PRED", NS0, AS, FS|_], Line), !,
	append( ["pl_", NS0, AS, "_va"], NS ).
take_line( Line, NS0, AS, FS ) :-
	append( _, [ "PRED_IMPL", NS0, AS, FS|_], Line), !,
	append( ["pl_", NS0, AS, "_va"], FS ).
take_line( Line, NS, AS, FS ) :-
	append( _, [ "PL_register_foreign", NS, AS, FS|_], Line), !.
take_line( Line, NS, AS, FS ) :-
	append( _, [ "PRED_DEF", NS, AS,_FS|_], Line), !,
	append( ["pl_", NS, AS, "_va"], FS ).
take_line( Line, NS, AS, FS ) :-
	append( _, [ "FRG", NS, AS, FS|_], Line), !.
                                % from odbc
take_line( Line, NS, AS, FS ) :-
	append( _, [ "NDET", NS, AS, FS|_], Line), !.
take_line( Line, NS, AS, FS ) :-
	append( _, [ "DET", NS, AS, FS|_], Line), !.


take_line( Line,  AS, FS ) :-
	append( _, [ "REGISTER_CPRED", FS, AS], Line), !.


take_line( Line, NS, AS, FSE, FSB ) :-
	append( _, [ "Yap_InitCPredBack", NS, AS, _, FSE, FSB|_], Line), !.

system_mod("ATTRIBUTES_MODULE", _, attributes, user ).
system_mod("HACKS_MODULE", _, '$hacks' , sys ).
system_mod("USER_MODULE", _, user, user ).
system_mod("DBLOAD_MODULE", _, '$db_load', sys ).
system_mod("GLOBALS_MODULE", _, globals, sys ).
system_mod("ARG_MODULE", _, arg, sys  ).
system_mod("PROLOG_MODULE", _ , prolog, sys ).
system_mod("RANGE_MODULE", _, range, user ).
system_mod("SWI_MODULE", _, swi, sys ).
system_mod("OPERATING_SYSTEM_MODULE", _, system , sys ).
system_mod("TERMS_MODULE", _, terms , sys).
system_mod("SYSTEM_MODULE", _, system, sys ).
system_mod("IDB_MODULE", _, idb, user ).
system_mod("CHARSIO_MODULE", _, charsio, sys ).
system_mod("cm", M, M, user ).

call_c_files(  File, Mod, _Fun, [CFile] ) :-
	search_file( CFile, File, c, F ),
	c_file(F, Mod).
call_c_files(  File, Mod, _Fun, CFile ) :-
	CFile \= [_|_],
	search_file( CFile, File, c, F ),
	c_file(F, Mod).

%

:- dynamic undo/3.

directive(G, F, M) :-
    undo(G,F,M),
    !.
directive(set_prolog_flag(Fl,V), F, M) :-
    !,
        prolog_flag(Fl, O,  V),
        asserta( undo( set_prolog_flag(Fl, O), F, M)),
        set_prolog_flag(Fl, V).
directive(yap_flag(Fl,V), F, M) :-
    !,
        prolog_flag(Fl, O,  V),
        asserta( undo( set_prolog_flag(Fl, O), F, M)),
        set_prolog_flag(Fl, V).
directive(yap_flag(Fl,O,V), F, M) :-
    !,
        prolog_flag(Fl, O,  V),
        asserta( undo( set_prolog_flag(Fl, O), F, M)),
        set_prolog_flag(Fl, V).
directive(op(X,Y,O), _F, M) :-
    !,
        op( X, Y, M:O).
directive(G, F, M) :-
    assert(M:G, R),
        asserta( undo(erase(R), F, M)).

clean_up(F, M) :-
    undo( G , F, M),
    call( G ),
    fail.
clean_up(_,_).

%
%
%
%
pl_interfs(Lev0, Dir - Mod) :-
	\+ ( fullskip( Dir ) ),
%	format('%~*| ************* ~a\n', [Lev0,Dir]),
    Lev is Lev0+1,
    nb_setval( current_module, Mod ),
	atom( Dir ),
	directory_files( Dir , Files),
	member( File, Files ),
	atom_concat([Dir,'/',File], Path),
	( ( sub_atom(File,_,_,0,'.yap') ; sub_atom(File,_,_,0,'.pl') ; sub_atom(File,_,_,0,'.ypp') ) ->
	  absolute_file_name( Path, APath ),
	  pl_interface( APath , Mod, Lev )
	;
	  exists_directory( Path ),
	  \+ atom_concat(_, '/.', Path),
	  \+ atom_concat(_, '/..', Path),
	  \+ atom_concat(_, '/.git', Path),
	  absolute_file_name( Path, APath ),
	  \+ doskip( APath ),
	  pl_interfs( Lev0, APath - Mod )
        	),
	fail.
pl_interfs(_, _).

%%
% pl_interface( File, Mod, Level)
% adds a node to the file graph and marks which files are modules
%
% main side-effect facts like edge( F0-Mod:File )
%                             exported( ( FMNATarget :- FMNASource ) ) ou exported(F-M, Op ),
%                             module_on ( M, File )
                                %
pl_interface(F, Mod, _Lev) :-
    module_on( F , _Mod, L ),
    maplist( private(F, Mod), L ),
    !.
pl_interface(F, Mod, _) :-
	consulted(F, Mod ),
	!.
pl_interface(F, Mod, Lev) :-
%    format('~*|------------------------- ~a~n',[Lev,F]),
%   ( sub_atom(F,_,_,_,'matrix.yap') -> spy get_interf ; true ),
%   ( sub_atom( F, _, _, 0, 'gecode.yap' ) -> spy user_deps; true ),
%    ( F = '/Users/vsc/git/yap-6.3/library/examples/mat.yap' -> trace ; true ),
	assert_new(consulted(F, Mod ) ),
	nb_getval( current_module, M0 ),
	nb_getval( private, Default ),
	nb_setval( private, false ),
	nb_getval( file_entry, OF:OMod ),
	nb_setval( file_entry, F:Mod ),
	preprocess_file( F, PF ),
	catch( open(PF, read, S, [script(true)]) , _, fail ),
	repeat,
	nb_getval( current_module, MR ),
 	catch( read_clause( S, T, [module( MR ),term_position(Pos),comments(Comment)] ), Throw, loop_error( S, MR:Throw)),

	( T == end_of_file
	->
      !,
      close(S),
	 (
	  c_dep( F, Fc),
	  c_file( Fc, MR ),
	  fail
     % cleanup
	 ;
	  module_on( F , _M, _Is)
	 ->
             % also, close ops defined in the module M, if M \= Mod
	  nb_setval( private, Default ),
	  nb_setval( file_entry, OF:OMod )
	 ;
	  true
	 ),
      clean_up( MR, F ),
      nb_setval( current_module, M0 )

%    writeln('***************************<<<<<<<<<<<'-M0),
%      (current_op(X,Y,O), write(M0:O), fail;nl)
	;
	 nb_getval( current_module, MC0 ),
	 stream_position_data( line_count, Pos, Line ),
	 nb_setval( line, Line ),
	 ( Mod == prolog -> MC = prolog ; MC = MC0 ),
      Lev1 is Lev+1,
	 get_interface( T, F, MC, Lev1  ),
     get_graph( T, F, Pos, MC ),
    fail
	).

get_interface( T, _F, _M0, _ ) :-
    %    ( T = (:- op(_,_,_)) -> trace ; true ),
	var(T),
	!.
%% switches to new file n
get_interface( (:- D ), F, M , Lev) :-
    !,
     get_directive( D, F, M, Lev ).
get_interface( (?- _ ), _F, _M , _Lev) :-
    !.
get_interface( T, F, M0 , _Lev) :-
    always_strip_module( M0:T, M, NT),
    (
        NT = goal_expansion(_,_) ;
        NT = goal_expansion(_,_,_);
        NT = term_expansion( _, _ )
    ),
    !,
    catch(directive(NT, F, M), Error, loop_error(c_file,Error)).
get_interface( ( M:H :- _B), F, _M , _Lev) :-
    !,
	functor( H, N, A),
	handle_pred( M, N, A, F ).
% not the time t
get_interface( (H :- _B), F, M , _Lev) :-
    !,
	functor( H, N, A),
	handle_pred( M, N, A, F ).
get_interface( G , F, M , _Lev) :-
	functor( G, N, A),
	handle_pred( M, N, A, F ).

get_directive( V , _F, _M , _Lev) :-
	var( V ),
	!.
get_directive( module( NM0, Is ), F, _M , _Lev) :-
    !,
    (
     (NM0 = system(NM) -> true ; NM0 = system(NM,_) -> true ; NM = NM0 ),
     assert(module_file( F, NM ) ),
     nb_setval( current_module, NM ),
     assert( module_on( F , NM, Is) ),
     maplist( public(F, NM), Is ),
     nb_setval( private, true )
    ->
     true
    ;
     writeln(oops:module( NM0, Is )),
     fail
    ).
    get_directive( reexport( Loc, Is ), F, M , Lev) :-
      !,
      search_file( Loc, F, prolog, F1),
      pl_interface(F1, M, Lev),
      module_on( F1 , NM, Is0),
      (var(Is) ->
        Is = Is0
        ;
        true
        ),
        % extend the interface.rg
        retract( module_on( F , M, IsOld) ),
        append( Is, IsOld, NIs ),
        assert( module_on( F , M, NIs) ),
        maplist( exported(F, M, F1, NM), NIs ).
get_directive(  use_module( Loc, Is ), F, M , Lev) :-
    !,
    include_files(  F, M, Is, Lev, Loc ).
get_directive(  use_module( Loc ), F, M , Lev) :-
    !,
    include_files0( F, M, Lev, Loc ).
%       nb_getval(current_module,MM), writeln(NM:MM:M).
get_directive(  use_module( Loc, Is, _ ), F, M , Lev) :-
    !,
    include_files( F, M, Is, Lev, Loc).
get_directive(  consult( Files ), F, M , Lev) :-
    !,
    include_files0(  F, M , Lev, Files).
get_directive(  reconsult( Files ), F, M , Lev) :-
    !,
    include_files0(  F, M, Lev, Files ).
get_directive(  ensure_loaded( Files ), F, M , Lev) :-
    !,
    include_files0(  F, M, Lev, Files ).
get_directive(  include( Files ), F, M , Lev) :-
    !,
    include_files0(  F, M, Lev, Files  ).
get_directive(  load_files( Files , [_|_] ), F, M , Lev) :-
    !,
    include_files0(  F, M,  Lev, Files ).
get_directive(  bootstrap( Files ), F, M , Lev) :-
    !,
    include_files0(  F, M, Lev, Files ).
get_directive( ( G -> _ ; _ ) , F, M, Lev) :-
    !,
    get_directive(  G   , F, M, Lev ).
get_directive(  catch( G , _, _ ) , F, M, Lev) :-
    !,
    get_directive(  G , F, M, Lev).
get_directive( initialization( G , now ) , F, M, Lev) :-
    !,
    get_directive( G  , F, M, Lev).
get_directive( load_files( Files , [_|_] ), F, M , Lev) :-
    !,
    include_files0(  F, M, Lev, Files ).
get_directive( [] , _F0, _M , _Lev) :- !.
get_directive(  [F1|Fs] , F, M , Lev) :-
    strip_module( M:F, M1, F1),
    !,
    include_files0(  F, M1, Lev, F1 ),
    get_directive(  Fs , F, M , Lev).
% don't actually use \this one.
get_directive(  load_foreign_files(Fs, _, Fun), F, M , _Lev) :-
    !,
    call_c_files(  F, M, Fun, Fs ).
get_directive(  load_foreign_library(F), F0, M , _Lev) :-
    !,
    always_strip_module(M:F, M1, F1),
    call_c_files(  F0, M1, '', F1 ).
get_directive(  load_foreign_library(F,Fun), F0, M , _Lev) :-
    !,
    always_strip_module(M:F, M1, F1),
    call_c_files(  F0, M1, Fun, F1 ).
get_directive( use_foreign_library(F), F0, M , _Lev) :-
!,
    always_strip_module(M:F, M1, F1),
    call_c_files(  F0, M1, '', F1 ).
get_directive( system_module( _NM, _Publics, _Hiddens), _F, _M , _Lev) :-
    nb_setval( current_module, prolog ),
    !.
get_directive( style_checker( _ ), _F, _M , _Lev) :-
    !.
get_directive( dynamic( T ), F, M , _Lev) :-
    !,
    declare_functors( T, F, M ).
get_directive( multifile( T ), F, M , _Lev) :- % public?
    !,
    declare_functors( T, F, M ).
get_directive( meta_predicate( T ), F, M , _Lev) :-!,
    declare_terms( T, F, M ),	% public?
    !.
get_directive( '$install_meta_predicate'( H, M), F, __M , _Lev) :-
    !,
    declare_functors( H, F, M ).
get_directive( thread_local( T ), F, M , _Lev) :-
    !,
    declare_functors( T, F, M ).
get_directive( op( X, Y, Z), _F, M , _Lev) :-
    !,
    new_op(M,M,X,Y,Z).
get_directive( record( Records ), F, M , _Lev) :-
    !,
    handle_record( Records, F, M).
get_directive( set_prolog_flag(dollar_as_lower_case,On), F, M , _Lev) :-
    !,
    catch(directive(set_prolog_flag(dollar_as_lower_case,M:On), F), Msg, loop_error(c_file, Msg) ).

% support SWI package record
handle_record( (Records1, Records2), F, M ) :-
	!,
	handle_record( Records1, F, M ),
	handle_record( Records2, F, M ).
handle_record( Record, F, M ) :-
	Record =.. [Constructor|Fields],
	atom_concat(Constructor, '_data', Data),
	handle_pred( M, Data, 3, F),
	atom_concat(default_, Constructor, New),
	handle_pred( M, New, 1, F),
	atom_concat(is_, Constructor, Is),
	handle_pred( M, Is, 1, F),
	atom_concat(make_, Constructor, Make),
	handle_pred( M, Make, 2, F),
	handle_pred( M, Make, 3, F),
	atom_concat([set_, Constructor,'_fields'], Sets),
	handle_pred( M, Sets, 3, F),
	handle_pred( M, Sets, 4, F),
	atom_concat([set_, Constructor,'_field'], Set),
	handle_pred( M, Set, 3, F),
	maplist( handle_record_field( Constructor, F, M) , Fields ).

handle_record_field( Constructor, F, M, Name:_=_ ) :-
	!,
	handle_record_field_name( Constructor, F, M, Name).
handle_record_field( Constructor, F, M, Name:_ ) :-
	!,
	handle_record_field_name( Constructor, F, M, Name).
handle_record_field( Constructor, F, M, Name=_ ) :-
	!,
	handle_record_field_name( Constructor, F, M, Name).
handle_record_field( Constructor, F, M, Name ) :-
	handle_record_field_name( Constructor, F, M, Name).

handle_record_field_name( Constructor, F, M, Name) :-
	atom_concat([ Constructor,'_', Name], Val),
	handle_pred( M, Val, 2, F),
	atom_concat([ set_, Name, '_of_', Constructor ], Set),
	handle_pred( M, Set, 3, F),
	handle_pred( M, Set, 2, F),
	atom_concat([ nb_set_, Name, '_of_', Constructor ], Set),
	handle_pred( M, Set, 3, F),
	handle_pred( M, Set, 2, F).

handle_pred( M, N, A, F ) :-
	(
	 system_mod( _, _, M, sys )
	->
	 (
	  atom_concat('$',_,N)
	 ->
	  private( F, M, N/A  )
	 ;
	  public( F, M, N/A  )
	 )
	;
	 ( nb_getval( private, false )
	 ->
	   public( F, M, N/A  )
	 ;
	   private( F, M, N/A  )
	 )
	).

exported( _NF, _F, NM, M, op(X,Y,Z)) :-
	!,
	new_op(M, NM, X,Y,Z).
exported( NF, F, NM, M, N/A) :- !,
    % sink no more
	assert_new_e( F,M:N/A , NF, NM:N/A  ).
exported( NF, F, NM, M, N/A as NN) :- !,
    % sink no more
	assert_new_e( F,M:NN/A , NF,NM:N/A  ).
exported( NF, F, NM, M, N//A) :- !,
	A2 is A+2,
    % sink no more
	assert_new_e( F,M:N/A2 , NF, NM:N/A2 ).
exported( NF, F, NM, M, N//A as NN) :- !,
	A2 is A+2,
    % sink no more
	assert_new_e( F, M:NN/A2 , NF, NM:N/A2  ).



include_files0( F, M, Lev, Files ) :-
	include_files( F, M, _Is, Lev, Files ).

include_files( F, M, Is, Lev, Files ) :-
	maplist( include_files( F, M, Is, Lev ), Files ),
	!.
include_files( F, M, Is, Lev, -Files ) :-
	!,
	include_files(  F, M, Is, Lev, Files).
include_files( F, M, Is, Lev, Files ) :-
	!,
	always_strip_module(M:Files, M1, NFiles),
	include_file( F, M1, Is, Lev, NFiles ).

include_file( F, M, Is, Lev, Loc ) :-
	is_list( Loc ), !,
	maplist( include_file( F, M, Is, Lev), Loc ).
include_file( F, M, Is0, Lev, Loc ) :-
                                % depth visit\
	(
 	nb_getval( private, Private ), % find the file
	once( search_file( Loc, F, prolog, NF ) ),
     pl_interface(NF, M, Lev),
    % should verify Is in _Is
    %trace,
     ( module_on(NF, NM, Is)
     ->
       ( var(Is0) -> Is = Is0 ; true ),
       maplist( exported( NF, F, NM, M), Is0 )
     ;
       true
     ),
     nb_setval( private, Private )
     ->
    true
    ;
    writeln(bad_include_file( F, M, Is0, Lev, Loc )),
     fail
    ).

source_files( F, M, Lev, Files ) :-
	maplist( source_files( F, M, Lev ), Files ),
	!.
source_files( F, M, Lev, Loc ) :-
	source_file( F, M, Lev, Loc ).

source_file( F, M, Loc, Lev ) :-
	once( search_file( Loc, F, prolog, NF ) ),
	% depth visit
	pl_source(NF, F, M, Lev).	% should verify Is in _Is

pl_source(F, F0, Mod, Lev) :-
	nb_getval( current_module, MR0 ),
    	preprocess_file( F, PF ),
%    	format('%~*| ************* ~a\n', [Lev,PF]),
	catch( open(PF, read, S, []) , _, fail ),
	repeat,
	nb_getval( current_module, MR ),
	%( sub_atom(F,_,_,_,'examples/matrix.yap') ->  spy get_interf ; nospyall ),
	catch( read_clause( S, T, [module( MR ),term_position(Pos)] ), Throw, loop_error( S, Throw)),
	(
	 T == end_of_file
	->
	 !,
     nb_setval( current_module, MR ),
	 close(S),
     	nb_setval( current_module, MR0 )
	;
	 nb_getval( current_module, MC0 ),
	 stream_position_data( line_count, Pos, Line ),
	 nb_setval( line, Line ),
	 ( Mod == prolog -> MC = prolog ; MC = MC0 ),
	 get_interface( T, F0, Lev, MC  ),
	 fail
	).

declare_functors( T, _F, _M1) :-  var(T), !,
	error( unbound_variable ).
declare_functors( M:T, F, _M1) :-  !,
	declare_functors( T, F, M).
declare_functors( (T1,T2), F, M1) :-  !,
	declare_functors( T1, F, M1),
	declare_functors( T2, F, M1).
declare_functors( Ts, F, M1) :-
	maplist( declare_functor( F, M1), Ts ), !.
declare_functors( T, F, M1) :-
	declare_functor( F, M1, T).

declare_functor(File, M, N/A) :-
	handle_pred( M, N, A, File ).

declare_terms( T, _F, _M1) :-  var(T), !,
	error( unbound_variable ).
declare_terms( M:T, F, _M1) :-  !,
	declare_functors( T, F, M).
declare_terms( (N1,N2), F, M) :-
	number(N1),
	number(N2),
	!,
	declare_term(  F, M, (N1,N2)).
declare_terms( (T1,T2), F, M1) :-  !,
	declare_terms( T1, F, M1),
	declare_terms( T2, F, M1).
declare_terms( Ts, F, M1) :-
	maplist( declare_term( F, M1), Ts ), !.
declare_terms( T, F, M1) :-
	declare_term( F, M1, T).

declare_term(F, M, S) :-
	functor(S, N, A),
	handle_pred( M, N, A, F ).

handle(Line, Error ) :-
    format('~s caused Error ~w~n~n', [Line, Error]),
    fail.
