:- include(r).


:- dynamic( real:r_started/1 ).


%%%

init_r_env :-
	getenv('R_HOME',Path),
	% done, except if in windows...
	\+ current_prolog_flag(windows, true),
	!,
     debug( real, 'Found R_HOME: ~a', [Path] ).
:- if(current_predicate(win_registry_get_value/3)).
init_r_env :-
	% windows is windows
        current_prolog_flag(windows, true),
	( HKEY='HKEY_LOCAL_MACHINE/Software/R-core/R';
		HKEY='HKEY_CURRENT_USER/Software/R-core/R' ),
        (
          catch(win_registry_get_value(HKEY,'Current Version', Version),_,fail)
        ->
          true
        ;
          catch(win_registry_get_subkey(HKEY, Version ), _, fail)
        ),
	!,
	atomic_list_concat([HKEY,Version],'/',SecondKey),
	win_registry_get_value(SecondKey,'InstallPath', RPath), !,
	setenv('R_HOME',RPath), % this probably does not help (at least not XPs)
	% now we need to have the DLL in our path
     % nicos: although on xp it seems that path has to already be set.
     ( current_prolog_flag(address_bits, 64) ->
          Psf = '\\bin\\x64'
          ;
          Psf = '\\bin\\i386'
     ),
     atomic_list_concat( [RPath,Psf], ToR ),
	install_in_ms_windows(ToR).
:- endif.


init_r_env :-
        current_prolog_flag(unix, true),
	% typical Linux 64 bit setup (fedora)
	current_prolog_flag(address_bits, 64),
	Linux64 = '/usr/lib64/R',
	exists_directory(Linux64), !,
        debug( real, 'Setting R_HOME to: ~a', [Linux64] ),
	setenv('R_HOME',Linux64).
init_r_env :-
        current_prolog_flag(unix, true),
	% typical Linux  setup (Ubuntu)
	Linux32 = '/usr/lib/R',
	exists_directory( Linux32 ), !,
     debug( real, 'Setting R_HOME to: ~a', [Linux32] ),
	setenv('R_HOME',Linux32).
% nicos, fixme: Linux multilib ?
init_r_env :-
    % typical MacOs setup
    current_prolog_flag(apple, true),
    init_in_osx.
init_r_env :-
            r_home_postfix( PostFix),
            absolute_file_name( path('R'), Rhome,
			 [ solutions(all),
			   file_type(directory),
               expand(true),
               glob(PostFix)
			 ] ),	!,
        debug( real, 'Setting R_HOME to bin relative: ~a', [Rhome] ),
	setenv('R_HOME',Rhome).

init_r_env :-
    popen('R RHOME',read,S),
    read_line_to_string(S,Lc),
    close(S),
    Lc \= end_of_file,
    !,
    setenv('R_HOME',Lc).

init_r_env :-
     throw(
     error(r_root) ).

% track down binary through symbolic links...
dirpath_to_r_home( This0, Rhome ) :-
	read_link(This0, _, This), !,
	dirpath_to_r_home( This, Rhome ).
dirpath_to_r_home( This, Rhome ) :-
     file_directory_name( This, R1 ),
     file_base_name(R1, Execdir) ->
     ( Execdir == bin ->
       Rhome = R1
     ;
       % windows with multiple binaries
       file_directory_name( R1, R2 ),
       file_base_name(R2, bin),
       file_directory_name( R2, Rhome )
     ).

r_home_postfix( 'lib64/R' ) :-
	current_prolog_flag(address_bits, 64).
r_home_postfix( 'lib/R' ).

to_nth( [To|T], To, T ) :- !.
to_nth( [_H|T], To, Right ) :-
	to_nth( T, To, Right ).


:- dynamic r_started/1.

%%	start_r.
%	Start an R object. This is done automatically upon loading the library.
%    Only 1 instance should be started per Prolog session.
%    Multiple sessions will be ignored silently.
%
start_r :-
      r_started( true ),
 !.
 start_r :-
	swipl_wins_warn,
	init_r_env,
	init_R,
	set_prolog_flag(double_quotes, string ),
	assert( r_started(true) ).
start_r.

%%	end_r.
%
%    End the connection to the R process.
end_r :-
	% so that module systems doesn't complain when
	% initialisation fails to find R.
	stop_R.


:- initialization(at_halt(halt_r),now).

:- initialization(start_r, now).

