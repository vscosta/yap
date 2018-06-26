
:- compile(library(maplist)).
:- use_module(library(myddas)).

main :-
		 init,
		 main_,
		  close.
main_ :-
	go,
	fail.
main_ .

init :-

    start_low_level_trace,
    db_open(sqlite3, '/data/user/0/pt.up.yap.yapdroid/files/Yap/chinook.db', _, _),
    writeln('chinook has landed').

go :-
    stop_low_level_trace,
	db_import('Artist', artist),
    writeln(('Artist -> artist')),
	db_import('Album', album),
    writeln(('Album -> album')),
	db_import('Track', track),
    writeln(('Track -> track')).

	go :-
		db_get_attributes_types(album,Als),
    format('~w -> ~w~n',[album,Als]),
		db_get_attributes_types(track,Ts),
  	    format('~w -> ~w~n',[track,Ts]),
	db_get_attributes_types(artist,As),
	     format('~w -> ~w~n',[artist,As]).
go :-
	db_number_of_fields(album,Als),
	db_number_of_fields(track,Ts),
	db_number_of_fields(artist,As),
	writeln(As:Als:Ts).

go :-
		db_describe(album, Desc), writeln(album:Desc).
go :-
		db_describe(track, Desc), writeln(track:Desc).
go :-
			db_describe(artist, Desc), writeln(artist:Desc).
go :-
				db_show_tables(Desc), writeln(tables:Desc).
go :-
					db_show_tables(table(T)),
					db_describe(T,tableinfo(FieldID,Type,Null,Primary,Default,'')),
					 writeln(T:tableinfo(FieldID,Type,Null,Primary,Default,'')).

go :-
	go_cut0.


go :-
%stop_low_level_trace,
	findall(X:Y,artist(X,Y),Ls),
	length(Ls,Total),
	sort(Ls, LLs),
	length(LLs, T),
    writeln(T:Total).

go :-
	go_cut1.

go :-
	X=1,
    artist(X,Y),
    writeln(X:Y).

go_cut0 :-
% start_low_level_trace,
    artist(X,Y),
    writeln(X:Y),
    !.


go_cut1 :-
	X=1,
    artist(X,Y),
    writeln(X:Y),
    !.

close :-
	db_close.

:- main.

