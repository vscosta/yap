
:- compile(library(myddas)).

main :-
		 init,
		 main_,
		  close.
main_ :-
	go,
	fail.
main_ .

init :-
    db_open(sqlite3, '/data/user/0/pt.up.yap.yapdroid/files/Yap/chinook.db', _, _),
    writeln('chinook has landed'),
	db_import('Artist', artist),
	db_import('Album', album),
	db_import('Track', track).

	go :-
		db_get_attributes_types(album,Als),
		db_get_attributes_types(track,Ts),
		db_get_attributes_types(artist,As),
		writeln(As:Als:Ts).
go :-
	db_number_of_fields(album,Als),
	db_number_of_fields(track,Ts),
	db_number_of_fields(artist,As),
	writeln(As:Als:Ts).

go :-
		db_describe(album, Desc), writeln(Desc) ;
		db_describe(track, Desc), writeln(Desc) ;
			db_describe(artist, Desc), writeln(Desc).
go :-
				db_show_tables(Desc), writeln(Desc).
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

