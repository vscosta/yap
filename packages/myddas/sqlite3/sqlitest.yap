:- stop_low_level_trace.


:- use_module(library(lists)).
 :- use_module(library(maplist)).
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
       db_open(sqlite3, '/data/user/0/pt.up.yap.yapdroid/files/Yap/chinook.db', _, _),
    %   db_open(sqlite3, 'chinook.db', _, _),
    writeln('chinook has landed').

go :-
    writeln(('db_import')),
    db_import('artists', artists),
    writeln(('artist -> artists')),
	db_import('albums', albums),
    writeln(('albums -> albums')),
	db_import('tracks', tracks),
    writeln(('tracks -> tracks')).

go :-
    		writeln(db_get_attributes_types),
    		db_get_attributes_types(albums,Als),
    format('~w -> ~w~n',[albums,Als]),
		db_get_attributes_types(tracks,Ts),
  	        format('~w -> ~w~n',[tracks,Ts]),
	db_get_attributes_types(artists,As),
	     format('~w -> ~w~n',[artists,As]).
go :-
	writeln(db_number_of_fields),
	db_number_of_fields(albums,Als),
	db_number_of_fields(tracks,Ts),
	db_number_of_fields(artists,As),
	writeln(As:Als:Ts).

go :-

		db_describe(albums, Desc), writeln(albums:Desc).
go :-
		db_describe(tracks, Desc), writeln(tracks:Desc).
go :-
			db_describe(artists, Desc), writeln(artists:Desc).
go :-
				db_show_tables(Desc), writeln(tables:Desc).
go :-
					db_show_tables(table(T)),
					db_describe(T,tableinfo(FieldID,Type,Null,Primary,Default,'')),
					 writeln(T:tableinfo(FieldID,Type,Null,Primary,Default,'')).

go :-
    writeln(access),
	go_cut0.


go :-
%stop_low_level_trace,
	findall(X:Y,artists(X,Y),Ls),
	length(Ls,Total),
	sort(Ls, LLs),
	length(LLs, T),
    writeln(T:Total).

go :-
	go_cut1.

go :-
%	X=1,
    artists(X,Y),
    writeln(X:Y).

go_cut0 :-
    artists(X,Y),
    writeln(X:Y),
    !.


go_cut1 :-
%	X=1,
    artists(X,Y),
    writeln(X:Y),
    !.

close :-
	db_close.



:- initialization(main).
