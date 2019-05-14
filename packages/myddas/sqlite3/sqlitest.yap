

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- use_module(library(myddas)).

main :-
		 init,
		 main_,
		  close.
main_ :-
	catch(go,E,writeln(E)),
	fail.
main_ .

init :-
    catch(db_open(sqlite3,con,('chinook.db'),_,_), _, fail),
    %   db_open(sqlite3, 'chinook.db', _, _),
    writeln('chinook has landed').

go :-
    writeln(('db_import')),
    db_import(con,'artists', artists),
    writeln(('artist -> artists')),
	db_import(con,'albums', albums),
    writeln(('albums -> albums')),
	db_import(con,'tracks', tracks),
    writeln(('tracks -> tracks')).


go :-
%	X=1,
    artists(X,Y),
    writeln(X:Y).
go :-
    		writeln(db_get_attributes_types),
    		db_get_attributes_types(con,albums,Als),
    format('~w -> ~w~n',[albums,Als]),
		db_get_attributes_types(con,tracks,Ts),
  	        format('~w -> ~w~n',[tracks,Ts]),
	db_get_attributes_types(con,artists,As),
	     format('~w -> ~w~n',[artists,As]).
go :-
	writeln(db_number_of_fields),
	db_number_of_fields(con,albums,Als),
	db_number_of_fields(con,tracks,Ts),
	db_number_of_fields(con,artists,As),
	writeln(As:Als:Ts).

go :-

		db_describe(con,albums, Desc), writeln(albums:Desc).
go :-
		db_describe(con,tracks, Desc), writeln(tracks:Desc).
go :-
			db_describe(con,artists, Desc), writeln(artists:Desc).
go :-
				db_show_tables(con,Desc), writeln(tables:Desc).
go :-
					db_show_tables(con,table(T)),
					db_describe(con,T,tableinfo(FieldID,Type,Null,Primary,Default,'')),
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
	db_close(con).


:- initialization(main, now).
