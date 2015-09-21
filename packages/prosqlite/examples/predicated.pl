
:- ensure_loaded( library(prosqlite) ).
:- ensure_loaded( library(debug) ).

predicated :-
     Sfile = 'phones.sqlite',
     does_not_exist( Sfile ),
     create_db, 
     interrogate,
     write( 'Deleting database file (phones.sqlite).' ), nl,
     delete_file( Sfile ),
     write( 'Trying to access expired predicate...' ), nl,
     end_bit,
     nodebug( sqlite ),
     write( 'All done.' ), nl.

end_bit :-
     catch(phones(name=naku,telephone=T),Excp,(write(caught(Excp)),nl,fail)),
     !,
     write( not_really(T) ), nl.
end_bit.

create_db :-
     sqlite_connect( phones, phones_db, exists(false) ),
     create( C ),
     sqlite_query( phones_db, C, _AffC ), 
     % move to dbf example
     % sqlite_create( phones_db, phones(name+text,telephone-text,address-text) ),
     close_db.

interrogate :-
     write( 'Testing arity(arity) option (as default).' ), nl,
     sqlite_connect( phones, phones_db, as_predicates(true) ),
     forall( insert(I), (sqlite_query(phones_db,I,Row),write(Row),nl) ),
     report_phones_db,
     findall( U-T, phones(U,T,_), UTs ), write( uts(UTs) ), nl,
     findall( T, phones(naku,T,_), Ts ), write( naku(Ts) ), nl,
     close_db, 

     write( 'Testing arity(both) option.' ), nl,
     Opts1 = [as_predicates(true),arity(both)], 
     sqlite_connect( phones, phones_db, Opts1 ),
     findall( U-T, phones(U,T,_), UT1s ), write( uts(UT1s) ), nl,
     findall( T, phones(naku,T,_), T1s ), write( naku(T1s) ), nl,
     findall( U-T, phones([name=U,telephone=T]), UT2s ),
     write( uts_unary(UT2s) ), nl,
     findall( T, phones([name=naku,telephone=T]), T2s ),
     write( naku_unary(T2s) ), nl,
     close_db, 

     write( 'Trying to map to another predicate name.' ), nl,
     Opts2 = [as_predicates(true),table_as(phones,catalog,unary)],
     sqlite_connect( phones, phones_db, Opts2 ),
     findall( U-T, catalog([name=U,telephone=T]), UT2s ),
     write( uts_catalog(UT2s) ), nl,
     findall( T, catalog([name=naku,telephone=T]), T2s ),
     write( naku_cataloh(T2s) ), nl,
     close_db, 

     Mess3='Testing palette value of sqlite_connect/3\'s arity() option.',
     write( Mess3 ), nl,
     Opts3 = [as_predicates(true),arity(palette)],
     sqlite_connect( phones, phones_db, Opts3 ),
     findall( U-T, phones( name=U,telephone=T ), UT4s ),
     write( uts_binary(UT4s) ), nl,
     findall( U-T, phones([name=U,telephone=T]), UT3s ),
     write( uts_unary_list(UT3s) ), nl,
     findall( T, phones([name=naku,telephone=T]), T3s ),
     write( naku_unary_list(T3s) ), nl,
     findall( T, phones( name=naku,telephone=T ), T4s ),
     write( naku_binary(T4s) ), nl,
     report_phones_db,
     sqlite_disconnect( phones_db ),
     write( 'Closed database.' ), nl, nl.

report_phones_db :-
     write( 'Select * from phones : ' ), nl,
     write( '---' ), nl,
     findall( Row, (sqlite_query(phones_db,'Select * from phones;',Row),write(Row),nl), _ ),
     write( '---' ), nl.

create( C ) :-
     C = 'CREATE TABLE phones (name text, telephone text, address text, Primary Key (name) );'.
     
insert( I ) :-
     I = 'Insert into phones (name, telephone, address) values ("naku","0044","uk");'.
insert( I ) :-
     I = 'Insert into phones (name, telephone, address) values ("άμπελος","0090","turkey");'.
     % I = 'Insert into phones (name, telephone, address) values ("ozzy","0090","turkey");'.

close_db :-
     sqlite_disconnect( phones_db ),
     write( 'Closed database, for now.' ), nl, nl.

does_not_exist( Sfile ) :-
     exists_file( Sfile ),
     !,
     write( 'Delete file "' ), 
     write( Sfile ), 
     write( '" to proceed with this example.' ), nl,
     fail.
does_not_exist( _Sfile ).

del :- delete_file('phones.sqlite').
