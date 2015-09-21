:- use_module( library(prosqlite) ).

/** simple.

==
?- simple.


Using database at: simple.sqlite
create_res(row(0))
i1(Insert into cited_by (pubmed_id,ret_date,citer) values (123, "2012/10/6", 321);)
insert1_res(row(1))
i2(Insert into cited_by (pubmed_id,ret_date,citer) values (120, "2012/10/6", 321);)
insert2_res(row(1))
sel(Select * from cited_by)
sel:row(123,2012/10/6,321)
sel:row(120,2012/10/6,321)
d(Delete From cited_by Where pubmed_id = 120;)
del_row(row(1))
sel:row(123,2012/10/6,321)
true.


?- 
==
*/

:- multifile user:message_poperty/2.
message_property(debug(sqlite), color([fg(yellow)])).

simple :-
     write( deleting_file('simple.sqlite') ), nl,
     catch( delete_file('simple.sqlite'), _, true ),
     debug( sqlite ),
     sqlite_connect( simple, simple, exists(false) ),
     C = 'CREATE TABLE cited_by (pubmed_id bigint(20), ret_date date, citer bigint(20), Primary Key (pubmed_id,citer) );',
     sqlite_query( simple, C, Row ),
     write( create_res(Row) ), nl,
     I1 = 'Insert into cited_by (pubmed_id,ret_date,citer) values (123, "2012/10/6", 321);',
     write( i1(I1) ), nl,
     sqlite_query( simple, I1, RowI1 ),
     write( insert1_res(RowI1) ), nl,
     Date = date(2012,10,06),
     Pfx2 = 'Insert into cited_by (pubmed_id,ret_date,citer) values (120, ',
     sqlite_date_sql_atom( Date, SDate ),
     atomic_list_concat( [Pfx2,SDate,', 321);'], I2 ),
     write( i2(I2) ), nl,
     sqlite_query( simple, I2, RowI2 ),
     write( insert2_res(RowI2) ), nl,
     Sel = 'Select * from cited_by',
     write( sel(Sel) ), nl,
     findall( _, (sqlite_query(simple,Sel,FRow),write(sel:FRow),nl), _ ),
     D = 'Delete From cited_by Where pubmed_id = 120;',
     write( d(D) ), nl,
     sqlite_query( simple, D, DRow ),
     write( del_row(DRow) ), nl,
     findall( _, (sqlite_query(simple,Sel,FRow),write(sel:FRow),nl), _ ),
     sqlite_disconnect( simple ).

