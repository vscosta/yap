:- module( uniprot, [uniprot/0] ).


:- nl, nl, nl.
:- write( 'To get the example database used in this examples do:' ), nl.
:- write( 'wget http://stoics.org.uk/~nicos/sware/sqlite/uniprot.sqlite' ), nl.
:- nl, nl.

:- use_module( library(prosqlite) ).

/** <module>  uniprot: A complete example for proSQLite.

Look at the sources for the difference predicate calls.

You need the test database from 
               http://stoics.org.uk/~nicos/sware/sqlite/uniprot.sqlite

The output should look like:

==
άμπελος;lib/db% swipl -f none

?- uniprot.
Running on :date(2012,10,17)
Using database at: uniprot.sqlite

table:secondary_accessions
table:identifier_mapping

secondary_accessions/secondary_accession
secondary_accessions/primary_accession
identifier_mapping/uniprot_accession
identifier_mapping/identifier_type
identifier_mapping/target_identifier

secondary_accessions:286525
identifier_mapping:3044651

secondary_accessions+286525
identifier_mapping+3044651

[A0A111,Q10706]-_G309
rows_for(_G309):[row(A0A111),row(Q10706)]

by_findall(P64943,[A0A111,Q10706])
Caution! Deleting db entries for-P64943
Affected rows:row(2)
now(P64943,[])

Not to worry! Adding back the db entries for-P64943
affected_two(row(1),row(1))
finally(P64943,[A0A111,Q10706])

v:0:1:0
vd:date(2012,10,17)

citation
Exploring file based databases via an Sqlite interface. 
 Canisius Sander, Nicos Angelopoulos and Lodewyk Wessels 
 In the ICLP Workshop on Logic-based methods in Programming Environments (WLPE'12),
 p.2-9, 2012. Budapest, Hungary.
true.

?-  


==
     @version 0.1.2, 2013/11/1 
     @see latest version at http://stoics.org.uk/~nicos/sware/prosqlite/uniprot.pl
     @see also http://stoics.org.uk/~nicos/sware/sqlite/uniprot.sqlite (184Mb)

*/


/** uniprot.

Test predicate for prosqlite. Tests all components.

You need the test database from 
               http://stoics.org.uk/~nicos/sware/sqlite/uniprot.sqlite


*/

uniprot :-
     date( Date ), 
     write( 'Running on ':Date ), nl,
     sqlite_connect('uniprot.sqlite',uniprot, as_predicates(true) ),  % connect the database
     nl,
     show_tables,
     show_columns,
     show_counts,
     findall_counts,
     Id = 'P64943',
     findall( A, secondary_accessions(A,Id), As ),
     write( As-Id ), nl,
     Q = 'Select secondary_accession From secondary_accessions Where primary_accession="~w"',
     findall( Row, sqlite_format_query( uniprot, Q-'P64943', Row ), Rows ),
     write(rows_for(Id):Rows), nl, nl,
     findall( S, secondary_accessions(S,Id), SetBef ),
     write( by_findall(Id,SetBef) ), nl,
     write( 'Caution! Deleting db entries for'-Id ), nl,
     sqlite_query( uniprot, 'Delete from secondary_accessions where primary_accession="P64943";', Aff ),
     write( 'Affected rows':Aff ), nl,
     findall( S, secondary_accessions(S,Id), SetAft ),
     write( now(Id,SetAft) ), nl, nl,
     write( 'Not to worry! Adding back the db entries for'-Id ), nl,
     sqlite_query( uniprot, 'Insert into secondary_accessions (primary_accession,secondary_accession) Values ("P64943","A0A111");', Aff1 ),
     sqlite_query( uniprot, 'Insert into secondary_accessions (secondary_accession,primary_accession) Values ("Q10706","P64943");', Aff2 ),
     write( affected_two( Aff1, Aff2 ) ), nl,
     findall( S, secondary_accessions(S,Id), SetFin ),
     write( finally(Id,SetFin) ), nl, nl,
     sqlite_version( Ver, VerDate ),
     write( v:Ver ), nl,
     write( vd:VerDate ), nl, nl,
     sqlite_citation( AtmCite, _B ),
     write( citation ), nl,
     write( AtmCite ), nl.


show_tables :-
     sqlite_current_table( uniprot, Table ),
     write( table:Table ), nl, 
     fail.
show_tables :-
     nl.

show_columns :-
     sqlite_table_column( uniprot, Table, Col ),
     write( Table/Col ), nl, 
     fail.
show_columns :-
     nl.

show_counts :-
     sqlite_current_table(uniprot, Table),
     sqlite_table_count(uniprot, Table, Count),
     write(Table:Count), nl, 
     fail.
show_counts :-
     nl.

findall_counts :-
     sqlite_current_table(uniprot, Table, arity(Arity) ),
     length(List, Arity),
     Pred =.. [Table|List],
     findall(1, Pred, Ones),
     length(Ones, Count),
     write( Table+Count ), nl, 
     fail.
findall_counts :-
     nl.
