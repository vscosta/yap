
:- use_module(library(python)).

:- use_module(library(lists)).

:- python_import('MySQLdb').

:- initialization(main).

main :- 
	python_import(pyx),
	ex(X),
	flush_output,
	fail.
main.

ex(open) :-
	connect,
% execute SQL query using execute() method.
        := $cursor:execute('SELECT VERSION()'),
% Fetch a single row using fetchone() method.
        Data := cursor:fetchone(_),
	Data = t(Version),
	format( "Database version : ~a~n ", [ Version ]),
	close.

ex(create) :-
	connect,
% Drop table if it already exist using execute() method.
	:= $cursor:execute('DROP TABLE IF EXISTS EMPLOYEE'),
        % Create table as per requirement
	sql := 'CREATE TABLE EMPLOYEE (
         FIRST_NAME  CHAR(20) NOT NULL,
         LAST_NAME  CHAR(20),
         AGE INT,  
         SEX CHAR(1),
         INCOME FLOAT ) DEFAULT charset=utf8',
	:= $cursor:execute($sql),
	close.

ex(insert) :-
	connect,
% Prepare SQL query to INSERT a record into the database.
	catch(add, _, rollback), 
   	close.

add :-
	customer(First, Last, Age, Sex, Income),
	format(atom(Sql), 'INSERT INTO EMPLOYEE(FIRST_NAME,
         LAST_NAME, AGE, SEX, INCOME)
         VALUES (\'~a\', \'~a\', ~d, \'~a\', ~g)',
	 [ First, Last, Age, Sex, Income ]),
	% Execute the SQL command
	:= $cursor:execute(Sql),
	fail.
add :- 
	% Commit your changes in the database
        := $db:commit(_).

rollback :-
	:= db:rollback(_).

connect :-
	db := connect('localhost','testuser','test123','TESTDB' ),
	:= $db:set_character_set('utf8'),
% prepare a cursor object using cursor() method
        cursor := db:cursor(_),
	:= $cursor:execute('SET NAMES utf8;'),
	:= $cursor:execute('SET CHARACTER SET utf8;'),
	:= $cursor:execute('SET character_set_connection=utf8;').

close :-
% disconnect from server
	:= db:close(_).

% Open database connection
ex(read) :-
	connect,
% Prepare SQL query to SELECT a record from the database.
	sql := 'SELECT * FROM EMPLOYEE \
               WHERE INCOME > 1000',
	catch(try, _, except),
	close.

try:-
   % Execute the SQL command
	:= $cursor:execute($sql),
   % Fetch all the rows in a list of lists.
	Results := $cursor:fetchall(_),
	Results =.. [_|LResults],
	member(t(Fname, Lname, Age, Sex, Income), LResults),
      % Now print fetched result
	format("fname=~a, lname=~a, age=~d, sex=~a, income=~g~n",
    [Fname, Lname, Age, Sex, Income ]),
	fail.
try.

except:-
   format('Error: unable to fecth data', []).

% disconnect from server

customer('João', 'Matos', 40, 'M', 2000).
customer('Maria', 'Söderling', 20, 'F', 3000).
customer('毛', '泽东', 44, 'M', 500).
customer('রবীন্দ্রনাথ', 'ঠাকুর', 30, 'M', 8000).
