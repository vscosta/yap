

## [The MYDDAS Data-base interface](myddas)

<!--- @ingroup packages --->
<!--- @{ --->
The MYDDAS database project was developed within a FCT project
aiming at the development of a highly efficient deductive database
system, based on the coupling of the MySQL relational database
system with the YAP Prolog system. MYDDAS was later expanded to
support the ODBC interface, postgres and sqlite3.

<!--- @} --->

### [MYDDAS Predicates](MYDDAS_ABI)
<!--- @ingroup myddas --->

[TOC]

<!--- @{ --->

@pred db view(+,+,+).
@pred db view(+,+).


If we import a database relation, such as an edge relation representing the edges of a directed graph, through

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_import('Edge',edge).
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqliand we then write a query to retrieve all the direct cycles in the
graph, such as

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- edge(A,B), edge(B,A).
A = 10,
B = 20 ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
this is clearly inefficient [3], because of relation-level
access. Relation-level access means that a separate SQL query will be
generated for every goal in the body of the clause. For the second
`edge/2` goal, a SQL query is generated using the variable bindings that
result from the first `edge/2` goal execution. If the second
`edge/2` goal
fails, or if alternative solutions are demanded, backtracking access the
next tuple for the first `edge/2` goal and another SQL query will be
generated for the second `edge/2` goal. The generation of this large
number of queries and the communication overhead with the database
system for each of them, makes the relation-level approach inefficient.
To solve this problem the view level interface can be used for the
definition of rules whose bodies includes only imported database
predicates.  One can use the view level interface through the predicates
db_view/3 and `db_view/2`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_view(Conn,PredName(Arg_1,...,Arg_n),DbGoal).
?- db_view(PredName(Arg_1,...,Arg_n),DbGoal).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All arguments are standard Prolog terms.  _Arg1_ through  _Argn_
define the attributes to be retrieved from the database, while
_DbGoal_ defines the selection restrictions and join
conditions.  _Conn_ is the connection identifier, which again can be
dropped. Calling predicate `PredName/n` will retrieve database
tuples using a single SQL query generated for the  _DbGoal_.  We next show
an example of a view definition for the direct cycles discussed
above. Assuming the declaration:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_import('Edge',edge).
yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
we
write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_view(direct_cycle(A,B),(edge(A,B), edge(B,A))).
yes
?- direct_cycle(A,B)).
A = 10,
B = 20 ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This call generates the SQL
statement:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SELECT A.attr1 , A.attr2
FROM Edge A , Edge B
WHERE B.attr1 = A.attr2 AND B.attr2 = A.attr1;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Backtracking, as in relational level interface, can be used to retrieve the next row of the view.
The view interface also supports aggregate function predicates such as
`sum`, `avg`, `count`, `min` and `max`. For
instance:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_view(count(X),(X is count(B, B^edge(10,B)))).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
generates the query :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SELECT COUNT(A.attr2)
FROM Edge A WHERE A.attr1 = 10;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To know how to use db `view/3`, please refer to Draxler's Prolog to
SQL Compiler Manual.

@pred db_sql(+,+,?).
@pred db_sql(+,?).



It is also possible to explicitly send a SQL query to the database server using

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_sql(Conn,SQL,List).
?- db_sql(SQL,List).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where  _SQL_ is an arbitrary SQL expression, and  _List_ is a list
holding the first tuple of result set returned by the server. The result
set can also be navigated through backtracking.

Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- db_sql('SELECT * FROM phonebook',LA).
LA = ['D','John Doe',123456789] ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



@pred db_assert(+,+).
@pred db_assert(+).




Assuming you have imported the related base table using
db_import/2 or db_import/3, you can insert to that table
by using db_assert/2 predicate any given fact.

~~~~
?- db_assert(Conn,Fact).
?- db_assert(Fact).
~~~~

The second argument must be declared with all of its arguments bound to
constants. For example assuming `helloWorld` is imported through
`db_import/2
This, would generate the following query

~~~~
INSERT INTO helloWorld
VALUES ('A','Ana',3)
~~~~

which would insert into the helloWorld, the following row:
`A,Ana,31`. If we want to insert `NULL`  values into the
relation, we call db_assert/2 with a uninstantiated variable in
the data base imported predicate. For example, the following query on
the YAP-prolog system:

~~~~
?- db_assert(helloWorld('A',NULL,31)).
yes
~~~~

Would insert the row: `A,null value,31` into the relation
`Hello World`, assuming that the second row allows null values.

@pred db insert(+,+,+).
@pred db insert(+,+).



This predicate would create a new database predicate, which will insert
any given tuple into the database.

~~~~
?- db_insert(Conn,RelationName,PredName).
?- db_insert(RelationName,PredName).
~~~~
This would create a new predicate with name  _PredName_, that will
insert tuples into the relation  _RelationName_. is the connection
identifier. For example, if we wanted to insert the new tuple
`('A',null,31)` into the relation `Hello World`, we do:

~~~~
?- db_insert('Hello World',helloWorldInsert).
yes
?- helloWorldInsert('A',NULL,31).
yes
~~~~

@pred db_get_attributes_types(+,+,?).
otype for this predicate is the following:

~~~~
?- db_get_attributes_types(Conn,RelationName,ListOfFields).
?- db_get_attributes_types(RelationName,ListOfFields).
~~~~

You can use the
predicate `db_get_attributes types/2` or db_get_attributes_types/3, to
know what are the names and attributes types of the fields of a given
relation. For example:

~~~~
?- db_get_attributes_types(myddas,'Hello World',LA).
LA = ['Number',integer,'Name',string,'Letter',string] ?
yes
~~~~
where <tt>Hello World</tt> is the name of the relation and <tt>myddas</tt> is the
connection identifier.

@pred db_number_of_fields(+,?).
@pred db_number_of_fields(+,+,?).



The prototype for this
predicate is the following:

~~~~
?- db_number_of_fields(Conn,RelationName,Arity).
?- db_number_of_fields(RelationName,Arity).
~~~~
You can use the predicate db_number_of_fields/2 or
`db_number_of_fields/3` to know what is the arity of a given
relation. Example:

~~~~
?- db_number_of_fields(myddas,'Hello World',Arity).
Arity = 3 ?
yes
~~~~
where `Hello World` is the name of the
relation and `myddas` is the connection identifier.

@pred db_datalog_describe(+,+).
@pred db_datalog_describe(+).



The db `datalog_describe/2` predicate does not really returns any
value. It simply prints to the screen the result of the MySQL describe
command, the same way as `DESCRIBE` in the MySQL prompt would.

~~~~
?- db_datalog_describe(myddas,'Hello World').
+----------+----------+------+-----+---------+-------+
|   Field  |  Type    | Null | Key | Default | Extra |
+----------+----------+------+-----+---------+-------+
+  Number  | int(11)  | YES  |     |   NULL  |       |
+  Name    | char(10) | YES  |     |   NULL  |       |
+  Letter  | char(1)  | YES  |     |   NULL  |       |
+----------+----------+------+-----+---------+-------+
yes
~~~~

@pred db_describe(+,+).



@pred db_describe(+)

The `db_describe/3` predicate does the same action as
db_datalog_describe/2 predicate but with one major
difference. The results are returned by backtracking. For example, the
last query:

~~~~
?- db_describe(myddas,'Hello World',Term).
Term = tableInfo('Number',int(11),'YES','',null(0),'') ? ;
Term = tableInfo('Name',char(10),'YES','',null(1),'' ? ;
Term = tableInfo('Letter',char(1),'YES','',null(2),'') ? ;
no
~~~~


@pred db_datalog_show_tables(+).
@pred db_datalog_show_tables


If we need to know what relations exists in a given MySQL Schema, we can use
the `db_datalog_show_tables/1` predicate. As <tt>db_datalog_describe/2</tt>,
it does not returns any value, but instead prints to the screen the result of the
`SHOW TABLES` command, the same way as it would be in the MySQL prompt.

~~~~
?- db_datalog_show_tables(myddas).
+-----------------+
| Tables_in_guest |
+-----------------+
|   Hello World   |
+-----------------+
yes
~~~~
@pred db_show_tables(+, ?).



@pred db_show_tables(?)




The db_show_tables/2 predicate does the same action as
`db_show_tables/1` predicate but with one major difference. The
results are returned by backtracking. For example, given the last query:

~~~~
?- db_show_tables(myddas,Table).
Table = table('Hello World') ? ;
no
~~~~




@pred db_top_level(+,+,+,+,+).
@pred db_top_level(+,+,+,+).




Through MYDDAS is also possible to access the MySQL Database Server, in
the same wthe mysql client. In this mode, is possible to query the
SQL server by just using the standard SQL language. This mode is exactly the same as
different from the standard mysql client. We can use this
mode, by invoking the db top level/5. as one of the following:

~~~~
?- db_top_level(mysql,Connection,Host/Database,User,Password).
?- db_top_level(mysql,Connection,Host/Database/Port,User,Password).
?- db_top_level(mysql,Connection,Host/Database/UnixSocket,User,Password).
?- db_top_level(mysql,Connection,Host/Database/Port/UnixSocket,User,Password).
~~~~

Usage is similar as the one described for the db_open/5 predicate
discussed above. If the login is successful, automatically the prompt of
the mysql client will be used.  For example:

~~~~
?- db_top_level(mysql,con1,localhost/guest_db,guest,'').
~~~~
opens a
connection identified by the `con1` atom, to an instance of a MySQL server
running on host `localhost`, using database guest `db` and user `guest` with
empty password. After this is possible to use MYDDAS as the mysql
client.

~~~~
?- db_top_level(mysql,con1,localhost/guest_db,guest,'').
Reading table information for completion of table and column names
You can turn off this feature to get a quicker startup with -A

Welcome to the MySQL monitor.
Commands end with ; or \g.

Your MySQL connection id is 4468 to server version: 4.0.20
Type 'help;' or '\h' for help.
Type '\c' to clear the buffer.
mysql> exit
Bye
yes
?-
~~~~

@pred db_verbose(+).


When we ask a question to YAP, using a predicate asserted by
db_import/3, or by db_view/3, this will generate a SQL
`QUERY`. If we want to see that query, we must to this at a given
point in our session on YAP.

~~~~
?- db_verbose(1).
yes
?-
~~~~
If we want to
disable this feature, we must call the `db_verbose/1` predicate with the value 0.

@pred db_top_level(+,+,+,+).

@pred db_module(?).





When we create a new database predicate, by using db_import/3,
db_view/3 or db_insert/3, that predicate will be asserted
by default on the `user` module. If we want to change this value, we can
use the db_module/1 predicate to do so.

~~~~
?- db_module(lists).
yes
?-
~~~~
By executing this predicate, all of the predicates asserted by the
predicates enumerated earlier will created in the lists module.
If we want to put back the value on default, we can manually put the
value user. Example:

~~~~
?- db_module(user).
yes
?-
~~~~

We can also see in what module the predicates are being asserted by doing:

~~~~
?- db_module(X).
X=user
yes
?-
~~~~

@pred db_my_result_set(?).

The MySQL C API permits two modes for transferring the data generated by
a query to the client, in our case YAP. The first mode, and the default
mod-MySQL, is to store the result. This mode copies all the
information generated to the client side.

~~~~
?- db_my_result_set(X).
X=store_result
yes
~~~~

The other mode that we can use is use result. This one uses the result
set created directly from the server. If we want to use this mode, he
simply do

~~~~
?- db_my_result_set(use_result).
yes
~~~~
After this command, all
of the database predicates will use use result by default. We can change
this by doing again `db_my_result_set(store_result)`.

@pred db_my_sql_mode(+Conn,?SQL_Mode).
@pred db_my_sql_mode(?SQL_Mode).




The MySQL server allows the user to change the SQL mode. This can be
very useful for debugging proposes. For example, if we want MySQL server
not to ignore the INSERT statement warnings and instead of taking
action, report an error, we could use the following SQL mode.

~~~~
?-db_my_sql_mode(traditional). yes
~~~~
You can see the available SQL Modes at the MySQL homepage at
<http://www.mysql.org>.

<!--- @} --->
### [Requirements and Installation Guide        ](MYDDAS_INSTall)
<!--- @ingroup myddas --->
<!--- @{ --->

Next, we describe how to use YAP plus the MYDDAS System.  MYDDAS
includes its own copy of SQLite3, a popular DBMS that is highky
portable. It can also them interface to the MySQL development
libraries, Postgres or the ODBC development libraries. At least one
of the this development libraries must be installed on the computer
system, otherwise MYDDAS will not compile. The MySQL development
libraries from MySQL 3.23 an above are know to work. We recommend
the usage of MySQL versus ODBC, but it is possible to have both
options installed

At the same time, without any problem. The MYDDAS system automatically
controls the two options. Currently, MYDDAS is know to compile without
problems in Linux. The usage of this system on Windows has not been
tested yet.  MYDDAS must be enabled at configure time. This can be done
with the following options:

   + `--enable-myddas`

This option will detect which development libraries are installed on the computer system, MySQL, ODBC or both, and will compile the Yap system with the support for which libraries it detects;

    + `--enable-myddas-stats`

This option is only available in MySQL. It includes code to get
statistics from the MYDDAS system;


    + `--enable-top-level`

This option is only available in MySQL.  It enables the option to interact with the MySQL server in
two different ways. As if we were on the MySQL Client Shell, and as if
we were using Datalog.

<!--- @} --->

### [MYDDAS Architecture](myddasarch)
<!--- @ingroup myddas --->

<!--- @{ --->

The system includes four main blocks that are put together through the
MYDDAS interface: the Yap Prolog compiler, the MySQL database system, an
ODBC level and a Prolog to SQL compiler. Current effort is put on the
MySQL interface rather than on the ODBC interface. If you want to use
the full power of the MYDDAS interface we recommend you to use a MySQL
database. Other databases, such as Oracle, PostGres or Microsoft SQL
Server, can be interfaced through the ODBC layer, but with limited
performance and features support.

The main structure of the MYDDAS interface is simple. Prolog queries
involving database goals are translated to SQL using the Prolog to SQL
compiler; then the SQL expression is sent to the database system, which
returns the set of tuples satisfying the query; and finally those tuples
are made available to the Prolog engine as terms. For recursive queries
involving database goals, the YapTab tabling engine provides the
necessary support for an efficient evaluation of such queries.

An important aspect of the MYDDAS interface is that for the programmer
the use of predicates which are defined in database relations is
completely transparent. An example of this transparent support is the
Prolog cut operator, which has exactly the same behaviour from
predicates defined in the Prolog program source code, or from predicates
defined in database as relations.

~~~~
Name = 'John Doe',
Number = 123456789 ?
yes
~~~~
Backtracking can then be used to retrieve the next row
of the relation phonebook.  Records with particular field values may be
selected in the same way as in Prolog. (In particular, no mode
specification for database predicates is required). For instance:

~~~~
?- phonebook(Letter,'John Doe',Letter).
Letter = 'D',
Number = 123456789 ?
yes
~~~~
generates the query

~~~~
SELECT A.Letter , 'John Doe' , A.Number
FROM 'phonebook' A
WHERE A.Name = 'John Doe';
~~~~

<!--- @} --->
