
proSQLite: a Prolog interface to the SQLite database system.
--------------------


This Prolog library was developed on SWI-Prolog 6.1.12 under linux.
It should compile easily on linux or MACs. It is also likely
that it compiles easily for YAP Prolog.

On SWI-Prolog is higly recommended that you install from with the engine
via 

   ?- pack_install( prosqlite ).

This takes care of everything and you can then load the library via

   ?- [library(prosqlite)].

If you need to compile from sources, edit buildenv.sh to fit your system
and in a bourne-compatible shell do : 

$ source env/buildenv.sh
$ make

The publication corresponding to this library is :
   
   Exploring file based databases via an Sqlite interface
   Canisius Sander, Nicos Angelopoulos and Lodewyk Wessels
   ICLP Workshop on Logic-based methods in Programming Environments (September, 2012. Budapest, Hungary).


---------



Nicos Angelopoulos, November 2013. (0.1.0 @ August 2012)
---
http://stoics.org.uk/~nicos/
nicos_mlists_06@yahoo.co.uk
