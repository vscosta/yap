/*
  persistence.yap - make assertions and retracts persistent

  Copyright (C) 2006, Christian Thaeter <chth@gmx.net>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License version 2 as
  published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, contact me.

*/

:- module(persistence,
	[
		persistent_open/3,
		persistent_close/1,
		persistent_assert/1,
		persistent_retract/1
	]).

:- use_module(library(system)).

:- dynamic(persistent_desc/2).

/**  persistent_open(PredDesc, File, Opts).
  
  declare Module:Functor/Arity (Functor/Arity) to be persistent
  stored in File's (*.db *.log *log.$PID *.lock *.bak)
  
  Opts are:
    db     - use dbfile (flat file containing all persistent predicates)
    log    - use logfile (logfile with either +(Term) for asserts and -(Term) for retracts)
    bak    - make backupfiles when regenerating the dbfile
    sync   - flush data always
    ro     - readonly, can load locked files, never changes data on disk
    wo     - (planned) writeonly, implies [log], data is only written to the log and not
             asserted into prolog, the database will not be loaded at persistent_open.
    conc   - (planned) concurrency, extends the locking for multiple readers/single writer locks
    trans  - (planned) support for transactions (begin/commit/abort)

  Guides:
  - if the data mutates a lot, use [db,log].
  - if you mostly append data [log] suffices.
  - if the data is not important (can be regenerated) and mostly readonly then [db] is ok.
  - when using only [db] you must not forget to persistent_close!
  - for extra security against failures add [bak,sync].
  - don't use [bak] if you need to conserve disk space and the database is huge.
  - don't use [sync] if you need very fast writes.
  - turning all on [db,log,bak,sync] is probably the best, if you are undecided.
  - [ro,db] loads only the last saved db file.
  - [ro,log] loads the last saved db file if it exists and replays the log.
  - note that [ro] will fail if the db is not intact (.bak file present).

  (planned features)
  - [wo] is very limited and only useful if you want to log data to a file
  - [wo,db] will replay the log at close
  - [conc] is useful for shareing data between prolog processes, but this is not a
    high performance solution.
  - [trans] can improve performance of concurrent access somewhat
*/
persistent_open(PredDesc, File, Opts) :-
	module_goal(PredDesc, Module:Functor/Arity),
	atom(Functor), integer(Arity), atom(File),
	\+ persistent_desc(Module:Functor/Arity,_),

	atom_concat(File,'.db',DBfile),
	assertz(persistent_desc(Module:Functor/Arity,dbfile(DBfile))),

	atom_concat(File,'.bak',Backupfile),
	assertz(persistent_desc(Module:Functor/Arity,backupfile(Backupfile))),

        atom_concat(File,'.log',Logfile),
	assertz(persistent_desc(Module:Functor/Arity,logfile(Logfile))),

        system:pid(Pid),
	assertz(persistent_desc(Module:Functor/Arity,pid(Pid))),

        number_atom(Pid,P),
        atom_concat(Logfile,P,Mylogfile),
	assertz(persistent_desc(Module:Functor/Arity,mylogfile(Mylogfile))),

        atom_concat(File,'.lock',Lockfile),
	assertz(persistent_desc(Module:Functor/Arity,lockfile(Lockfile))),

        persistent_opts_store(Module:Functor/Arity,Opts),
	persistent_load(Module:Functor/Arity),

        (       \+ persistent_desc(Module:Functor/Arity, ro), persistent_desc(Module:Functor/Arity, log)
        ->      open(Logfile, append, Log),
                assertz(persistent_desc(Module:Functor/Arity,logstream(Log)))
        ;       true
        ).

/*
  closes the database associated with PredDesc ([Module:]Functor/Arity)
*/
persistent_close(PredDesc0) :-
	module_goal(PredDesc0,PredDesc),
        (       persistent_desc(PredDesc, logstream(Log))
        ->      close(Log)
        ;       true
        ),
        persistent_save(PredDesc),
        persistent_desc(PredDesc, backupfile(Backupfile)),
        (system:delete_file(Backupfile,[ignore]); true),
        persistent_lock_release(PredDesc),
        retractall(persistent_desc(PredDesc,_)).

/*
  assert data to the database, this is always an assertz, if you need some ordering,
  then store some kind of key within your data.
  rules can be asserted too
*/
persistent_assert(Term) :-
        Term = (Head0 :- Body),
	module_goal(Head0, Module:Head),
        functor(Head, Functor, Arity),
	once(persistent_desc(Module:Functor/Arity,_)),!,
        (       persistent_desc(Module:Functor/Arity, logstream(Log))
        ->      writeq(Log,+(((Module:Head):-Body))), write(Log,'.\n'),
                (       persistent_desc(Module:Functor/Arity, sync)
                ->      flush_output(Log)
                ;       true
                )
        ;       true
        ),
        assertz((Module:Head:-Body)).
persistent_assert(Term0) :-
	module_goal(Term0, Module:Term),
        functor(Term,Functor,Arity),
	once(persistent_desc(Module:Functor/Arity,_)),!,
        (       persistent_desc(Module:Functor/Arity,logstream(Log))
        ->      writeq(Log,+(Module:Term)), write(Log,'.\n'),
                (       persistent_desc(Module:Functor/Arity, sync)
                ->      flush_output(Log)
                ;       true
                )
        ;       true
        ),
        assertz(Module:Term).

/*
  retract a persistent Term
*/
persistent_retract(Term0) :-
	module_goal(Term0, Module:Term),
        functor(Term,Functor,Arity),
        once(persistent_desc(Module:Functor/Arity,_)),!,
        retract(Module:Term),
        (       persistent_desc(Module:Functor/Arity, logstream(Log))
        ->      writeq(Log,-(Module:Term)), write(Log,'.\n'),
                (       persistent_desc(Module:Functor/Arity, sync)
                ->      flush_output(Log)
                ;       true
                )
        ;       true
        ).

% transaction support for future
persistent_begin.
persistent_commit.
persistent_abort.


/*

  PRIVATE PREDICATES, DONT USE THESE

*/

% save all data to a .db file
persistent_save(PredDesc) :-
        \+  persistent_desc(PredDesc,ro),
	(       persistent_desc(PredDesc,db)
	->	persistent_desc(PredDesc,dbfile(DBfile)),
		(
                        persistent_desc(PredDesc,bak)
                ->      persistent_desc(PredDesc,backupfile(Backupfile)),
                        (       system:file_exists(DBfile)
                        ->      system:rename_file(DBfile,Backupfile)
                        ;       true
                        )
                ;       true
                ),
                open(DBfile, write, S),
                persistent_writeall(PredDesc,S),
                close(S),
                persistent_desc(PredDesc,logfile(Logfile)),
                (system:delete_file(Logfile,[ignore]); true)
        ;       true
        ).

% write all predicates matching Functor/Arity to stream S
persistent_writeall(PredDesc, S) :-
	module_goal(PredDesc, Module:Functor/Arity),
        functor(Clause, Functor, Arity),
        clause(Module:Clause, Body),
        (       Body = true
        ->      writeq(S,Module:Clause)
        ;       writeq(S,(Module:Clause:-Body))
        ),
        write(S,'.\n'),
        fail.
persistent_writeall(_,_).

% load a database, recover logfile, recreate .db
persistent_load(PredDesc) :-
	persistent_desc(PredDesc,dbfile(DBfile)),
	persistent_desc(PredDesc,backupfile(Backupfile)),
	persistent_desc(PredDesc,logfile(Logfile)),

        (       persistent_desc(PredDesc,ro)
        ->      \+ system:file_exists(Backupfile),
                (       system:file_exists(DBfile)
                ->      persistent_load_file(DBfile)
                ;       true
                ),
                (       persistent_desc(PredDesc,log), system:file_exists(Logfile)
                ->      persistent_load_file(Logfile)
                ;       true
                )
        ;
                persistent_lock_exclusive(PredDesc),
                (       system:file_exists(Backupfile)
                ->      system:rename_file(Backupfile, DBfile)
                ;       true
                ),
                (       system:file_exists(DBfile)
                ->      persistent_load_file(DBfile)
                ;       true
                ),
                (       system:file_exists(Logfile)
                ->      persistent_load_file(Logfile),
                        (       persistent_desc(PredDesc, db)
                        ->      persistent_save(PredDesc)
                        ;       true
                        )
                ;       true
                )
        ).

% load a .db file or replay a .log file
persistent_load_file(File) :-
        open(File, read, S),
        repeat,
        read(S, TermIn),
        (
                TermIn == end_of_file,
                close(S),
                !
        ;
                (
                        TermIn = +(Term),
                        assertz(Term)
                ;
                        TermIn = -(Term),
                        retract(Term)
                ;
                        assertz(TermIn)
                ),
                fail
        ).

%lock handling, so far only exclusive locks
persistent_lock_exclusive(PredDesc) :-
	persistent_desc(PredDesc,lockfile(Lockfile)),
	persistent_desc(PredDesc,pid(Pid)),
        open(Lockfile, append, Lockappend),
        write(Lockappend,lock(write,Pid)),write(Lockappend,'.\n'),
        close(Lockappend),
        open(Lockfile, read, Lockread),
        read(Lockread,LPid),
        close(Lockread),
        LPid = lock(_,Pid).

% recover lock
persistent_lock_exclusive(PredDesc) :-
	persistent_desc(PredDesc, lockfile(Lockfile)),
        open(Lockfile, read, Lockread),
        read(Lockread,lock(_,LPid)),
        close(Lockread),
        \+ catch(kill(LPid,0),_,fail),
        (system:delete_file(Lockfile,[ignore]); true),
        %system:sleep(1),
        persistent_lock_exclusive(PredDesc).

persistent_lock_release(PredDesc) :-
	persistent_lock_exclusive(PredDesc),
	persistent_desc(PredDesc,lockfile(Lockfile)),
        (system:delete_file(Lockfile,[ignore]); true).


persistent_opts_store(_,[]).
persistent_opts_store(PredDesc,[H|T]) :-
	assertz(persistent_desc(PredDesc,H)),
	persistent_opts_store(PredDesc,T).

module_goal(Module:Goal,Module:Goal) :-
	callable(Goal), nonvar(Module),!.
module_goal(Goal,Module:Goal) :-
	callable(Goal), prolog_flag(typein_module,Module).
