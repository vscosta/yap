/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2018, VU University Amsterdam
                              CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(jpl_config,
          [ jpl_config_dylib/0
          ]).

:- use_module(library(process)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% :- debug(dylib(jpl)).

%!  jpl_config_dylib
%
%   Update the JVM dependencies of  libjpl.dylib   to  point at the user
%   preferred Java installation. The  preferred   Java  installation  is
%   found by first inspecting  =JAVA_HOME=  or,   if  this  is  not set,
%   running =/usr/libexec/java_home=.

jpl_config_dylib :-
    current_prolog_flag(apple, true),
    !,
    jpl_config_dylib(['libjvm.dylib', 'libjsig.dylib', 'libjawt.dylib']).
jpl_config_dylib :-
    print_message(warning, jpl_config(apple_only)).

jpl_config_dylib(Libs) :-
    java_home(JavaHome),
    debug(dylib(jpl), 'Java home at ~p', [JavaHome]),
    dylib_jpl(Dylib),
    debug(dylib(jpl), 'JPL at ~p', [Dylib]),
    dylib_dependencies(Dylib, Dependencies),
    maplist(update_dylib(JavaHome, Dylib, Dependencies), Libs).

update_dylib(JavaHome, Dylib, Dependencies, Lib) :-
    member(Lib0, Dependencies),
    file_base_name(Lib0, Lib),
    !,
    debug(dylib(jpl), 'Points at ~p', [Lib0]),
    (   sub_atom(Lib0, 0, _, _, JavaHome)
    ->  debug(dylib(jpl), 'In Java home.  OK', [])
    ;   jni_dylib_dir(JDylibDir),
        atomic_list_concat([ JavaHome, JDylibDir, Lib], '/', NewLib),
        exists_file(NewLib)
    ->  debug(dylib(jpl), '~w at ~p', [Lib, NewLib]),
        process_create('/usr/bin/install_name_tool',
                       [ '-change', Lib0, NewLib, Dylib], []),
        print_message(informational, jpl_config(updated(Dylib, NewLib)))
    ;   print_message(error, jpl_config(not_found(Dylib, Lib))),
        fail
    ).
update_dylib(_, _, _, Lib) :-
    debug(dylib(jpl), 'Skipping ~p as this is not a dependency', [Lib]).


jni_dylib_dir('jre/lib/server').
jni_dylib_dir('lib/server').                    % Java 8
jni_dylib_dir('lib').                           % Java 10 libjawt.dylib

java_home(Dir) :-
    getenv('JAVA_HOME', Dir),
    exists_directory(Dir),
    !.
java_home(Dir) :-
    setup_call_cleanup(
        process_create('/usr/libexec/java_home', [],
                       [ stdout(pipe(Out)) ]),
        read_string(Out, _, String),
        close(Out)),
    split_string(String, "\n", "", [HomeString|_]),
    exists_directory(HomeString),
    atom_string(Dir, HomeString).

dylib_dependencies(File, Dependencies) :-
    setup_call_cleanup(
        process_create('/usr/bin/otool', ['-L', File],
                       [ stdout(pipe(Out)) ]),
        read_string(Out, _, String),
        close(Out)),
    split_string(String, "\n", " \t", [_Header|Deps]),
    convlist(dependency, Deps, Dependencies).

dependency(Line, File) :-
    split_string(Line, "(", " \t", [FileString, _|_]),
    atom_string(File, FileString).

dylib_jpl(Dylib) :-
    absolute_file_name(foreign(libjpl), Dylib,
                       [ file_type(executable),
                         access(read)
                       ]).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(jpl_config(Action)) -->
    message(Action).

message(apple_only) -->
    [ 'jpl_config_dylib/0 only works on MacOS'-[] ].
message(updated(Dylib, NewLib)) -->
    { file_base_name(Dylib, DylibBase),
      file_base_name(NewLib, Dep)
    },
    [ '~p: Updated ~p to ~p'-[DylibBase, Dep, NewLib] ].
message(not_found(Dylib, Lib)) -->
    { file_base_name(Dylib, DylibBase)
    },
    [ '~p: Could not find dependency ~p'-[DylibBase, Lib] ].
