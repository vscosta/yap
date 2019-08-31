

@page run Running YAP

@section Running_YAP_Interactively Running YAP Interactively


Most often you will want to use YAP in interactive mode. Assuming that
YAP is in the user's search path, the top-level can be invoked under
Unix with the following command:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yap [-s n] [-h n] [-a n] [-c IP_HOST port ] [filename]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All the arguments and flags are optional and have the following meaning:

+ -?
print a short error message.
+ -s _Size_
allocate  _Size_ KBytes for local and global stacks. The user may
specify <tt>M</tt> bytes.
+ -h _Size_
allocate  _Size_ KBytes for heap and auxiliary stacks
+ -t _Size_
allocate  _Size_ KBytes for the trail stack
+ -L _Size_
SWI-compatible option to allocate  _Size_ K bytes for local and global stacks, the local stack
cannot be expanded. To avoid confusion with the load option,  _Size_
must immediately follow the letter `L`.
+ -G _Size_
SWI-compatible option to allocate  _Size_ K bytes for local and global stacks; the global
stack cannot be expanded
+ -T _Size_
SWI-compatible option to allocate  _Size_ K bytes for the trail stack; the trail cannot be expanded.
+ -l  _YAP_FILE_
compile the Prolog file  _YAP_FILE_ before entering the top-level.
+ -L  _YAP_FILE_
compile the Prolog file  _YAP_FILE_ and then halt. This option is
useful for implementing scripts.
+ -g  _Goal_
run the goal  _Goal_ before top-level. The goal is converted from
an atom to a Prolog term.
+ -z  _Goal_
run the goal  _Goal_ as top-level. The goal is converted from
an atom to a Prolog term.
+ -b  _BOOT_FILE_
boot code is in Prolog file  _BOOT_FILE_. The filename must define
the predicate `'$live'/0`.
+ -c <tt>IP_HOST</tt> <tt>port</tt>
connect standard streams to host <tt>IP_HOST</tt> at port <tt>port</tt>
+ filename
restore state saved in the given file
+ -f
do not consult initial files
+ -q
do not print informational messages
+ --
separator for arguments to Prolog code. These arguments are visible
through the unix/1 built-in predicate.


Note that YAP will output an error message on the following conditions:

+ a file name was given but the file does not exist or is not a saved
YAP state;

+ the necessary amount of memory could not be allocated;

+ the allocated memory is not enough to restore the state.

  When restoring a saved state, YAP will allocate the same amount of memory as that in use when the state was saved, unless a different amount is specified by flags in the command line. By default, YAP restores the file startup.yss from the current directory or from the YAP library.

+ YAP usually boots from a saved state. The saved state will use the default
installation directory to search for the YAP binary unless you define
the environment variable YAPBINDIR.

+ YAP always tries to find saved states from the current directory
	first. If it cannot it will use the environment variable YAPLIBDIR,
   if
	defined, or search the default library directory.

+ YAP will try to find library files from the YAPSHAREDIR/library directory.

### Running Prolog Files


YAP can also be used to run Prolog files as scripts, at least in
Unix-like environments. A simple example is shown next (do not forget
that the shell comments are very important):

~~~~
#!/usr/local/bin/yap -L --
#
# Hello World script file using YAP
#
# put a dot because of syntax errors .

:- write('Hello World'), nl.

~~~~

The `#!`  characters specify that the script should call the binary
file YAP. Notice that many systems will require the complete path to the
YAP binary. The `-L` flag indicates that YAP should consult the
current file when booting and then halt. The remaining arguments are
then passed to YAP. Note that YAP will skip the first lines if they
start with `#` (the comment sign for Unix's shell). YAP will
consult the file and execute any commands.

A slightly more sophisticated example is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#!/usr/bin/yap -L --
#
# Hello World script file using YAP
# .

:- initialization(main).

main :- write('Hello World'), nl.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `initialization` directive tells YAP to execute the goal main
after consulting the file. Source code is thus compiled and `main`
executed at the end. The `.` is useful while debugging the script
as a Prolog program: it guarantees that the syntax error will not
propagate to the Prolog code.

Notice that the `--` is required so that the shell passes the extra
arguments to YAP.  As an example, consider the following script
`dump_args`:

~~~~
#!/usr/bin/yap -L --
#.

main( [] ).
main( [H|T] ) :-
        write( H ), nl,
        main( T ).

:- unix( argv(AllArgs) ), main( AllArgs ).

~~~~

If you this run this script with the arguments:

~~~~
./dump_args -s 10000
~~~~
the script will start an YAP process with stack size `10MB`, and
the list of arguments to the process will be empty.

Often one wants to run the script as any other program, and for this it
is convenient to ignore arguments to YAP. This is possible by using
`L --` as in the next version of `dump_args`:

~~~~
#!/usr/bin/yap -L --

main( [] ).
main( [H|T] ) :-
        write( H ), nl,
        main( T ).

:- unix( argv(AllArgs) ), main( AllArgs ).

~~~~

The `--` indicates the next arguments are not for YAP. Instead,
they must be sent directly to the argv built-in. Hence, running

~~~~
./dump_args test
~~~~

will write `test` on the standard output.
