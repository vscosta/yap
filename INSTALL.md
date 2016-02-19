

Installing YAP           {#install}
==============

YAP=6.3.4 is a [`cmake`]() based system. We discuss how to use `cmake`
to install YAP, and what are the major options.

Compiling YAP {#CompilingYAP}
-------------

To compile YAP it should be sufficient to:

2 create a directory, say `Build` and `cd` to the directory (`cd Build`).

  obs: avoid compiling YAP in the src directory, some packages do not allow for that.

1 run `cmake`, ideally using a cmake above 3.0.

2 `make`.

3 If the compilation succeeds, try `./yap`.

4  If you feel satisfied with the result, do `make install`.

5 In most systems you will need to be superuser in order to do
    `make install` and `make info` on the standard directories.

Tuning the Functionality of YAP
-------------------------------

By default, YAP supports tabling, depth first search, and most features found in
modern Prologs. In some cases, you may want to suport extra features, or reduce system size.
`cmake`provides an graphical interface for doing so. From the commmand line,  a typical example could be:

~~~~~
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=~ ~/src/yap
~~~~~

The first argument says that this is a release, compiled with full optimisation. The second argument says YAP should install under the ~ drectory. In this case, YAP will add the binaries to ~/bin, include files to `~/include/Yap`, Prolog files to `~/share/Yap`, and ay DLL to `~/lib/Yap`.

Options include:

+ `CMAKE_BUILD_TYPE`: the two main options are `Debug`, for system development, and Release. to use the system.

+ `CMAKE_INSTALL_PREFIX`: where to install YAP, by default `/usr/local`.

+ WITH_DEPTH_LIMIT allows depth limited evaluation, say for
implementing iterative deepening (default)

+ `WITH_CALL_TRACER` allows support for tracing all calls,
retries, and backtracks in the system. This can help in debugging your
application, but results in performance loss (enabled in Debug).

+ `WITH_WAM_PROFILER` allows profiling of abstract machine
instructions. This is useful when developing YAP, should not be so
useful for normal users (not currently supportted)

+ `WITH_YAP_CONDOR` allows using the Condor system that
support High Throughput Computing (HTC) on large collections of
distributively owned computing resources (not yet available)

+ `WITH_TABLING` allows tabling support (default)

+ `WITH_YAPOR_COPY` allows
or-parallelism according to the Muse-based, YapOR model. This option is
still highly experimental.

+ `WITH_GMP` give a path to where one can find the
`GMP` library if not installed in the default path. 
It is highly advised to have GMP in your compilation environment.

+ `WITH_READLINE` give a path to where one can find the
`readline` library if not installed in the default path. 
It is highly advised in Unix-like environments but not useful in Windows.

+ `-WITH_Threads` allows using of the multi-threading
predicates provided by YAP. 

You may also want to use a different compilation environment. As an example:

~~~~~
cmake -GXcode ..
~~~~~

will generate files for compilation of YAP within Apple's Xcode IDE.
You can also use `xcodebuild` from the command line.

