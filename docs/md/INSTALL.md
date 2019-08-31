
@page install Downloading and Installing YAP

### Downloading YAP

The latest development version of Yap-6 is available source-only
through GIT repositories. The main reference repository is at

+ development branch: [github](https://github.com/vscosta/yap-6.3)

We store an older version of YAP at:

+ snapshots: [sourceforge](http://sourceforge.net/p/yap/yap-6.3)

Please just use `git clone` to obtain the distribution. Ie, to download YAP from the command line please type:

~~~~~
git clone https://github.com/vscosta/yap-6.3 yap-6.3
~~~~~

The first argument is the repository, the last argument is the (optional) target directory.

 There are a variety of graphical interfaces to `git`, including GitHub's own [GitHub Desktop](https://desktop.github.com/) that supports Microsoft Windows and Apple OSX. A list with GUI applications, editor integration,  and much more can be found at the  [git Wiki](https://git.wiki.kernel.org/index.php/InterfacesFrontendsAndTools),

#### Download Options 
 <!--- $0 --->

It may be useful to know:

+ If you are have limited bandwith or disk spaceq, consider using
~~~~~
git clone --depth XX
~~~~~
to only include the last `XX` commits.

+ Older versions of YAP were distributed with modules. YAP-6.3.5 is
     a single package, and it does not need `git submodule`.

+ The GitHub site includes a number of companion packages for YAP,
   including [doxygen-yap](https://github.com/vscosta/doxygen-yap), a
   version of doxygen adapted to Prolog that was used to generate
   these documents.

### Compiling YAP 


YAP-6.3.4 is a [cmake](www.cmake.org) based
system. We use `cmake` because it supports mosts popular software, can
generate Makefiles, Ninja, Apple's XCode, VisualStudio and ANdroid
Studio, and because it includes packaging suppport, The steps required
to install core YAP under `cmake` are presented in detail next.

#### The compiler: *Status as of early 2017* 

YAP should compile well under the [GNU-CC](https://gcc.gnu.org/) and
    the [C-LANG](https://clang.llvm.org/) families, that are available
    across most configurations. It sshould also compile well undder
    Intel `icc`.

We do not recommend using Microoft's VC++. To the best of our
    knowledge MSC does not support threaded emulation, which YAP recquires
    for performance, You can still use the IDE, and experiment with
    the c-lang plugin.

YAP compiles cleanly under cross-compilers, and we have used the
    crosss-compilation system [mxe](http://mxe.cc/) system with good results.

#### cmake 
 <!--- $0 --->

All Linux and BSD distributions include `cmake`, so
does [Homebrew](https://brew.sh/)
and [MacPorts](https://www.macports.org/) for the Mac,
and [MSYS2](http://www.msys2.org/)
and [cygwin](http://www.cygwin.org/) for WIN32. Android Studio has
native support for `cmake`since 2.1, although we advise to use
2.2. Last, there are excellent plugins for the Visual Codes. In case
you need a recent version, consider using pre-compiled binaries at
the [CMake site](https://www.cmake.org).

If you have an older Linux you may need to compile from source,
available at GitHub.

#### [Ensure that you have other necessary packages installed: ](extrapacks)
 <!--- $0 --->

+ YAP requires [gmp]{https://gmplib.org/} for infinite precision
          integer and rational. Please ensure the development pacakage
          is installed in Linux: `(lib)gmp-dev(el). In the Mac and
          WIN32, you can obtain GMPlib from the pakage collections mentioned above.

	  The [MPIR]{http://mpir.org} library is compatible with GMPlib,
      and has good support for VC++.

+ The [readline]() library provides line-editing and command
  history. In Linux, make sure you have the development package.

  Readline is disabled by default in WIN32. OSX is distributed with a line editing library that includes a subset of readline. We advise against using that library.

- To use YAP as a library in [Python]() you need the [SWIG]()
  interface generator and python3-dev. SWIG and Python binaries can be
  easily obtained  for all these platfors

	+ make sure to install Python-3, and not Python-2,

#### Compile and Install 

1: Create a directory, say `Build` and `cd` to the directory (`cd Build`).
	*YAP should not be compiled at its rootxo directory, some packages do not allow for that.

2: Run `cmake ../` from within `Build` (or equivalent)

3: Run `make` from within `Build` (or equivalent)

4: If the compilation succeeds, try `./yap`.  This is your executable.

5: If you feel satisfied with the result, do `make install`.
	* In most systems you will need to be superuser in order to do `make install` and `make info` on the standard directories.

#### the Functionality of YAP 

By default, YAP supports tabling, depth first search, and most features found in
modern Prologs. In some cases, you may want to suport extra features, or reduce system size.
`cmake`provides an graphical interface for doing so. From the commmand line,  a typical example could be:

~~~~~
Cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=~ /users/vsc/src/yap
~~~~~

The first argument says that this is a release, compiled with full optimisation. The second argument says YAP should install under the ~ drectory. In this case, YAP will add the binaries to /users/vsc/bin, include files to `/users/vsc/include/Yap`, Prolog files to `/users/vsc/share/Yap`, and ay DLL to `/users/vsc/lib/Yap`.

Options include:

+ `CMAKE_BUILD_TYPE`: the two main options are `Debug`, for system development, and Release. to use the system.

+ `CMAKE_INSTALL_PREFIX`: where to install YAP, by default `/usr/local`.

+ `WITH_DEPTH_LIMIT` allows depth limited evaluation, say for
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

Bext follow instructions to fully compile YAP:

#### Compilation Notes for OSX/Brew 
 <!--- $0 --->

Next follows a detailed description of a full install of YAP, including all the packages that YAP can use:

	1. Install the XCode toolkit from the Apple App Store (you may have to register as a developer).

	2. Install a package manager, such as [brew](http://brew.sh),

	3. You will need `cmake` and `gmp`: both are available in brew.

	4. other brew packages you may want to install: `gecode`, `libxml2`, `openssl`,
 `mariadb`, `openmpi`,  `postgresql`, `raptor`,
 `sqlite3`, `swig`, `unixodbc`. Note that some of  them may be preconditions
 to other packages.

	5. to install [cudd](http://vlsi.colorado.edu/~fabio/CUDD), a package used by ProbLog and cplint, just use:
~~~~~
brew tap mht208/formal
brew install cudd
~~~~~

	6. To use [R](https://www.r-project.org), you must download the `R` distribution from one of the many `R` mirrors.

	6. To use [Java](https://www.java.com), you should download the Oracle distributed JDK.

	6. When installing [Python](https://www.python.org), verify which python you
 are using.

	7. There may be conflict between  original OSX and the brew packages (eg, `openssl`, `python`, and `sqlite3`. If you prefer leaning on brew:
~~~~~
cmake -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl ..
~~~~~

#### Compilation Notes for Android

 Next we present the compilation process for Android. The environment is an OSX, but steps
 should be similar for Linux machines. We assume you have downloaded both the Android NDK and the Android SDK.

1. Reserve a directory for compilation:
~~~~~
 mkdir android
 cd android
~~~~~

2. Get the [GMP-android](https://github.com/Rupan/gmp) port.

2. Call `cmake`:
 ~~~~~
 export ANDROID_NDK=/Volumes/Transcend/vsc/android-ndk-r11c; \
 export ANDROID_SDK=/Volumes/Transcend/vsc/AndroidSDK;\
 export ANDROID_PORT=1;\
 export  PATH=$PATH:/Volumes/Transcend/vsc/AndroidSDK/tools/;\
 cmake \
       -DGMP_INCLUDE_DIRS=/Users/vsc/github/gmp/armeabi-v7a \
       -DGMP_LIBRARIES=/Users/vsc/github/gmp/armeabi-v7a/libgmp.so\
       -DCMAKE_TOOLCHAIN_FILE=/users/vsc/github/yap-6.3/cmake/android.toolchain.cmake \
      -DANDROID_ABI=armeabi-v7a -DANDROID_NATIVE_API_LEVEL=android-23  \
      .. && make -j
~~~~~
