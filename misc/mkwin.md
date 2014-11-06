
AT the time of this writing (Nov 2014), YAP uses the mkwin script to
compile in WIN32. The script requires either a WIN32 environment, or a
cross-compiler/emulator package.

YAP has been known to compile under VISUAL C++, and should compile and
work under cygwin, but the favorite approach is to use a native
msys/mingw environment. This approach has two key advantages:

  + it does not need an interface layer and a DLL, like cygwin.

  + it enables cross-compilation.

YAP uses rge `mkwin` script to generate a new YAP installer. The script is
controlled by a set of of variables that should be defined early on in
the text. It executes by first calling `configure`, next running `make`, and
last (if all went well) executing `nsys`.

In more detail, the following mingw based environments have been
tested to develop YAP:

  * MSYS 1 and mingw32/64: most WIN32 development did occur in this
    native environment. Best results were achieved with
    MSYS-1.0.* and TDM-GCC:
     
    mingw: http://www.mingw.org/
    original msys: http://www.mingw.org/wiki/MSYS
    mingw64: http://mingw-w64.sourceforge.net/
    TDM-GCC: http://tdm-gcc.tdragon.net/

  * This distribution was compiled with the MSYS2 integrated
    development, that supports 32 and 64 bit compilation. Setting up
    MSYS2 should be done with care, but it is worth it as the
    distribution works nicely in MINGW32 and MINGW64 mode. A third
    compilation mode, MSYS mode, has problems with compiling sockets.

    msys2: http://sourceforge.net/projects/msys2/

  * cygwin and cygwin64 now can generate native applications

    cygwin: https://www.cygwin.com/

  * Linux has a nice cross-compilation environment, with some of the best 
    work done for Fedora.

    fedora mingw cross-compiler: http://fedoraproject.org/wiki/MinGW/CrossCompilerFramework

    One problem is that this environment requires emulation of WIN32
    executables to generate the initial saved state and to compile
    `chr`. `wine` sometimes does the task, but it sometimes fails.

  * OSX has the `mxe` package, a port of mingw that is in active 
    development.

    mxe: http://mxe.cc/

    Note that OSX has technical limitations that preclude porting
    wine64. wine32 is distributed with package managers such as ports
    and brew.

=== Setting up WIN32 compilation

Compiling WIN32 packages depends on a number of parameters: chosen compiler,
packages to install, directory setup. You may have to change these ones that
control the `mkwin` script:

 * `VER`: major/minor number  
 * `PATCHID`: third digit
 * `SRC`: directory containing yap sources, in the local environment notation.
 * `SRC_WIN`: same, but in WIN32 standard notation.
 * `THREADS`: yes or no? controllable from the command line.
 * `ABI`: "32" or "64", controllable from the command line.
 * `NSIS`: installer generator, usually "/c/Program Files (x86)/NSIS/makensis".
 * `DOCS_DIR`: where you have the doxygen output.
 * `GCC_DIR`: root of gcc seup.
 * `HOST`: argument to `--host` configure command.
 * `BUILD`: build directory
 * `GMP`: multi-precision package; yes, no, or the installation directory; usually in the distribution.
 * `CUDD`: BDD package, usually in the distribution.
 * `JAVA`: Java sdk directory, usually in the distribution.
 * `PYTHON`: Python package, usually in the distribution.
 * `R`: R environment package, usually in the distribution.
 * `GECODE`: constraint solver package, usually not in the WIN32 distribution.