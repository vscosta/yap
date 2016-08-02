
## YAPDroid

This file documents the YAPDroid application. This app was developed in order to
test the YAP-Android interface. It provides a simple REPL with a query window and  a text
viewer.

### Design

The YAP interface to Android is based on the SWIG interface generator. SWIG exports the YAP
C++ classes as Java classes. In practice there exist two worlds, the native application
and the Java application:

  - YAP runs in Android as a native application in a Linux
environment. Android does not support glibc. Instead, Android libraries are provided by the Android NDK, and
are somewhat limited, i.e., the NDK misses in-memory streams and a `glob`
predicate. Moreover, read-only data is kept in a zipped archive that is made to appear as
a directory, `~/assets`

  - SWIG generates the glue code: JNI classes that act as Java classes. Callbacks are possible.

  - Java code operates as usual.

### Compiling YAPDroid

The current version was compiled using the recent Android Studio `cmake` support. Android Studio uses
`gradle` as the build system. `gradle` orchestrates compilation of all Java code. External code is delegated
to `cmake` or the Android NDK own builder. The idea fits nicely with YAP; unfortunately, the process currently
crashes in middle.

To install, navigate through these steps:
1. Obtain `gmp` for Android. YAP has used the [Rupan repo](https://github.com/Rupan/gmp); place the repo next to
the yap-6.3 top directory.

2. Install [swig](www/swig.org)

2. Obtain Android Studio. This work used Android Studio 2.2 Beta 3. The IDE includes most everything else you need: the ADK, the NDK, `cmake`, the `ninja` build system, and  the debugger.

3. Set YAPDroid as your directory.

4. Adapt the `build.gradle` files to your configuration.
  + It may be a good idea to first generate an empty configuration and compare.
  + In the `lib` directory, please verify wich targets you are generating for.

4. Build the system, either from the GUI or from a terminal, say as:
~~~~~
./gradlew assembleDebug
~~~~~
or
~~~~~
./gradlew :lib:assembleDebug
~~~~~
You should see `cmake` being executed, and after some time `gradle` crash.

5.  To actually install the binaries, use:
~~~~~
cd lib/build/intermediates/cmake/debug/json/armeabi-v7a
ninja install
~~~~~
6. Comment the four lines in `lib/build.gradle` to avoid crashing in the `cmake` task.

7. use the GUI commands to compile the app or set your directory bak to
the YAPDroid top-directory and call `gradle`.

Enjoy!

### Limitations and TODO

- improve error handling.
- support `/assets`: the code is written but not tested yet.
- network access
- sqlite testing and support
