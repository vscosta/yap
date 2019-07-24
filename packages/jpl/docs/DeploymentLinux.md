# Deploying for users - on Linux

Recall that to use JPL under linux one must have the following in place:

* C Native JPL Library `libjpl.so`, generally found at `/usr/lib/swi-prolog/lib/amd64/libjpl.so`
* Java API Jar file `jpl.jar` in the Java `CLASSPATH`, generally accessible from `/usr/share/java/jpl.jar`.
* Prolog API as an SWI source module `jpl.pl` at `$SWI_HOME_DIR/library`, generally accessible from `/usr/lib/swi-prolog/library/jpl.pl`

JPL is **generally distributed with official Linux**. For example, in Ubuntu-based systems, JPL is provided via package `swi-prolog-java`. That package includes the C library `libjpl.so`, the Java API `jpl.jar`, the Prolog module `jpl.pl` as well as all documentation associated.

However, the **official packages are often out-of-date**. For Debian-based systems (Debian, Ubuntu, Mint, ...) you can get the latest stable and development versions via [this PPAs](http://www.swi-prolog.org/build/PPA.txt).

Finally, to be **able to use JPL** you may need to make sure that:

* Extend environment library `LD_PRELOAD` for system to pre-load `libswipl.so`: 
    * `export LD_PRELOAD=libswipl.so:$LD_PRELOAD`
    * Check [this post](https://answers.ros.org/question/132411/unable-to-load-existing-owl-in-semantic-map-editor/) and [this one](https://blog.cryptomilk.org/2014/07/21/what-is-preloading/) about library preloading.
    * Also, check [this](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=690734) and [this](https://github.com/yuce/pyswip/issues/10) posts.
* Extend environment variable `LD_LIBRARY_PATH`  to point to the directory where `libjpl.so` is located:
    * `export LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/amd64/`
* Make sure `jpl.jar` is in your Java CLASSPATH. This is generally automatically done if environment variable `SWI_HOME_DIR` is correctly set to `/usr/lib/swi-prolog/`.

To install JPL from scratch (e.g., to compiling it in order to develop it further) please refer to the [Developing JPL](Developing-JPL) guide.

## Troubleshooting
