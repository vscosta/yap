# To do

## Documentation

* Refresh docs/javadoc from each new stable Windows build, or link to a URL where they are published at swi-prolog.org

## Prolog API

* Export `jpl_java_lib_version/1` (for consistency with `jpl_c_lib_version/1` and `jpl_pl_lib_version/1`).

* Export

    ```prolog
    jpl_c_lib_path/1
    jpl_java_lib_path/1
    jpl_pl_lib_path/1
    ```

* Find out whether C code can discover the identity of a JVM *before* initialising it, so JPL can pass vendor-specific or version-specific options when initialising it.

## Java API

* Find out whether C code can discover the version of a SWI-Prolog library *before* initialising it, so JPL can pass version-specific options when initialising it.
