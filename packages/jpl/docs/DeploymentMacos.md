#  Deploying for users - on MacOS

First of all, you may want to check [this issue](https://github.com/SWI-Prolog/packages-jpl/issues/2#event-1900141361) entry.
 
So, there is an issue with using JPL in Mac OS due to a linking error in JPL. 

The problem was that SWI-Prolog come as a _relocatable_ app and the JPL library is therefore _run-path dependent_, causing the linking error when loaded without the proper run-path search paths. That is JPL does not know where SWI is.
 
So, one way to solve this would to use a _not_ relocatable version of SWI-prolog such as the homebrew or macport installs. However, those do not include the JPL library. :-(
 
The only option is therefore to manually remove the run-path dependencies from the lib using the `install_name_tool`: 

    install_name_tool <old_path> <new_path> dynlib

So the first step is to install a not relocatable version of SWI-prolog:

    brew install swi-prolog

This will install SWI-Prolog (here version 7.6.4) in `/usr/local/Cellar/swi-prolog/7.6.4/`

As the homebrew version does _not_ come with the JPL library (an [issue](https://github.com/Homebrew/homebrew-core/issues/32364) on the homebrew github as been opened about this), the second step is therefore to buid the library from source. Alternatively, you can download the already compiled  [libjpl.dylib](files/libjpl.dylib) (version 7.7.19) and copy it into folder:

    /usr/local/Cellar/swi-prolog/7.6.4/libexec/lib/swipl-7.6.4/lib/

Now we need to ensure all dependencies link of the lib are valid (and absolute).
Then cd into this directory and check all the dependencies paths using the command:

    otool -L libjpl.dylib

For example:

    libjpl.dylib:
    /Applications/SWI-Prolog.app/Contents/swipl/lib/x86_64-darwin15.6.0/libjpl.dylib:
        @rpath/libjsig.dylib (compatibility version 1.0.0, current version 1.0.0)
        @rpath/libjvm.dylib (compatibility version 1.0.0, current version 1.0.0)
        @executable_path/../swipl/lib/x86_64-darwin15.6.0/libswipl.dylib (compatibility version 0.0.0, current version 7.6.4)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1226.10.1)

If an entry is not a valid path on your system you can change it using the command:

    install_name_tool -change <the_invalid_path> <your_new_valid_path> libjpl.dylib

Above, for example, `libswipl.dylib` is not installed in  `@executable_path/../swipl/lib/x86_64-darwin15.6.0/libswipl.dylib` but instead in `/usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libswipl.dylib`
So we can do:

    install_name_tool -change @rpath/libjsig.dylib /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/jre/lib/server/libjsig.dylib libjpl.dylib
    install_name_tool -change @rpath/libjvm.dylib /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/jre/lib/server/libjvm.dylib libjpl.dylib
    install_name_tool -change @executable_path/../swipl/lib/x86_64-darwin15.6.0/libswipl.dylib /usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libswipl.dylib libjpl.dylib

As you can see the goal here is to replace of the run-path dependent part (`@rpath` or `@executable_path`) with an absolute path on your system.

After doing this step using `install_name_tool`, your lib should not depend on any external run-path:

    libjpl.dylib:
     /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/jre/lib/server/libjsig.dylib (compatibility version 1.0.0, current version 1.0.0)
     /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/jre/lib/server/libjvm.dylib (compatibility version 1.0.0, current version 1.0.0)
     /usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libswipl.dylib (compatibility version 0.0.0, current version 7.7.19)
     /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.50.4)


Once all entries are valid, you can add the path to this library:

    /usr/local/Cellar/swi-prolog/7.6.4/libexec/lib/swipl-7.6.4/lib/ 

to your `java.path.libray` and it will, hopefully, link correctly: :-)
  
    java -Djava.library.path=/usr/local/Cellar/swi-prolog/7.6.4/libexec/lib/swipl-7.6.4/lib/x86_64-darwin17.3.0/
 
The Java Virtual Machine (JVM) uses the `java.library.path` property in order to locate and load _native libraries_. 
When a Java application loads a native library using the `System.loadLibrary()` method, the `java.library.path` is scanned for the specified library. 
If the JVM is not able to detect the requested library, it throws an `UnsatisfiedLinkError`. 
See [this tutorial](https://examples.javacodegeeks.com/java-basics/java-library-path-what-is-it-and-how-to-use/) for more explanation on that variable.


You can also do this in Eclipse: 

1. Select your project in the Package Explorer area and press a right click on it.
2. Select `Build Path → Configure Build Path...` option.
3. In the appearing window, select the `Libraries` tab.
4. Then, expand the JRE System library option and select the Native library location.
5. Click on the Edit... button at the right panel.
6. Locate the required library and then click OK.
 
And that's it, your project will link and run properly on Mac OS ;)


### Acknowledgements

The above guide is a further elaboration from Sebastian Sardina to the original guide by Theo Champion in the context of my AOPD course in 2018. Thanks Theo!
