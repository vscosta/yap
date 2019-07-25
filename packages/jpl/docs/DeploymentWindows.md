# Deploying for users - on Windows

Tested successfully in Windows 7 with SWI 7.6.4.

* Make sure SWI is installed with the JPL Java-SWI connectivity. You should have a `jpl.dll` (in the SWI `bin/` subdir) and a `jpl.jar` (in the SWI `lib/` subdir).
* Define a _system_ environment variable `SWI_HOME_DIR` and set it to the root directory of your installed version of SWI-Prolog (e.g., to `C:\Program Files\swipl`).
    * Extend `Path` system environment variable with the following two components:
        * `%SWI_HOME_DIR%\bin`
        * `%SWI_HOME_DIR%\lib\jpl.jar`
* No changes to `CLASSPATH` are needed.

### Troubleshooting

If the example programs don't run successfully, look carefully at any error messages; they typically indicate where the problem lies.  Then check the installation instruction s and prerequisites carefully. 
  
 * If you get a message about
 
        ... jpl.dll ... Access is denied ...
 
    then you may have lost execute permission on `jpl.dll` (please consult local Windows expertise if you don't know how to correct this).
 
 * If `jpl_examples/0` complains that
 
        The dynamic link library jvm.dll could not be found in the specified path 
  
    then you should locate jvm.dll within the Java runtime which you intend to use, and ensure that its directory is within the `PATH`.
 
 * If the Java examples (e.g. `jpl\examples\Exception\run.bat`) complain that 
  
        The name specified is not recognized as an internal or external command, operable program or batch file. 
   
     then there is no Java executable java.exe in any folder on your `PATH`: you should have a `PATH` entry such as `C:\jdk1.3.1_01\bin;` 
     
 * If the Java examples complain that
 
        The dynamic link library libpl.dll could not be found in the specified path
     or
     
        Exception in thread "main" java.lang.UnsatisfiedLinkError: C:\paul\bin\jpl.dll: Can't find dependent libraries 
 
     then there is no SWI-Prolog library `libpl.dll` in any folder on your `PATH`: you should have a `PATH` entry such as `C:\Program Files\pl\bin`
