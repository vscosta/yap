
@addtogroup absolute_file_name

@pred absolute_file_name(+File:atom, +Options:list, +Path:atom) is nondet
@pred absolute_file_name(-File:atom, +Path:atom, +Options:list) is nondet

   _Options_ is a list of options to guide the conversion:

  -  extensions(+ _ListOfExtensions_)

     List of file-name suffixes to add to try adding to the file. The
     Default is the empty suffix, `''`.  For each extension,
     absolute_file_name/3 will first add the extension and then verify
     the conditions imposed by the other options.  If the condition
     fails, the next extension of the list is tried.  Extensions may
     be specified both with dot, as `.ext`, or without, as plain
     `ext`.

  -  relative_to(+ _FileOrDir_ )

     Resolve the path relative to the given directory or directory the
     holding the given file.  Without this option, paths are resolved
     relative to the working directory (see working_directory/2) or,
     if  _Spec_  is atomic and absolute_file_name/3 is executed
     in a directive, it uses the current source-file as reference.

  -  access(+ _Mode_ )

     Imposes the condition access_file( _File_ ,  _Mode_ ).   _Mode_  is one of `read`, `write`, `append`, `exist` or
     `none` (default).

     See also access_file/2.

  -  file_type(+ _Type_ )

     Defines suffixes matching one of several pre-specified type of files. Default mapping is as follows:

       1.  `txt` implies `[ '' ]`,

       2.  `prolog` implies `['.yap', '.pl', '.prolog', '']`,

       3.  `executable`  implies `['.so', ',dylib', '.dll']` depending on the Operating system,

       4.  `qly` implies `['.qly', '']`,

       5.  `directory` implies `['']`,

       6.  The file-type `source` is an alias for `prolog` designed to support compatibility with SICStus Prolog. See also prolog_file_type/2.

     Notice that this predicate only
     returns non-directories, unless the option `file_type(directory)` is
     specified, or unless `access(none)`.

  -  file_errors(`fail`/`error`)

     If `error` (default), throw  `existence_error` exception
     if the file cannot be found.  If `fail`, stay silent.

  -  solutions(`first`/`all`)

     If `first` (default), commit to the first solution.  Otherwise
     absolute_file_name will enumerate all solutions via backtracking.

  -  expand(`true`/`false`)

     If `true` (default is `false`) and _Spec_ is atomic, call
     expand_file_name/2 followed by member/2 on _Spec_ before
     proceeding.  This is originally a SWI-Prolog extension, but
     whereas SWI-Prolog implements its own conventions, YAP uses the
     shell's `glob` primitive.

  -  glob(`Pattern`)

     If  _Pattern_ is atomic, add the pattern as a suffix to the current expansion, and call
     expand_file_name/2 followed by member/2 on the result. This is originally  a SICStus Prolog exception.

     Both `glob` and `expand` rely on the same underlying
     mechanism. YAP gives preference to `glob`.

  -  verbose_file_search(`true`/`false`)

     If `true` (default is `false`) output messages during
     search. This is often helpful when debugging. Corresponds to the
     SWI-Prolog flag `verbose_file_search` (also available in YAP).


Compatibility considerations to common argument-order in ISO as well
as SICStus absolute_file_name/3 forced us to be flexible here.
If the last argument is a list and the second not, the arguments are
swapped, making the call
~~~~~~~~~~~prolog
  absolute_file_name(+ _Spec_ , - _Path_ ,+ _Options_ )
~~~~~~~~~~~
  valid as well.


@pred user:library_directory(?Directory:atom) is nondet, dynamic

Dynamic, multi-file predicate that succeeds when _Directory_ is a
current library directory name. Asserted in the user module.

Library directories are the places where files specified in the form
`library( _File_ )` are searched by the predicates consult/1,
reconsult/1, use_module/1, ensure_loaded/1, and load_files/2.

This directory is initialized by a rule that calls  the system predicate
system_library/1.
