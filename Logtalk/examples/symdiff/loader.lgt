/*
On Windows, the compilation of this example generates invalid file names for 
the XML documenting files, wence the xmldocs(off) option used below. No problems
on MacOS X and no problems expect in other POSIX systems.
*/

:- initialization(
	logtalk_load(
		[symdiff], [xmldocs(off)])). 

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[symdiff], [xmlsref(standalone)])).
*/
