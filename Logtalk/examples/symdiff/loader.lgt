/*
On Windows, the compilation of this example generates invalid file names for 
the XML documenting files, wence the xmldocs(off) option used below. No problems
on MacOS X and no problems expect in other POSIX systems.
*/

:- initialization(
	logtalk_load(symdiff, [xmldocs(off)])). 
