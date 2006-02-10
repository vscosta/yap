
:- initialization(
	(logtalk_load(hook),
	 logtalk_load(object, [hook(hook::hook), xmldocs(on)]))). 

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(logtalk_load(hooks, [hook(hook::hook), xmldocs(on), xmlsref(standalone)])).
*/
