
:- initialization(
	logtalk_load([
		termp, term,
		atomic,
		atom, callable,
		characterp, character,
		number, float, integer, natural,
		compound,
		listp, list, difflist,
		numberlistp, numberlist,
		varlist,
		queuep, queue,
		dictionaryp, bintree,
		setp, set,
		comparingp])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[termp, term,
		atomic,
		atom, callable,
		characterp, character,
		number, float, integer, natural,
		compound,
		listp, list, difflist,
		numberlistp, numberlist,
		varlist,
		queuep, queue,
		dictionaryp, bintree,
		setp, set,
		comparingp], [xmlsref(standalone)])).
*/
