
:- initialization(
	logtalk_load([
		termp, term,
		atomic,
		atom, callable,
		characterp, character,
		number, float, integer, natural,
		compound,
		listp, list, list1,
		difflist,
		numberlistp, numberlist, varlist,
		queuep, queue,
		dictionaryp, bintree,
		setp, set, set1,
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
		listp, list, list1,
		difflist,
		numberlistp, numberlist, varlist,
		queuep, queue,
		dictionaryp, bintree,
		setp, set, set1,
		comparingp], [xmlsref(standalone)])).
*/
