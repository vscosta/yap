
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
		comparingp], [reload(skip)])).
