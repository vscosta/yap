
:- initialization(
	logtalk_load([
		relations])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[relations], [xmlsref(standalone)])).
*/
