
:- op(10, fy, ~).
:- op(20, yfx, &).
:- op(30, yfx, v).
:- op(40, xfx, =>).
:- op(40, xfx, <=>).

:- initialization(
	logtalk_load([
		translator])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[translator], [xmlsref(standalone)])).
*/
