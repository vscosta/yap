
:- initialization(
	logtalk_load([
		hierarchyp,
		proto_hierarchyp, proto_hierarchy,
		class_hierarchyp, class_hierarchy])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[hierarchyp,
		proto_hierarchyp, proto_hierarchy,
		class_hierarchyp, class_hierarchy], [xmlsref(standalone)])).
*/
