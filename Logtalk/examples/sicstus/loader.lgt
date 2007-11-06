
:- initialization((
	logtalk_load(
		[library(hierarchies_loader), library(types_loader)],
		[reload(skip)]),		% allow for static binding
	logtalk_load([ovals, polygons, sorting]))).
