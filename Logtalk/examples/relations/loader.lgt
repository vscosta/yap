
:- initialization((
	logtalk_load(
		[library(events_loader), library(types_loader), library(hierarchies_loader)],
		[reload(skip)]),							% allow for static binding
	logtalk_load(roots(loader), [reload(skip)]),	% allow for static binding
	logtalk_load(relations))).
