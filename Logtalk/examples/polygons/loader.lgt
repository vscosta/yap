
:- initialization((
	logtalk_load([library(events_loader), library(types_loader), library(metapredicates_loader), library(hierarchies_loader)], [reload(skip)]),
	logtalk_load(roots(loader), [reload(skip)]),
	logtalk_load(relations(loader), [reload(skip)]),
	logtalk_load(polygons, [events(on)]))).
