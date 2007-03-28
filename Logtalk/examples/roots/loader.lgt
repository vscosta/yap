
:- initialization((
	logtalk_load([library(events_loader), library(types_loader), library(hierarchies_loader)], [reload(skip)]),
	logtalk_load([initialization, classes, prototypes, nil], [unknown(silent)]))).
