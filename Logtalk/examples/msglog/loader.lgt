
:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),
	logtalk_load(msglog, [events(on)]))).
