
:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),
	logtalk_load([timetable, forms, periods, subjects, teachers]))).
