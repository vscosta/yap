
:- initialization((
	logtalk_load(library(lgtunit), [reload(skip)]),		% allow for static binding
	logtalk_load(testing))).
