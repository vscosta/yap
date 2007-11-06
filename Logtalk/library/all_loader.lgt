
:- initialization(
	logtalk_load([
		library(dates_loader),
		library(events_loader),
		library(debugging_loader),
		library(dependents_loader),
		library(hierarchies_loader),
		library(metapredicates_loader),
		library(random_loader),
		library(types_loader)],
		[reload(skip)])).	% allow for static binding
