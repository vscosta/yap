
:- initialization(
	logtalk_load([
		event_handlersp,
		event_registryp, event_registry,
		before_event_registry, after_event_registry,
		monitorp, monitor], [events(on), reload(skip)])).	% allow for static binding
