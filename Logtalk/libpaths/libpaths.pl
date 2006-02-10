
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.27.0
%
%  Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(
	(assertz(logtalk_library_path(lgtuser, '$LOGTALKUSER/')),
	 assertz(logtalk_library_path(contributions, lgtuser('contributions/'))),
	 assertz(logtalk_library_path(examples, lgtuser('examples/'))),
	 assertz(logtalk_library_path(library, lgtuser('library/'))),
	 assertz(logtalk_library_path(aliases, examples('aliases/'))),
	 assertz(logtalk_library_path(assignvars, examples('assignvars/'))),
	 assertz(logtalk_library_path(benchmarks, examples('benchmarks/'))),
	 assertz(logtalk_library_path(birds, examples('birds/'))),
	 assertz(logtalk_library_path(bricks, examples('bricks/'))),
	 assertz(logtalk_library_path(classvars, examples('classvars/'))),
	 assertz(logtalk_library_path(dcgs, examples('dcgs/'))),
	 assertz(logtalk_library_path(diamonds, examples('diamonds/'))),
	 assertz(logtalk_library_path(dynpred, examples('dynpred/'))),
	 assertz(logtalk_library_path(encodings, examples('encodings/'))),
	 assertz(logtalk_library_path(engines, examples('engines/'))),
	 assertz(logtalk_library_path(errors, examples('errors/'))),
	 assertz(logtalk_library_path(expansion, examples('expansion/'))),
	 assertz(logtalk_library_path(hello_world, examples('hello_world/'))),
	 assertz(logtalk_library_path(hooks, examples('hooks/'))),
	 assertz(logtalk_library_path(inheritance, examples('inheritance/'))),
	 assertz(logtalk_library_path(instmethods, examples('instmethods/'))),
	 assertz(logtalk_library_path(lo_planner, examples('lo/planner/'))),
	 assertz(logtalk_library_path(lo_travellers, examples('lo/travellers/'))),
	 assertz(logtalk_library_path(logic, examples('logic/'))),
	 assertz(logtalk_library_path(lpa_faults, examples('lpa/faults/'))),
	 assertz(logtalk_library_path(lpa_timetables, examples('lpa/timetables/'))),
	 assertz(logtalk_library_path(metainterpreters, examples('metainterpreters/'))),
	 assertz(logtalk_library_path(metapredicates, examples('metapredicates/'))),
	 assertz(logtalk_library_path(mi, examples('mi/'))),
	 assertz(logtalk_library_path(miscellaneous, examples('miscellaneous/'))),
	 assertz(logtalk_library_path(modules, examples('modules/'))),
	 assertz(logtalk_library_path(msglog, examples('msglog/'))),
	 assertz(logtalk_library_path(operators, examples('operators/'))),
	 assertz(logtalk_library_path(parametric, examples('parametric/'))),
	 assertz(logtalk_library_path(poem, examples('poem/'))),
	 assertz(logtalk_library_path(points, examples('points/'))),
	 assertz(logtalk_library_path(polygons, examples('polygons/'))),
	 assertz(logtalk_library_path(profiling, examples('profiling/'))),
	 assertz(logtalk_library_path(puzzles, examples('puzzles/'))),
	 assertz(logtalk_library_path(reflection, examples('reflection/'))),
	 assertz(logtalk_library_path(relations, examples('relations/'))),
	 assertz(logtalk_library_path(roots, examples('roots/'))),
	 assertz(logtalk_library_path(searching, examples('searching/'))),
	 assertz(logtalk_library_path(shapes_ch, examples('shapes/ch/'))),
	 assertz(logtalk_library_path(shapes_ph, examples('shapes/ph/'))),
	 assertz(logtalk_library_path(sicstus, examples('sicstus/'))),
	 assertz(logtalk_library_path(symdiff, examples('symdiff/'))),
	 assertz(logtalk_library_path(viewpoints, examples('viewpoints/'))))).
