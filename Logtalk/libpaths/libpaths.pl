
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.22.3
%
%  Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(
	(assertz(logtalk_library_path(library, '$LOGTALKUSER/library/')),
	 assertz(logtalk_library_path(aliases, '$LOGTALKUSER/examples/aliases/')),
	 assertz(logtalk_library_path(benchmarks, '$LOGTALKUSER/examples/benchmarks/')),
	 assertz(logtalk_library_path(birds, '$LOGTALKUSER/examples/birds/')),
	 assertz(logtalk_library_path(bricks, '$LOGTALKUSER/examples/bricks/')),
	 assertz(logtalk_library_path(classvars, '$LOGTALKUSER/examples/classvars/')),
	 assertz(logtalk_library_path(dcgs, '$LOGTALKUSER/examples/dcgs/')),
	 assertz(logtalk_library_path(diamonds, '$LOGTALKUSER/examples/diamonds/')),
	 assertz(logtalk_library_path(dynpred, '$LOGTALKUSER/examples/dynpred/')),
	 assertz(logtalk_library_path(engines, '$LOGTALKUSER/examples/engines/')),
	 assertz(logtalk_library_path(errors, '$LOGTALKUSER/examples/errors/')),
	 assertz(logtalk_library_path(hello_world, '$LOGTALKUSER/examples/hello_world/')),
	 assertz(logtalk_library_path(inheritance, '$LOGTALKUSER/examples/inheritance/')),
	 assertz(logtalk_library_path(instmethods, '$LOGTALKUSER/examples/instmethods/')),
	 assertz(logtalk_library_path(lo_planner, '$LOGTALKUSER/examples/lo/planner/')),
	 assertz(logtalk_library_path(lo_travellers, '$LOGTALKUSER/examples/lo/travellers/')),
	 assertz(logtalk_library_path(logic, '$LOGTALKUSER/examples/logic/')),
	 assertz(logtalk_library_path(lpa, '$LOGTALKUSER/examples/lpa/')),
	 assertz(logtalk_library_path(metainterpreters, '$LOGTALKUSER/examples/metainterpreters/')),
	 assertz(logtalk_library_path(metapredicates, '$LOGTALKUSER/examples/metapredicates/')),
	 assertz(logtalk_library_path(mi, '$LOGTALKUSER/examples/mi/')),
	 assertz(logtalk_library_path(miscellaneous, '$LOGTALKUSER/examples/miscellaneous/')),
	 assertz(logtalk_library_path(msglog, '$LOGTALKUSER/examples/msglog/')),
	 assertz(logtalk_library_path(operators, '$LOGTALKUSER/examples/operators/')),
	 assertz(logtalk_library_path(parametric, '$LOGTALKUSER/examples/parametric/')),
	 assertz(logtalk_library_path(poem, '$LOGTALKUSER/examples/poem/')),
	 assertz(logtalk_library_path(points, '$LOGTALKUSER/examples/points/')),
	 assertz(logtalk_library_path(polygons, '$LOGTALKUSER/examples/polygons/')),
	 assertz(logtalk_library_path(profiling, '$LOGTALKUSER/examples/profiling/')),
	 assertz(logtalk_library_path(puzzles, '$LOGTALKUSER/examples/puzzles/')),
	 assertz(logtalk_library_path(reflection, '$LOGTALKUSER/examples/reflection/')),
	 assertz(logtalk_library_path(relations, '$LOGTALKUSER/examples/relations/')),
	 assertz(logtalk_library_path(roots, '$LOGTALKUSER/examples/roots/')),
	 assertz(logtalk_library_path(searching, '$LOGTALKUSER/examples/searching/')),
	 assertz(logtalk_library_path(shapes_ch, '$LOGTALKUSER/examples/shapes/ch/')),
	 assertz(logtalk_library_path(shapes_ph, '$LOGTALKUSER/examples/shapes/ph/')),
	 assertz(logtalk_library_path(sicstus, '$LOGTALKUSER/examples/sicstus/')),
	 assertz(logtalk_library_path(symdiff, '$LOGTALKUSER/examples/symdiff/')),
	 assertz(logtalk_library_path(viewpoints, '$LOGTALKUSER/examples/viewpoints/')))).
