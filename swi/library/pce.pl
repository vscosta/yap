
% support global analysis of the full yap xsources.

:- module(pce,
          [ 
      op(100,  xf,  *),
	op(1200, xfx, :->),
	op(1200, xfx, :<-),
	op(910,  xfy, ::),
        op(200, fy,  @),
        op(250, yfx, ?),
        op(990, xfx, :=)
         ]).
