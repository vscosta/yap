/**
  @file yapops.yap
  @brief all operators in the YAP system
*/

:- 	   op(800, xfx, <==),
	   op(800, xfx, +==),
	   op(800, xfx, -==),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	    op(700, xfx , (in)),
	    op(700, xfx, (within)),
	    op(700, xfx, (ins)),
        op(450, xfx, '..'), % should bind more tightly than \/
	op(790, fx, (matrix)),
	op(790, fx, array),
	op(780, xfx, of),
	op(700, xfx, [?=]),
	op(200, fx, (@)),
%	op(500, xfx, ':='),
op(500, xfx, '+='),
	  op(100, yf, []),
                  op(760, yfx, #<==>),
                  op(750, xfy, #==>),
                  op(750, yfx, #<==),
           op(740, yfx, #\/),
                  op(730, yfx, #\),
                  op(720, yfx, #/\),
                  op(710,  fy, #\),
                  op(705, xfx, where),
                  op(700, xfx, #>),
                  op(700, xfx, #<),
                  op(700, xfx, #>=),
                  op(700, xfx, #=<),
                  op(700, xfx, #=),
                  op(700, xfx, #\=),
                  op(700,  xf, #>),
                  op(700,  xf, #<),
                  op(700,  xf, #>=),
                  op(700,  xf, #=<),
                  op(700,  xf, #=),
                  op(700,  xf, #\=),
		  op(500, yfx, '<=>'),
		  op(500, yfx, '=>'),
	op(600,xfy,~),
	op(600,fy,~),
	op(600,yfx,'..'),
	% op(400,yfx,'%x%'),  % function exists
	% op(400,yfx,'%%'),   % mod
	% op(400,yfx,'%/%'),  % //
	op(300,yfx,@*@),  % op(300,yfx,'%*%'),  % matrix multiplication: inner product
	op(300,yfx,@^@),  % op(300,yfx,'%o%'),  % outer product ?
	% op(400,yfx,'%in%'), % function can be called instead
	op(400,yfx,$),
	op(400,yfx,@),
	op(150,yf,i),  % complex number
	op(800,fx,@),
	op(400,xfy,=+ ),
                  op(450, xfx, ..), % should bind more tightl	   op(100,fy,$),
	   op(950,fy,:=),
	   op(950,yfx,:=),
	    op(50, yf, []),
            op(50, yf, '()'),
            op(100, xfy, '.'),
            op(100, fy, '.').
