
:- op(1200, xfx, @).				% rulename
:- op(1190, xfx, pragma).
:- op(1180, xfx, [==>, <=>]).
:- op(1180,fy,chr_spy).
:- op(1180,fy,chr_nospy).
:- op(1150, fx, handler).
:- op(1150, fx, constraints).
:- op(1150, fx, rules).
% :- op(1100, xfx, '|').			% read as ; by sicstus
:- op(1100, xfx, \ ).
:- op(1050,xfx,&).				% current_op(1000,xfy,',')
% :- op(500,yfx,#).				% already defined in SICStus
