%% -*- prolog -*-
%%=============================================================================
%% Copyright (C) 2011 by Denys Duchier
%%
%% This program is free software: you can redistribute it and/or modify it
%% under the terms of the GNU Lesser General Public License as published by the
%% Free Software Foundation, either version 3 of the License, or (at your
%% option) any later version.
%% 
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
%% more details.
%% 
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%=============================================================================

is_IntRelType_('IRT_EQ').
is_IntRelType_('IRT_NQ').
is_IntRelType_('IRT_LQ').
is_IntRelType_('IRT_LE').
is_IntRelType_('IRT_GQ').
is_IntRelType_('IRT_GR').

is_IntRelType_('IRT_EQ','IRT_EQ').
is_IntRelType_('IRT_NQ','IRT_NQ').
is_IntRelType_('IRT_LQ','IRT_LQ').
is_IntRelType_('IRT_LE','IRT_LE').
is_IntRelType_('IRT_GQ','IRT_GQ').
is_IntRelType_('IRT_GR','IRT_GR').

is_IntRelType(X,Y) :- nonvar(X), is_IntRelType_(X,Y).
is_IntRelType(X) :- is_IntRelType(X,_).

is_BoolOpType_('BOT_AND').
is_BoolOpType_('BOT_OR').
is_BoolOpType_('BOT_IMP').
is_BoolOpType_('BOT_EQV').
is_BoolOpType_('BOT_XOR').

is_BoolOpType_('BOT_AND','BOT_AND').
is_BoolOpType_('BOT_OR','BOT_OR').
is_BoolOpType_('BOT_IMP','BOT_IMP').
is_BoolOpType_('BOT_EQV','BOT_EQV').
is_BoolOpType_('BOT_XOR','BOT_XOR').

is_BoolOpType(X,Y) :- nonvar(X), is_BoolOpType_(X,Y).
is_BoolOpType(X) :- is_BoolOpType(X,_).

is_IntConLevel_('ICL_VAL').
is_IntConLevel_('ICL_BND').
is_IntConLevel_('ICL_DOM').
is_IntConLevel_('ICL_DEF').

is_IntConLevel_('ICL_VAL','ICL_VAL').
is_IntConLevel_('ICL_BND','ICL_BND').
is_IntConLevel_('ICL_DOM','ICL_DOM').
is_IntConLevel_('ICL_DEF','ICL_DEF').

is_IntConLevel(X,Y) :- nonvar(X), is_IntConLevel_(X,Y).
is_IntConLevel(X) :- is_IntConLevel(X,_).

is_TaskType_('TT_FIXP').
is_TaskType_('TT_FIXS').
is_TaskType_('TT_FIXE').

is_TaskType_('TT_FIXP','TT_FIXP').
is_TaskType_('TT_FIXS','TT_FIXS').
is_TaskType_('TT_FIXE','TT_FIXE').

is_TaskType(X,Y) :- nonvar(X), is_TaskType_(X,Y).
is_TaskType(X) :- is_TaskType(X,_).

is_ExtensionalPropKind_('EPK_DEF').
is_ExtensionalPropKind_('EPK_SPEED').
is_ExtensionalPropKind_('EPK_MEMORY').

is_ExtensionalPropKind_('EPK_DEF','EPK_DEF').
is_ExtensionalPropKind_('EPK_SPEED','EPK_SPEED').
is_ExtensionalPropKind_('EPK_MEMORY','EPK_MEMORY').

is_ExtensionalPropKind(X,Y) :- nonvar(X), is_ExtensionalPropKind_(X,Y).
is_ExtensionalPropKind(X) :- is_ExtensionalPropKind(X,_).

is_IntVarBranch_('INT_VAR_NONE').
is_IntVarBranch_('INT_VAR_RND').
is_IntVarBranch_('INT_VAR_DEGREE_MIN').
is_IntVarBranch_('INT_VAR_DEGREE_MAX').
is_IntVarBranch_('INT_VAR_AFC_MIN').
is_IntVarBranch_('INT_VAR_AFC_MAX').
is_IntVarBranch_('INT_VAR_MIN_MIN').
is_IntVarBranch_('INT_VAR_MIN_MAX').
is_IntVarBranch_('INT_VAR_MAX_MIN').
is_IntVarBranch_('INT_VAR_MAX_MAX').
is_IntVarBranch_('INT_VAR_SIZE_MIN').
is_IntVarBranch_('INT_VAR_SIZE_MAX').
is_IntVarBranch_('INT_VAR_SIZE_DEGREE_MIN').
is_IntVarBranch_('INT_VAR_SIZE_DEGREE_MAX').
is_IntVarBranch_('INT_VAR_SIZE_AFC_MIN').
is_IntVarBranch_('INT_VAR_SIZE_AFC_MAX').
is_IntVarBranch_('INT_VAR_REGRET_MIN_MIN').
is_IntVarBranch_('INT_VAR_REGRET_MIN_MAX').
is_IntVarBranch_('INT_VAR_REGRET_MAX_MIN').
is_IntVarBranch_('INT_VAR_REGRET_MAX_MAX').

is_IntVarBranch_('INT_VAR_NONE','INT_VAR_NONE').
is_IntVarBranch_('INT_VAR_RND','INT_VAR_RND').
is_IntVarBranch_('INT_VAR_DEGREE_MIN','INT_VAR_DEGREE_MIN').
is_IntVarBranch_('INT_VAR_DEGREE_MAX','INT_VAR_DEGREE_MAX').
is_IntVarBranch_('INT_VAR_AFC_MIN','INT_VAR_AFC_MIN').
is_IntVarBranch_('INT_VAR_AFC_MAX','INT_VAR_AFC_MAX').
is_IntVarBranch_('INT_VAR_MIN_MIN','INT_VAR_MIN_MIN').
is_IntVarBranch_('INT_VAR_MIN_MAX','INT_VAR_MIN_MAX').
is_IntVarBranch_('INT_VAR_MAX_MIN','INT_VAR_MAX_MIN').
is_IntVarBranch_('INT_VAR_MAX_MAX','INT_VAR_MAX_MAX').
is_IntVarBranch_('INT_VAR_SIZE_MIN','INT_VAR_SIZE_MIN').
is_IntVarBranch_('INT_VAR_SIZE_MAX','INT_VAR_SIZE_MAX').
is_IntVarBranch_('INT_VAR_SIZE_DEGREE_MIN','INT_VAR_SIZE_DEGREE_MIN').
is_IntVarBranch_('INT_VAR_SIZE_DEGREE_MAX','INT_VAR_SIZE_DEGREE_MAX').
is_IntVarBranch_('INT_VAR_SIZE_AFC_MIN','INT_VAR_SIZE_AFC_MIN').
is_IntVarBranch_('INT_VAR_SIZE_AFC_MAX','INT_VAR_SIZE_AFC_MAX').
is_IntVarBranch_('INT_VAR_REGRET_MIN_MIN','INT_VAR_REGRET_MIN_MIN').
is_IntVarBranch_('INT_VAR_REGRET_MIN_MAX','INT_VAR_REGRET_MIN_MAX').
is_IntVarBranch_('INT_VAR_REGRET_MAX_MIN','INT_VAR_REGRET_MAX_MIN').
is_IntVarBranch_('INT_VAR_REGRET_MAX_MAX','INT_VAR_REGRET_MAX_MAX').

is_IntVarBranch(X,Y) :- nonvar(X), is_IntVarBranch_(X,Y).
is_IntVarBranch(X) :- is_IntVarBranch(X,_).

is_IntValBranch_('INT_VAL_MIN').
is_IntValBranch_('INT_VAL_MED').
is_IntValBranch_('INT_VAL_MAX').
is_IntValBranch_('INT_VAL_RND').
is_IntValBranch_('INT_VAL_SPLIT_MIN').
is_IntValBranch_('INT_VAL_SPLIT_MAX').
is_IntValBranch_('INT_VAL_RANGE_MIN').
is_IntValBranch_('INT_VAL_RANGE_MAX').
is_IntValBranch_('INT_VALUES_MIN').
is_IntValBranch_('INT_VALUES_MAX').

is_IntValBranch_('INT_VAL_MIN','INT_VAL_MIN').
is_IntValBranch_('INT_VAL_MED','INT_VAL_MED').
is_IntValBranch_('INT_VAL_MAX','INT_VAL_MAX').
is_IntValBranch_('INT_VAL_RND','INT_VAL_RND').
is_IntValBranch_('INT_VAL_SPLIT_MIN','INT_VAL_SPLIT_MIN').
is_IntValBranch_('INT_VAL_SPLIT_MAX','INT_VAL_SPLIT_MAX').
is_IntValBranch_('INT_VAL_RANGE_MIN','INT_VAL_RANGE_MIN').
is_IntValBranch_('INT_VAL_RANGE_MAX','INT_VAL_RANGE_MAX').
is_IntValBranch_('INT_VALUES_MIN','INT_VALUES_MIN').
is_IntValBranch_('INT_VALUES_MAX','INT_VALUES_MAX').

is_IntValBranch(X,Y) :- nonvar(X), is_IntValBranch_(X,Y).
is_IntValBranch(X) :- is_IntValBranch(X,_).

is_IntAssign_('INT_ASSIGN_MIN').
is_IntAssign_('INT_ASSIGN_MED').
is_IntAssign_('INT_ASSIGN_MAX').
is_IntAssign_('INT_ASSIGN_RND').

is_IntAssign_('INT_ASSIGN_MIN','INT_ASSIGN_MIN').
is_IntAssign_('INT_ASSIGN_MED','INT_ASSIGN_MED').
is_IntAssign_('INT_ASSIGN_MAX','INT_ASSIGN_MAX').
is_IntAssign_('INT_ASSIGN_RND','INT_ASSIGN_RND').

is_IntAssign(X,Y) :- nonvar(X), is_IntAssign_(X,Y).
is_IntAssign(X) :- is_IntAssign(X,_).

is_SetRelType_('SRT_EQ').
is_SetRelType_('SRT_NQ').
is_SetRelType_('SRT_SUB').
is_SetRelType_('SRT_SUP').
is_SetRelType_('SRT_DISJ').
is_SetRelType_('SRT_CMPL').

is_SetRelType_('SRT_EQ','SRT_EQ').
is_SetRelType_('SRT_NQ','SRT_NQ').
is_SetRelType_('SRT_SUB','SRT_SUB').
is_SetRelType_('SRT_SUP','SRT_SUP').
is_SetRelType_('SRT_DISJ','SRT_DISJ').
is_SetRelType_('SRT_CMPL','SRT_CMPL').

is_SetRelType(X,Y) :- nonvar(X), is_SetRelType_(X,Y).
is_SetRelType(X) :- is_SetRelType(X,_).

is_SetOpType_('SOT_UNION').
is_SetOpType_('SOT_DUNION').
is_SetOpType_('SOT_INTER').
is_SetOpType_('SOT_MINUS').

is_SetOpType_('SOT_UNION','SOT_UNION').
is_SetOpType_('SOT_DUNION','SOT_DUNION').
is_SetOpType_('SOT_INTER','SOT_INTER').
is_SetOpType_('SOT_MINUS','SOT_MINUS').

is_SetOpType(X,Y) :- nonvar(X), is_SetOpType_(X,Y).
is_SetOpType(X) :- is_SetOpType(X,_).

is_SetVarBranch_('SET_VAR_NONE').
is_SetVarBranch_('SET_VAR_RND').
is_SetVarBranch_('SET_VAR_DEGREE_MIN').
is_SetVarBranch_('SET_VAR_DEGREE_MAX').
is_SetVarBranch_('SET_VAR_AFC_MIN').
is_SetVarBranch_('SET_VAR_AFC_MAX').
is_SetVarBranch_('SET_VAR_MIN_MIN').
is_SetVarBranch_('SET_VAR_MIN_MAX').
is_SetVarBranch_('SET_VAR_MAX_MIN').
is_SetVarBranch_('SET_VAR_MAX_MAX').
is_SetVarBranch_('SET_VAR_SIZE_MIN').
is_SetVarBranch_('SET_VAR_SIZE_MAX').
is_SetVarBranch_('SET_VAR_SIZE_DEGREE_MIN').
is_SetVarBranch_('SET_VAR_SIZE_DEGREE_MAX').
is_SetVarBranch_('SET_VAR_SIZE_AFC_MIN').
is_SetVarBranch_('SET_VAR_SIZE_AFC_MAX').

is_SetVarBranch_('SET_VAR_NONE','SET_VAR_NONE').
is_SetVarBranch_('SET_VAR_RND','SET_VAR_RND').
is_SetVarBranch_('SET_VAR_DEGREE_MIN','SET_VAR_DEGREE_MIN').
is_SetVarBranch_('SET_VAR_DEGREE_MAX','SET_VAR_DEGREE_MAX').
is_SetVarBranch_('SET_VAR_AFC_MIN','SET_VAR_AFC_MIN').
is_SetVarBranch_('SET_VAR_AFC_MAX','SET_VAR_AFC_MAX').
is_SetVarBranch_('SET_VAR_MIN_MIN','SET_VAR_MIN_MIN').
is_SetVarBranch_('SET_VAR_MIN_MAX','SET_VAR_MIN_MAX').
is_SetVarBranch_('SET_VAR_MAX_MIN','SET_VAR_MAX_MIN').
is_SetVarBranch_('SET_VAR_MAX_MAX','SET_VAR_MAX_MAX').
is_SetVarBranch_('SET_VAR_SIZE_MIN','SET_VAR_SIZE_MIN').
is_SetVarBranch_('SET_VAR_SIZE_MAX','SET_VAR_SIZE_MAX').
is_SetVarBranch_('SET_VAR_SIZE_DEGREE_MIN','SET_VAR_SIZE_DEGREE_MIN').
is_SetVarBranch_('SET_VAR_SIZE_DEGREE_MAX','SET_VAR_SIZE_DEGREE_MAX').
is_SetVarBranch_('SET_VAR_SIZE_AFC_MIN','SET_VAR_SIZE_AFC_MIN').
is_SetVarBranch_('SET_VAR_SIZE_AFC_MAX','SET_VAR_SIZE_AFC_MAX').

is_SetVarBranch(X,Y) :- nonvar(X), is_SetVarBranch_(X,Y).
is_SetVarBranch(X) :- is_SetVarBranch(X,_).

is_SetValBranch_('SET_VAL_MIN_INC').
is_SetValBranch_('SET_VAL_MIN_EXC').
is_SetValBranch_('SET_VAL_MED_INC').
is_SetValBranch_('SET_VAL_MED_EXC').
is_SetValBranch_('SET_VAL_MAX_INC').
is_SetValBranch_('SET_VAL_MAX_EXC').
is_SetValBranch_('SET_VAL_RND_INC').
is_SetValBranch_('SET_VAL_RND_EXC').

is_SetValBranch_('SET_VAL_MIN_INC','SET_VAL_MIN_INC').
is_SetValBranch_('SET_VAL_MIN_EXC','SET_VAL_MIN_EXC').
is_SetValBranch_('SET_VAL_MED_INC','SET_VAL_MED_INC').
is_SetValBranch_('SET_VAL_MED_EXC','SET_VAL_MED_EXC').
is_SetValBranch_('SET_VAL_MAX_INC','SET_VAL_MAX_INC').
is_SetValBranch_('SET_VAL_MAX_EXC','SET_VAL_MAX_EXC').
is_SetValBranch_('SET_VAL_RND_INC','SET_VAL_RND_INC').
is_SetValBranch_('SET_VAL_RND_EXC','SET_VAL_RND_EXC').

is_SetValBranch(X,Y) :- nonvar(X), is_SetValBranch_(X,Y).
is_SetValBranch(X) :- is_SetValBranch(X,_).

is_SetAssign_('SET_ASSIGN_MIN_INC').
is_SetAssign_('SET_ASSIGN_MIN_EXC').
is_SetAssign_('SET_ASSIGN_MED_INC').
is_SetAssign_('SET_ASSIGN_MED_EXC').
is_SetAssign_('SET_ASSIGN_MAX_INC').
is_SetAssign_('SET_ASSIGN_MAX_EXC').
is_SetAssign_('SET_ASSIGN_RND_INC').
is_SetAssign_('SET_ASSIGN_RND_EXC').

is_SetAssign_('SET_ASSIGN_MIN_INC','SET_ASSIGN_MIN_INC').
is_SetAssign_('SET_ASSIGN_MIN_EXC','SET_ASSIGN_MIN_EXC').
is_SetAssign_('SET_ASSIGN_MED_INC','SET_ASSIGN_MED_INC').
is_SetAssign_('SET_ASSIGN_MED_EXC','SET_ASSIGN_MED_EXC').
is_SetAssign_('SET_ASSIGN_MAX_INC','SET_ASSIGN_MAX_INC').
is_SetAssign_('SET_ASSIGN_MAX_EXC','SET_ASSIGN_MAX_EXC').
is_SetAssign_('SET_ASSIGN_RND_INC','SET_ASSIGN_RND_INC').
is_SetAssign_('SET_ASSIGN_RND_EXC','SET_ASSIGN_RND_EXC').

is_SetAssign(X,Y) :- nonvar(X), is_SetAssign_(X,Y).
is_SetAssign(X) :- is_SetAssign(X,_).

unary(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_TaskTypeArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_BoolVarArgs(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_unary_333(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_BoolVarArgs(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_unary_329(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=1))).

sqr(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_sqr_321(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=1))).

dom(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_dom_135(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_SetVar(X1,Y1)
                 -> (is_SetRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_BoolVar(X5,Y5)
                                 -> gecode_constraint_dom_145(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=1))).

convex(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_SetVar(X2,Y2)
                 -> gecode_constraint_convex_51(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(convex(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(convex(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(convex(X0,X1,X2),arg=1))).

nooverlap(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> gecode_constraint_nooverlap_226(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=1))).

assign(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_IntAssign(X2,Y2)
                 -> gecode_constraint_assign_4(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
             ;  (is_BoolVar(X1,Y1)
                 -> (is_IntAssign(X2,Y2)
                     -> gecode_constraint_assign_3(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntAssign(X2,Y2)
                         -> gecode_constraint_assign_5(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                     ;  (is_IntVar(X1,Y1)
                         -> (is_IntAssign(X2,Y2)
                             -> gecode_constraint_assign_7(Y0,Y1,Y2)
                             ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                         ;  (is_SetVarArgs(X1,Y1)
                             -> (is_SetAssign(X2,Y2)
                                 -> gecode_constraint_assign_6(Y0,Y1,Y2)
                                 ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                             ;  (is_SetVar(X1,Y1)
                                 -> (is_SetAssign(X2,Y2)
                                     -> gecode_constraint_assign_8(Y0,Y1,Y2)
                                     ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                                 ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=2))))))))
         ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=1))).

element(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_element_154(Y0,Y1,Y2,Y3)
                     ;  (is_IntVar(X3,Y3)
                         -> gecode_constraint_element_158(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_element_148(Y0,Y1,Y2,Y3)
                         ;  (is_BoolVar(X3,Y3)
                             -> gecode_constraint_element_146(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4))))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                 ;  (is_IntSetArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_SetVar(X3,Y3)
                             -> gecode_constraint_element_153(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_SetVar(X3,Y3)
                                 -> gecode_constraint_element_161(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                         ;  (is_IntArgs(X1,Y1)
                             -> (is_IntVar(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> gecode_constraint_element_164(Y0,Y1,Y2,Y3)
                                     ;  (is_IntVar(X3,Y3)
                                         -> gecode_constraint_element_170(Y0,Y1,Y2,Y3)
                                         ;  (is_BoolVar(X3,Y3)
                                             -> gecode_constraint_element_162(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4)))))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=2)))))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=1))).

sequence(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> gecode_constraint_sequence_314(Y0,Y1)
             ;  throw(gecode_argument_error(sequence(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(sequence(X0,X1),arg=1))).

notMax(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_notMax_232(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(notMax(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(notMax(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(notMax(X0,X1,X2),arg=1))).

unary(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> gecode_constraint_unary_326(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(unary(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(unary(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(unary(X0,X1,X2),arg=1))).

circuit(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_circuit_45(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> gecode_constraint_circuit_36(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=1))).

dom(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> gecode_constraint_dom_134(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_dom_139(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5))))
                     ;  (is_BoolVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_dom_133(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4))))
                 ;  (is_IntSet(X2,Y2)
                     -> (is_BoolVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_dom_129(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_dom_127(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_SetVar(X1,Y1)
                     -> (is_SetRelType(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_int(X4,Y4)
                                 -> gecode_constraint_dom_144(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_BoolVar(X4,Y4)
                                     -> gecode_constraint_dom_143(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5))))
                             ;  (is_IntSet(X3,Y3)
                                 -> (is_BoolVar(X4,Y4)
                                     -> gecode_constraint_dom_141(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4))))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=2)))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=1))).

channel(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_BoolVar(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_channel_32(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_channel_27(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=3)))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> gecode_constraint_channel_23(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=3)))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_IntConLevel(X3,Y3)
                                 -> gecode_constraint_channel_21(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=3)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=2))))))
         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_BoolVarArgs(X7,Y7)
                                     -> gecode_constraint_nooverlap_228(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_nooverlap_231(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=8))))
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

element(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> gecode_constraint_element_156(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> (is_BoolVar(X6,Y6)
                                     -> gecode_constraint_element_150(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                 ;  (is_IntSetArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> (is_int(X5,Y5)
                                     -> (is_SetVar(X6,Y6)
                                         -> gecode_constraint_element_152(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> (is_IntVar(X4,Y4)
                                     -> (is_int(X5,Y5)
                                         -> (is_SetVar(X6,Y6)
                                             -> gecode_constraint_element_160(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                         ;  (is_IntArgs(X1,Y1)
                             -> (is_IntVar(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> (is_IntVar(X4,Y4)
                                         -> (is_int(X5,Y5)
                                             -> (is_IntVar(X6,Y6)
                                                 -> gecode_constraint_element_168(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                                 ;  (is_BoolVar(X6,Y6)
                                                     -> gecode_constraint_element_166(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=2)))))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=1))).

max(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_max_208(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(max(X0,X1,X2),arg=3)))
             ;  (is_SetVar(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> gecode_constraint_max_212(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(max(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(max(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(max(X0,X1,X2),arg=1))).

unshare(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> gecode_constraint_unshare_338(Y0,Y1)
             ;  (is_BoolVarArgs(X1,Y1)
                 -> gecode_constraint_unshare_336(Y0,Y1)
                 ;  throw(gecode_argument_error(unshare(X0,X1),arg=2))))
         ;  throw(gecode_argument_error(unshare(X0,X1),arg=1))).

path(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_path_244(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_path_243(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=1))).

mult(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_mult_222(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=1))).

clause(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_clause_49(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  (is_BoolVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_clause_47(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=1))).

precede(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_precede_249(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=1))).

distinct(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> gecode_constraint_distinct_116(Y0,Y1)
             ;  throw(gecode_argument_error(distinct(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(distinct(X0,X1),arg=1))).

mod(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_mod_221(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=1))).

cardinality(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_cardinality_18(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(cardinality(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(cardinality(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(cardinality(X0,X1,X2),arg=1))).

atmostOne(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> gecode_constraint_atmostOne_9(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(atmostOne(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(atmostOne(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(atmostOne(X0,X1,X2),arg=1))).

channelSorted(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_SetVar(X2,Y2)
                 -> gecode_constraint_channelSorted_33(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(channelSorted(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(channelSorted(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(channelSorted(X0,X1,X2),arg=1))).

linear(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_linear_202(Y0,Y1,Y2,Y3)
                     ;  (is_IntVar(X3,Y3)
                         -> gecode_constraint_linear_206(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_linear_178(Y0,Y1,Y2,Y3)
                         ;  (is_IntVar(X3,Y3)
                             -> gecode_constraint_linear_182(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=4))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=1))).

circuit(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> gecode_constraint_circuit_42(Y0,Y1)
             ;  throw(gecode_argument_error(circuit(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1),arg=1))).

rel(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> gecode_constraint_rel_264(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_rel_267(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                     ;  (is_BoolVar(X3,Y3)
                         -> (is_BoolVar(X4,Y4)
                             -> gecode_constraint_rel_260(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_rel_263(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                 ;  (is_BoolOpType(X2,Y2)
                     -> (is_BoolVar(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> gecode_constraint_rel_258(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_BoolVar(X4,Y4)
                                 -> gecode_constraint_rel_256(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_rel_275(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                         ;  (is_BoolVarArgs(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_rel_271(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                             ;  (is_BoolVar(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_rel_269(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_SetOpType(X1,Y1)
                     -> (is_SetVarArgs(X2,Y2)
                         -> (is_IntSet(X3,Y3)
                             -> (is_SetVar(X4,Y4)
                                 -> gecode_constraint_rel_299(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                         ;  (is_IntVarArgs(X2,Y2)
                             -> (is_IntSet(X3,Y3)
                                 -> (is_SetVar(X4,Y4)
                                     -> gecode_constraint_rel_297(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                     ;  (is_IntVarArgs(X1,Y1)
                         -> (is_IntRelType(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_rel_283(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                 ;  (is_IntVar(X3,Y3)
                                     -> (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_rel_285(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                     ;  (is_IntVarArgs(X3,Y3)
                                         -> (is_IntConLevel(X4,Y4)
                                             -> gecode_constraint_rel_279(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                         ;  (is_IntVar(X1,Y1)
                             -> (is_IntRelType(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> (is_BoolVar(X4,Y4)
                                         -> gecode_constraint_rel_286(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_IntConLevel(X4,Y4)
                                             -> gecode_constraint_rel_289(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                     ;  (is_IntVar(X3,Y3)
                                         -> (is_BoolVar(X4,Y4)
                                             -> gecode_constraint_rel_290(Y0,Y1,Y2,Y3,Y4)
                                             ;  (is_IntConLevel(X4,Y4)
                                                 -> gecode_constraint_rel_293(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                                 ;  (is_SetRelType(X2,Y2)
                                     -> (is_SetVar(X3,Y3)
                                         -> (is_BoolVar(X4,Y4)
                                             -> gecode_constraint_rel_296(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                             ;  (is_SetVar(X1,Y1)
                                 -> (is_SetRelType(X2,Y2)
                                     -> (is_IntVar(X3,Y3)
                                         -> (is_BoolVar(X4,Y4)
                                             -> gecode_constraint_rel_307(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                         ;  (is_SetVar(X3,Y3)
                                             -> (is_BoolVar(X4,Y4)
                                                 -> gecode_constraint_rel_309(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                                 ;  (is_BoolOpType(X1,Y1)
                                     -> (is_BoolVarArgs(X2,Y2)
                                         -> (is_int(X3,Y3)
                                             -> (is_IntConLevel(X4,Y4)
                                                 -> gecode_constraint_rel_255(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                             ;  (is_BoolVar(X3,Y3)
                                                 -> (is_IntConLevel(X4,Y4)
                                                     -> gecode_constraint_rel_253(Y0,Y1,Y2,Y3,Y4)
                                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=2)))))))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=1))).

min(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_min_216(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_min_215(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=3)))
                 ;  (is_SetVar(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_BoolVar(X3,Y3)
                             -> gecode_constraint_min_219(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=3)))
                     ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=2)))))
         ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=1))).

cardinality(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_cardinality_19(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=1))).

count(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_count_65(Y0,Y1,Y2,Y3)
                     ;  (is_IntArgs(X3,Y3)
                         -> gecode_constraint_count_62(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=4))))
                 ;  (is_IntSet(X2,Y2)
                     -> (is_IntArgs(X3,Y3)
                         -> gecode_constraint_count_60(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=4)))
                     ;  (is_IntSetArgs(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_count_59(Y0,Y1,Y2,Y3)
                             ;  (is_IntArgs(X3,Y3)
                                 -> gecode_constraint_count_56(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=4))))
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=3)))))
             ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=1))).

sqrt(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_sqrt_322(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sqrt(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(sqrt(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(sqrt(X0,X1,X2),arg=1))).

cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntArgs(X6,Y6)
                                 -> (is_bool(X7,Y7)
                                     -> (is_IntConLevel(X8,Y8)
                                         -> gecode_constraint_cumulatives_113(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                             ;  (is_IntArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> (is_IntConLevel(X8,Y8)
                                             -> gecode_constraint_cumulatives_111(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                     ;  (is_IntArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> (is_IntConLevel(X8,Y8)
                                             -> gecode_constraint_cumulatives_109(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> (is_IntConLevel(X8,Y8)
                                                 -> gecode_constraint_cumulatives_107(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4))))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> (is_IntConLevel(X8,Y8)
                                             -> gecode_constraint_cumulatives_105(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> (is_IntConLevel(X8,Y8)
                                                 -> gecode_constraint_cumulatives_103(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntVarArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> (is_IntConLevel(X8,Y8)
                                                 -> gecode_constraint_cumulatives_101(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                     ;  (is_IntArgs(X5,Y5)
                                         -> (is_IntArgs(X6,Y6)
                                             -> (is_bool(X7,Y7)
                                                 -> (is_IntConLevel(X8,Y8)
                                                     -> gecode_constraint_cumulatives_99(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4))))
                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=2))))
         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=1))).

binpacking(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> gecode_constraint_binpacking_10(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=1))).

linear(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntRelType(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_BoolVar(X5,Y5)
                             -> (is_IntConLevel(X6,Y6)
                                 -> gecode_constraint_linear_193(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  (is_IntVar(X4,Y4)
                             -> (is_BoolVar(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_linear_197(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_BoolVarArgs(X2,Y2)
                     -> (is_IntRelType(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_BoolVar(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_linear_185(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  (is_IntVar(X4,Y4)
                                 -> (is_BoolVar(X5,Y5)
                                     -> (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_linear_189(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=1))).

abs(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_abs_2(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=1))).

convex(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> gecode_constraint_convex_50(Y0,Y1)
             ;  throw(gecode_argument_error(convex(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(convex(X0,X1),arg=1))).

div(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_div_118(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=1))).

rel(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_rel_287(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_BoolVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_rel_291(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_IntSet(X1,Y1)
                 -> (is_SetOpType(X2,Y2)
                     -> (is_SetVar(X3,Y3)
                         -> (is_SetRelType(X4,Y4)
                             -> (is_IntSet(X5,Y5)
                                 -> gecode_constraint_rel_276(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  (is_SetVar(X5,Y5)
                                     -> gecode_constraint_rel_277(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  (is_BoolVar(X1,Y1)
                     -> (is_IntRelType(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_BoolVar(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_rel_265(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  (is_BoolVar(X3,Y3)
                                 -> (is_BoolVar(X4,Y4)
                                     -> (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_rel_261(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4))))
                         ;  (is_BoolOpType(X2,Y2)
                             -> (is_BoolVar(X3,Y3)
                                 -> (is_int(X4,Y4)
                                     -> (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_rel_259(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  (is_BoolVar(X4,Y4)
                                         -> (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_rel_257(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3))))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_SetOpType(X2,Y2)
                             -> (is_IntSet(X3,Y3)
                                 -> (is_SetRelType(X4,Y4)
                                     -> (is_IntSet(X5,Y5)
                                         -> gecode_constraint_rel_302(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_SetVar(X5,Y5)
                                             -> gecode_constraint_rel_303(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                                 ;  (is_SetVar(X3,Y3)
                                     -> (is_SetRelType(X4,Y4)
                                         -> (is_IntSet(X5,Y5)
                                             -> gecode_constraint_rel_304(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  (is_SetVar(X5,Y5)
                                                 -> gecode_constraint_rel_305(Y0,Y1,Y2,Y3,Y4,Y5)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4))))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=2))))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=1))).

weights(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_SetVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_weights_340(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=1))).

max(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_max_211(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=1))).

path(X0,X1,X2,X3,X4,X5,X6,X7,X8) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_IntVar(X7,Y7)
                                     -> (is_IntConLevel(X8,Y8)
                                         -> gecode_constraint_path_239(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=1))).

unary(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_TaskTypeArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> gecode_constraint_unary_334(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> gecode_constraint_unary_330(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=4)))
                     ;  (is_IntArgs(X2,Y2)
                         -> (is_BoolVarArgs(X3,Y3)
                             -> gecode_constraint_unary_324(Y0,Y1,Y2,Y3)
                             ;  (is_IntConLevel(X3,Y3)
                                 -> gecode_constraint_unary_327(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=4))))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=3))))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=1))).

sorted(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_sorted_317(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=1))).

circuit(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_circuit_40(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_circuit_37(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=5)))
                         ;  (is_IntVarArgs(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> gecode_constraint_circuit_34(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=3))))
             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=1))).

dom(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_dom_138(Y0,Y1,Y2,Y3)
                     ;  (is_BoolVar(X3,Y3)
                         -> gecode_constraint_dom_132(Y0,Y1,Y2,Y3)
                         ;  (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_dom_137(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))))
                 ;  (is_IntSet(X2,Y2)
                     -> (is_BoolVar(X3,Y3)
                         -> gecode_constraint_dom_128(Y0,Y1,Y2,Y3)
                         ;  (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_dom_131(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4))))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_dom_126(Y0,Y1,Y2,Y3)
                         ;  (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_dom_125(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4))))
                     ;  (is_IntSet(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_dom_123(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3))))
                 ;  (is_SetVar(X1,Y1)
                     -> (is_SetRelType(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> gecode_constraint_dom_142(Y0,Y1,Y2,Y3)
                             ;  (is_IntSet(X3,Y3)
                                 -> gecode_constraint_dom_140(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4))))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=2)))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=1))).

abs(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_abs_1(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(abs(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(abs(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(abs(X0,X1,X2),arg=1))).

channel(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> gecode_constraint_channel_29(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_channel_24(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=1))).

rel(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> gecode_constraint_rel_280(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> gecode_constraint_rel_272(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=1))).

path(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_path_242(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=1))).

branch(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarBranch(X2,Y2)
                 -> (is_IntValBranch(X3,Y3)
                     -> gecode_constraint_branch_14(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVarBranch(X2,Y2)
                     -> (is_IntValBranch(X3,Y3)
                         -> gecode_constraint_branch_13(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                 ;  (is_SetVarArgs(X1,Y1)
                     -> (is_SetVarBranch(X2,Y2)
                         -> (is_SetValBranch(X3,Y3)
                             -> gecode_constraint_branch_15(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=2)))))
         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=1))).

mult(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_mult_223(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=1))).

circuit(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_circuit_41(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  (is_IntVarArgs(X4,Y4)
                             -> (is_IntVar(X5,Y5)
                                 -> gecode_constraint_circuit_38(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_circuit_35(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=1))).

clause(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> gecode_constraint_clause_48(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_BoolVar(X4,Y4)
                             -> gecode_constraint_clause_46(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=1))).

precede(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_precede_251(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_precede_248(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=4)))
                     ;  (is_IntArgs(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_precede_247(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=3))))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=1))).

channel(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_channel_30(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=1))).

cumulative(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_TaskTypeArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_IntArgs(X5,Y5)
                             -> (is_BoolVarArgs(X6,Y6)
                                 -> gecode_constraint_cumulative_82(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_cumulative_85(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> gecode_constraint_cumulative_78(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_cumulative_81(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntArgs(X4,Y4)
                                 -> (is_BoolVarArgs(X5,Y5)
                                     -> (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_cumulative_75(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  (is_IntVar(X1,Y1)
                 -> (is_TaskTypeArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> gecode_constraint_cumulative_94(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_cumulative_97(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_IntVarArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntArgs(X5,Y5)
                                     -> (is_BoolVarArgs(X6,Y6)
                                         -> gecode_constraint_cumulative_90(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  (is_IntConLevel(X6,Y6)
                                             -> gecode_constraint_cumulative_93(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                             ;  (is_IntArgs(X3,Y3)
                                 -> (is_IntArgs(X4,Y4)
                                     -> (is_BoolVarArgs(X5,Y5)
                                         -> (is_IntConLevel(X6,Y6)
                                             -> gecode_constraint_cumulative_87(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=3))))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=1))).

distinct(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntConLevel(X2,Y2)
                 -> gecode_constraint_distinct_117(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> gecode_constraint_distinct_114(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=1))).

mod(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_mod_220(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=1))).

sqr(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_sqr_320(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sqr(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(sqr(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(sqr(X0,X1,X2),arg=1))).

sequence(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntSet(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> (is_IntConLevel(X6,Y6)
                                 -> gecode_constraint_sequence_313(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntSet(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_sequence_311(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=2))))
         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=1))).

path(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> gecode_constraint_path_240(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntVar(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_path_237(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  (is_IntVarArgs(X5,Y5)
                                     -> (is_IntVar(X6,Y6)
                                         -> gecode_constraint_path_234(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=1))).

divmod(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_divmod_121(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=1))).

sorted(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> gecode_constraint_sorted_318(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sorted(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(sorted(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(sorted(X0,X1,X2),arg=1))).

circuit(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> gecode_constraint_circuit_44(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntConLevel(X2,Y2)
                     -> gecode_constraint_circuit_43(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=1))).

channel(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_BoolVar(X2,Y2)
                 -> gecode_constraint_channel_31(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_SetVarArgs(X2,Y2)
                     -> gecode_constraint_channel_28(Y0,Y1,Y2)
                     ;  (is_IntVarArgs(X2,Y2)
                         -> gecode_constraint_channel_26(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3))))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> gecode_constraint_channel_22(Y0,Y1,Y2)
                         ;  (is_SetVar(X2,Y2)
                             -> gecode_constraint_channel_25(Y0,Y1,Y2)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3))))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> gecode_constraint_channel_20(Y0,Y1,Y2)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=2))))))
         ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=1))).

count(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntRelType(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> gecode_constraint_count_52(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntVar(X4,Y4)
                             -> gecode_constraint_count_54(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                 ;  (is_int(X2,Y2)
                     -> (is_IntRelType(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> gecode_constraint_count_66(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntVar(X4,Y4)
                                 -> gecode_constraint_count_68(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                     ;  (is_IntSet(X2,Y2)
                         -> (is_IntArgs(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_count_61(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                         ;  (is_IntSetArgs(X2,Y2)
                             -> (is_IntArgs(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_count_57(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                             ;  (is_IntVarArgs(X2,Y2)
                                 -> (is_IntArgs(X3,Y3)
                                     -> (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_count_63(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5)))
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                                 ;  (is_IntVar(X2,Y2)
                                     -> (is_IntRelType(X3,Y3)
                                         -> (is_int(X4,Y4)
                                             -> gecode_constraint_count_70(Y0,Y1,Y2,Y3,Y4)
                                             ;  (is_IntVar(X4,Y4)
                                                 -> gecode_constraint_count_72(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5))))
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=3))))))))
             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=1))).

cumulatives(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntArgs(X6,Y6)
                                 -> (is_bool(X7,Y7)
                                     -> gecode_constraint_cumulatives_112(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  (is_IntArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> gecode_constraint_cumulatives_110(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  (is_IntArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> gecode_constraint_cumulatives_108(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> gecode_constraint_cumulatives_106(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=4))))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> gecode_constraint_cumulatives_104(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> gecode_constraint_cumulatives_102(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntVarArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> gecode_constraint_cumulatives_100(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  (is_IntArgs(X5,Y5)
                                         -> (is_IntArgs(X6,Y6)
                                             -> (is_bool(X7,Y7)
                                                 -> gecode_constraint_cumulatives_98(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=4))))
                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=2))))
         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

binpacking(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_binpacking_11(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=1))).

linear(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_linear_201(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_BoolVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_linear_205(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_BoolVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_linear_177(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  (is_IntVar(X3,Y3)
                             -> (is_BoolVar(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_linear_181(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  (is_IntArgs(X1,Y1)
                     -> (is_IntVarArgs(X2,Y2)
                         -> (is_IntRelType(X3,Y3)
                             -> (is_int(X4,Y4)
                                 -> (is_BoolVar(X5,Y5)
                                     -> gecode_constraint_linear_192(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_linear_195(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  (is_IntVar(X4,Y4)
                                     -> (is_BoolVar(X5,Y5)
                                         -> gecode_constraint_linear_196(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_linear_199(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4)))
                         ;  (is_BoolVarArgs(X2,Y2)
                             -> (is_IntRelType(X3,Y3)
                                 -> (is_int(X4,Y4)
                                     -> (is_BoolVar(X5,Y5)
                                         -> gecode_constraint_linear_184(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_linear_187(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  (is_IntVar(X4,Y4)
                                         -> (is_BoolVar(X5,Y5)
                                             -> gecode_constraint_linear_188(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  (is_IntConLevel(X5,Y5)
                                                 -> gecode_constraint_linear_191(Y0,Y1,Y2,Y3,Y4,Y5)
                                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=2)))))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> gecode_constraint_nooverlap_230(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_IntArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_BoolVarArgs(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_nooverlap_225(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=1))).

div(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_div_119(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=1))).

max(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_max_210(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_max_209(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=3)))
                 ;  (is_SetVar(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_BoolVar(X3,Y3)
                             -> gecode_constraint_max_213(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=3)))
                     ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=2)))))
         ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=1))).

path(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> (is_IntConLevel(X7,Y7)
                                     -> gecode_constraint_path_241(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  (is_IntVarArgs(X6,Y6)
                                     -> (is_IntVar(X7,Y7)
                                         -> gecode_constraint_path_238(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=7))))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntVar(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_path_235(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=3))))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

unary(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_TaskTypeArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_BoolVarArgs(X4,Y4)
                         -> gecode_constraint_unary_332(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_unary_335(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_BoolVarArgs(X4,Y4)
                             -> gecode_constraint_unary_328(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_unary_331(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=4)))
                     ;  (is_IntArgs(X2,Y2)
                         -> (is_BoolVarArgs(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_unary_325(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=4)))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=3))))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=1))).

sorted(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> gecode_constraint_sorted_316(Y0,Y1,Y2,Y3)
                     ;  (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_sorted_319(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=1))).

element(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> (is_IntConLevel(X7,Y7)
                                     -> gecode_constraint_element_157(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> (is_BoolVar(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_element_151(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
                 ;  (is_IntArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> (is_int(X5,Y5)
                                     -> (is_IntVar(X6,Y6)
                                         -> (is_IntConLevel(X7,Y7)
                                             -> gecode_constraint_element_169(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  (is_BoolVar(X6,Y6)
                                             -> (is_IntConLevel(X7,Y7)
                                                 -> gecode_constraint_element_167(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=7))))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=2)))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

element(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_element_155(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_element_159(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_element_149(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                         ;  (is_BoolVar(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_element_147(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_SetOpType(X1,Y1)
                     -> (is_SetVarArgs(X2,Y2)
                         -> (is_SetVar(X3,Y3)
                             -> (is_SetVar(X4,Y4)
                                 -> gecode_constraint_element_174(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))
                         ;  (is_IntSetArgs(X2,Y2)
                             -> (is_SetVar(X3,Y3)
                                 -> (is_SetVar(X4,Y4)
                                     -> gecode_constraint_element_172(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3))))
                     ;  (is_IntArgs(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_element_165(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                 ;  (is_IntVar(X3,Y3)
                                     -> (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_element_171(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                     ;  (is_BoolVar(X3,Y3)
                                         -> (is_IntConLevel(X4,Y4)
                                             -> gecode_constraint_element_163(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=2))))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=1))).

sequence(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_SetVar(X2,Y2)
                 -> gecode_constraint_sequence_315(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(sequence(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(sequence(X0,X1,X2),arg=1))).

circuit(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntConLevel(X6,Y6)
                                 -> gecode_constraint_circuit_39(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=3)))
             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=1))).

precede(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> gecode_constraint_precede_250(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntArgs(X2,Y2)
                     -> gecode_constraint_precede_246(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=1))).

cumulative(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_TaskTypeArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_IntArgs(X5,Y5)
                             -> gecode_constraint_cumulative_84(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> gecode_constraint_cumulative_80(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntArgs(X4,Y4)
                                 -> (is_BoolVarArgs(X5,Y5)
                                     -> gecode_constraint_cumulative_74(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_cumulative_77(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  (is_IntVar(X1,Y1)
                 -> (is_TaskTypeArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> gecode_constraint_cumulative_96(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_IntVarArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntArgs(X5,Y5)
                                     -> gecode_constraint_cumulative_92(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  (is_IntArgs(X3,Y3)
                                 -> (is_IntArgs(X4,Y4)
                                     -> (is_BoolVarArgs(X5,Y5)
                                         -> gecode_constraint_cumulative_86(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_cumulative_89(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4))))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=3))))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=1))).

distinct(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_distinct_115(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=1))).

min(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_min_214(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(min(X0,X1,X2),arg=3)))
             ;  (is_SetVar(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> gecode_constraint_min_218(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(min(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(min(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(min(X0,X1,X2),arg=1))).

sqrt(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_sqrt_323(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=1))).

sequence(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntSet(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> gecode_constraint_sequence_312(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntSet(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> gecode_constraint_sequence_310(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=1))).

unshare(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntConLevel(X2,Y2)
                 -> gecode_constraint_unshare_339(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntConLevel(X2,Y2)
                     -> gecode_constraint_unshare_337(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=1))).

path(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_path_245(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntVar(X5,Y5)
                                 -> gecode_constraint_path_236(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=1))).

divmod(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_divmod_120(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_BoolVarArgs(X7,Y7)
                                     -> (is_IntConLevel(X8,Y8)
                                         -> gecode_constraint_nooverlap_229(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=1))).

cumulative(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> gecode_constraint_cumulative_76(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_IntVar(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> gecode_constraint_cumulative_88(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=1))).

count(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntRelType(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_count_67(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  (is_IntVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_count_69(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntVar(X2,Y2)
                     -> (is_IntRelType(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_count_71(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  (is_IntVar(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_count_73(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  (is_IntArgs(X2,Y2)
                         -> (is_IntRelType(X3,Y3)
                             -> (is_int(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_count_53(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  (is_IntVar(X4,Y4)
                                     -> (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_count_55(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=4)))
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=3)))))
             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=1))).

notMin(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_notMin_233(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(notMin(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(notMin(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(notMin(X0,X1,X2),arg=1))).

cumulative(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_TaskTypeArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_IntArgs(X5,Y5)
                             -> (is_BoolVarArgs(X6,Y6)
                                 -> (is_IntConLevel(X7,Y7)
                                     -> gecode_constraint_cumulative_83(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_cumulative_79(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=3))))
             ;  (is_IntVar(X1,Y1)
                 -> (is_TaskTypeArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_cumulative_95(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_IntVarArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntArgs(X5,Y5)
                                     -> (is_BoolVarArgs(X6,Y6)
                                         -> (is_IntConLevel(X7,Y7)
                                             -> gecode_constraint_cumulative_91(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=3))))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

branch(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntValBranch(X2,Y2)
                 -> gecode_constraint_branch_16(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=3)))
             ;  (is_BoolVar(X1,Y1)
                 -> (is_IntValBranch(X2,Y2)
                     -> gecode_constraint_branch_12(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=3)))
                 ;  (is_SetVar(X1,Y1)
                     -> (is_SetValBranch(X2,Y2)
                         -> gecode_constraint_branch_17(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=3)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=2)))))
         ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=1))).

dom(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> gecode_constraint_dom_136(Y0,Y1,Y2)
                 ;  (is_IntSet(X2,Y2)
                     -> gecode_constraint_dom_130(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> gecode_constraint_dom_124(Y0,Y1,Y2)
                     ;  (is_IntSet(X2,Y2)
                         -> gecode_constraint_dom_122(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3))))
                 ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=1))).

linear(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> gecode_constraint_linear_200(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_linear_203(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_BoolVar(X4,Y4)
                             -> gecode_constraint_linear_204(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_linear_207(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_BoolVar(X4,Y4)
                             -> gecode_constraint_linear_176(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_linear_179(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                         ;  (is_IntVar(X3,Y3)
                             -> (is_BoolVar(X4,Y4)
                                 -> gecode_constraint_linear_180(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_linear_183(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_IntArgs(X1,Y1)
                     -> (is_IntVarArgs(X2,Y2)
                         -> (is_IntRelType(X3,Y3)
                             -> (is_int(X4,Y4)
                                 -> gecode_constraint_linear_194(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_IntVar(X4,Y4)
                                     -> gecode_constraint_linear_198(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))
                         ;  (is_BoolVarArgs(X2,Y2)
                             -> (is_IntRelType(X3,Y3)
                                 -> (is_int(X4,Y4)
                                     -> gecode_constraint_linear_186(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_IntVar(X4,Y4)
                                         -> gecode_constraint_linear_190(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=2)))))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_BoolVarArgs(X5,Y5)
                             -> gecode_constraint_nooverlap_224(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_nooverlap_227(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=1))).

element(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetOpType(X1,Y1)
             -> (is_SetVarArgs(X2,Y2)
                 -> (is_SetVar(X3,Y3)
                     -> (is_SetVar(X4,Y4)
                         -> (is_IntSet(X5,Y5)
                             -> gecode_constraint_element_175(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntSetArgs(X2,Y2)
                     -> (is_SetVar(X3,Y3)
                         -> (is_SetVar(X4,Y4)
                             -> (is_IntSet(X5,Y5)
                                 -> gecode_constraint_element_173(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=1))).

rel(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_rel_266(Y0,Y1,Y2,Y3)
                     ;  (is_BoolVar(X3,Y3)
                         -> gecode_constraint_rel_262(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_rel_274(Y0,Y1,Y2,Y3)
                         ;  (is_BoolVarArgs(X3,Y3)
                             -> gecode_constraint_rel_270(Y0,Y1,Y2,Y3)
                             ;  (is_BoolVar(X3,Y3)
                                 -> gecode_constraint_rel_268(Y0,Y1,Y2,Y3)
                                 ;  (is_IntConLevel(X3,Y3)
                                     -> gecode_constraint_rel_273(Y0,Y1,Y2,Y3)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))))
                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                 ;  (is_SetOpType(X1,Y1)
                     -> (is_SetVarArgs(X2,Y2)
                         -> (is_SetVar(X3,Y3)
                             -> gecode_constraint_rel_300(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                         ;  (is_IntVarArgs(X2,Y2)
                             -> (is_SetVar(X3,Y3)
                                 -> gecode_constraint_rel_298(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                     ;  (is_IntVarArgs(X1,Y1)
                         -> (is_IntRelType(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> gecode_constraint_rel_282(Y0,Y1,Y2,Y3)
                                 ;  (is_IntVar(X3,Y3)
                                     -> gecode_constraint_rel_284(Y0,Y1,Y2,Y3)
                                     ;  (is_IntVarArgs(X3,Y3)
                                         -> gecode_constraint_rel_278(Y0,Y1,Y2,Y3)
                                         ;  (is_IntConLevel(X3,Y3)
                                             -> gecode_constraint_rel_281(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                         ;  (is_IntVar(X1,Y1)
                             -> (is_IntRelType(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> gecode_constraint_rel_288(Y0,Y1,Y2,Y3)
                                     ;  (is_IntVar(X3,Y3)
                                         -> gecode_constraint_rel_292(Y0,Y1,Y2,Y3)
                                         ;  (is_SetVar(X3,Y3)
                                             -> gecode_constraint_rel_294(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                                 ;  (is_SetRelType(X2,Y2)
                                     -> (is_SetVar(X3,Y3)
                                         -> gecode_constraint_rel_295(Y0,Y1,Y2,Y3)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                             ;  (is_SetVar(X1,Y1)
                                 -> (is_IntRelType(X2,Y2)
                                     -> (is_IntVar(X3,Y3)
                                         -> gecode_constraint_rel_301(Y0,Y1,Y2,Y3)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                                     ;  (is_SetRelType(X2,Y2)
                                         -> (is_IntVar(X3,Y3)
                                             -> gecode_constraint_rel_306(Y0,Y1,Y2,Y3)
                                             ;  (is_SetVar(X3,Y3)
                                                 -> gecode_constraint_rel_308(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                                 ;  (is_BoolOpType(X1,Y1)
                                     -> (is_BoolVarArgs(X2,Y2)
                                         -> (is_int(X3,Y3)
                                             -> gecode_constraint_rel_254(Y0,Y1,Y2,Y3)
                                             ;  (is_BoolVar(X3,Y3)
                                                 -> gecode_constraint_rel_252(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=2)))))))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=1))).

min(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_min_217(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=1))).

count(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> gecode_constraint_count_64(Y0,Y1,Y2)
                 ;  (is_IntSetArgs(X2,Y2)
                     -> gecode_constraint_count_58(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(count(X0,X1,X2),arg=3))))
             ;  throw(gecode_argument_error(count(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2),arg=1))).

