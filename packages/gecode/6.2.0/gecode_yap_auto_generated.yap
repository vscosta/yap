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

is_RestartMode_('RM_NONE').
is_RestartMode_('RM_CONSTANT').
is_RestartMode_('RM_LINEAR').
is_RestartMode_('RM_LUBY').
is_RestartMode_('RM_GEOMETRIC').


is_RestartMode_('RM_NONE','RM_NONE').
is_RestartMode_('RM_CONSTANT','RM_CONSTANT').
is_RestartMode_('RM_LINEAR','RM_LINEAR').
is_RestartMode_('RM_LUBY','RM_LUBY').
is_RestartMode_('RM_GEOMETRIC','RM_GEOMETRIC').


is_RestartMode(X,Y) :- nonvar(X), is_RestartMode_(X,Y).
is_RestartMode(X) :- is_RestartMode_(X,_).


is_FloatRelType_('FRT_EQ').
is_FloatRelType_('FRT_NQ').
is_FloatRelType_('FRT_LQ').
is_FloatRelType_('FRT_LE').
is_FloatRelType_('FRT_GQ').
is_FloatRelType_('FRT_GR').


is_FloatRelType_('FRT_EQ','FRT_EQ').
is_FloatRelType_('FRT_NQ','FRT_NQ').
is_FloatRelType_('FRT_LQ','FRT_LQ').
is_FloatRelType_('FRT_LE','FRT_LE').
is_FloatRelType_('FRT_GQ','FRT_GQ').
is_FloatRelType_('FRT_GR','FRT_GR').


is_FloatRelType(X,Y) :- nonvar(X), is_FloatRelType_(X,Y).
is_FloatRelType(X) :- is_FloatRelType_(X,_).


is_ReifyMode_('RM_EQV').
is_ReifyMode_('RM_IMP').
is_ReifyMode_('RM_PMI').


is_ReifyMode_('RM_EQV','RM_EQV').
is_ReifyMode_('RM_IMP','RM_IMP').
is_ReifyMode_('RM_PMI','RM_PMI').


is_ReifyMode(X,Y) :- nonvar(X), is_ReifyMode_(X,Y).
is_ReifyMode(X) :- is_ReifyMode_(X,_).


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
is_IntRelType(X) :- is_IntRelType_(X,_).


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
is_BoolOpType(X) :- is_BoolOpType_(X,_).


is_IntPropLevel_('IPL_DEF').
is_IntPropLevel_('IPL_VAL').
is_IntPropLevel_('IPL_BND').
is_IntPropLevel_('IPL_DOM').
is_IntPropLevel_('IPL_BASIC').
is_IntPropLevel_('IPL_ADVANCED').
is_IntPropLevel_('IPL_BASIC_ADVANCED').
is_IntPropLevel_('_IPL_BITS').


is_IntPropLevel_('IPL_DEF','IPL_DEF').
is_IntPropLevel_('IPL_VAL','IPL_VAL').
is_IntPropLevel_('IPL_BND','IPL_BND').
is_IntPropLevel_('IPL_DOM','IPL_DOM').
is_IntPropLevel_('IPL_BASIC','IPL_BASIC').
is_IntPropLevel_('IPL_ADVANCED','IPL_ADVANCED').
is_IntPropLevel_('IPL_BASIC_ADVANCED','IPL_BASIC_ADVANCED').
is_IntPropLevel_('_IPL_BITS','_IPL_BITS').


is_IntPropLevel(X,Y) :- nonvar(X), is_IntPropLevel_(X,Y).
is_IntPropLevel(X) :- is_IntPropLevel_(X,_).


is_TaskType_('TT_FIXP').
is_TaskType_('TT_FIXS').
is_TaskType_('TT_FIXE').


is_TaskType_('TT_FIXP','TT_FIXP').
is_TaskType_('TT_FIXS','TT_FIXS').
is_TaskType_('TT_FIXE','TT_FIXE').


is_TaskType(X,Y) :- nonvar(X), is_TaskType_(X,Y).
is_TaskType(X) :- is_TaskType_(X,_).


is_TraceEvent_('TE_INIT').
is_TraceEvent_('TE_PRUNE').
is_TraceEvent_('TE_FIX').
is_TraceEvent_('TE_FAIL').
is_TraceEvent_('TE_DONE').
is_TraceEvent_('TE_PROPAGATE').
is_TraceEvent_('TE_COMMIT').
is_TraceEvent_('TE_POST').


is_TraceEvent_('TE_INIT','TE_INIT').
is_TraceEvent_('TE_PRUNE','TE_PRUNE').
is_TraceEvent_('TE_FIX','TE_FIX').
is_TraceEvent_('TE_FAIL','TE_FAIL').
is_TraceEvent_('TE_DONE','TE_DONE').
is_TraceEvent_('TE_PROPAGATE','TE_PROPAGATE').
is_TraceEvent_('TE_COMMIT','TE_COMMIT').
is_TraceEvent_('TE_POST','TE_POST').


is_TraceEvent(X,Y) :- nonvar(X), is_TraceEvent_(X,Y).
is_TraceEvent(X) :- is_TraceEvent_(X,_).


is_SetRelType_('SRT_EQ').
is_SetRelType_('SRT_NQ').
is_SetRelType_('SRT_SUB').
is_SetRelType_('SRT_SUP').
is_SetRelType_('SRT_DISJ').
is_SetRelType_('SRT_CMPL').
is_SetRelType_('SRT_LQ').
is_SetRelType_('SRT_LE').
is_SetRelType_('SRT_GQ').
is_SetRelType_('SRT_GR').


is_SetRelType_('SRT_EQ','SRT_EQ').
is_SetRelType_('SRT_NQ','SRT_NQ').
is_SetRelType_('SRT_SUB','SRT_SUB').
is_SetRelType_('SRT_SUP','SRT_SUP').
is_SetRelType_('SRT_DISJ','SRT_DISJ').
is_SetRelType_('SRT_CMPL','SRT_CMPL').
is_SetRelType_('SRT_LQ','SRT_LQ').
is_SetRelType_('SRT_LE','SRT_LE').
is_SetRelType_('SRT_GQ','SRT_GQ').
is_SetRelType_('SRT_GR','SRT_GR').


is_SetRelType(X,Y) :- nonvar(X), is_SetRelType_(X,Y).
is_SetRelType(X) :- is_SetRelType_(X,_).


is_SetOpType_('SOT_UNION').
is_SetOpType_('SOT_DUNION').
is_SetOpType_('SOT_INTER').
is_SetOpType_('SOT_MINUS').


is_SetOpType_('SOT_UNION','SOT_UNION').
is_SetOpType_('SOT_DUNION','SOT_DUNION').
is_SetOpType_('SOT_INTER','SOT_INTER').
is_SetOpType_('SOT_MINUS','SOT_MINUS').


is_SetOpType(X,Y) :- nonvar(X), is_SetOpType_(X,Y).
is_SetOpType(X) :- is_SetOpType_(X,_).


assign(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_BoolAssign(X2,Y2)
                 -> gecode_constraint_assign_1(Y0,Y1,Y2)
                 ;  throw(error(type_error('BoolAssign'(X2)),gecode_argument_error(assign(X0,X1,X2),arg=3))))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatAssign(X2,Y2)
                     -> gecode_constraint_assign_3(Y0,Y1,Y2)
                     ;  throw(error(type_error('FloatAssign'(X2)),gecode_argument_error(assign(X0,X1,X2),arg=3))))
                 ;  (is_IntVar(X1,Y1)
                     -> (is_IntAssign(X2,Y2)
                         -> gecode_constraint_assign_5(Y0,Y1,Y2)
                         ;  throw(error(type_error('IntAssign'(X2)),gecode_argument_error(assign(X0,X1,X2),arg=3))))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_SetAssign(X2,Y2)
                             -> gecode_constraint_assign_7(Y0,Y1,Y2)
                             ;  throw(error(type_error('SetAssign'(X2)),gecode_argument_error(assign(X0,X1,X2),arg=3))))
                         ;  throw(error(type_error('SetVar'(X1)),gecode_argument_error(assign(X0,X1,X2),arg=2)))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(assign(X0,X1,X2),arg=1)))).

assign(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_BoolAssign(X2,Y2)
                 -> (is_BoolVarValPrint(X3,Y3)
                     -> gecode_constraint_assign_2(Y0,Y1,Y2,Y3)
                     ;  throw(error(type_error('BoolVarValPrint'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                 ;  throw(error(type_error('BoolAssign'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatAssign(X2,Y2)
                     -> (is_FloatVarValPrint(X3,Y3)
                         -> gecode_constraint_assign_4(Y0,Y1,Y2,Y3)
                         ;  throw(error(type_error('FloatVarValPrint'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                     ;  throw(error(type_error('FloatAssign'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
                 ;  (is_IntVar(X1,Y1)
                     -> (is_IntAssign(X2,Y2)
                         -> (is_IntVarValPrint(X3,Y3)
                             -> gecode_constraint_assign_6(Y0,Y1,Y2,Y3)
                             ;  throw(error(type_error('IntVarValPrint'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                         ;  throw(error(type_error('IntAssign'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_SetAssign(X2,Y2)
                             -> (is_SetVarValPrint(X3,Y3)
                                 -> gecode_constraint_assign_8(Y0,Y1,Y2,Y3)
                                 ;  throw(error(type_error('SetVarValPrint'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                             ;  throw(error(type_error('SetAssign'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
                         ;  (is_BoolVarArgs(X1,Y1)
                             -> (is_BoolVarBranch(X2,Y2)
                                 -> (is_BoolAssign(X3,Y3)
                                     -> gecode_constraint_assign_9(Y0,Y1,Y2,Y3)
                                     ;  throw(error(type_error('BoolAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                                 ;  throw(error(type_error('BoolVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
                             ;  (is_FloatVarArgs(X1,Y1)
                                 -> (is_FloatVarBranch(X2,Y2)
                                     -> (is_FloatAssign(X3,Y3)
                                         -> gecode_constraint_assign_12(Y0,Y1,Y2,Y3)
                                         ;  throw(error(type_error('FloatAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                                     ;  throw(error(type_error('FloatVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
                                 ;  (is_IntVarArgs(X1,Y1)
                                     -> (is_IntVarBranch(X2,Y2)
                                         -> (is_IntAssign(X3,Y3)
                                             -> gecode_constraint_assign_15(Y0,Y1,Y2,Y3)
                                             ;  throw(error(type_error('IntAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                                         ;  throw(error(type_error('IntVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
                                     ;  (is_SetVarArgs(X1,Y1)
                                         -> (is_SetVarBranch(X2,Y2)
                                             -> (is_SetAssign(X3,Y3)
                                                 -> gecode_constraint_assign_18(Y0,Y1,Y2,Y3)
                                                 ;  throw(error(type_error('SetAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3),arg=4))))
                                             ;  throw(error(type_error('SetVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3),arg=3))))
                                         ;  throw(error(type_error('SetVarArgs'(X1)),gecode_argument_error(assign(X0,X1,X2,X3),arg=2)))))))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(assign(X0,X1,X2,X3),arg=1)))).

assign(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_BoolVarBranch(X2,Y2)
                 -> (is_BoolAssign(X3,Y3)
                     -> (is_BoolBranchFilter(X4,Y4)
                         -> gecode_constraint_assign_10(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(error(type_error('BoolBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(error(type_error('BoolAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(error(type_error('BoolVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatVarBranch(X2,Y2)
                     -> (is_FloatAssign(X3,Y3)
                         -> (is_FloatBranchFilter(X4,Y4)
                             -> gecode_constraint_assign_13(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error('FloatBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(error(type_error('FloatAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(error(type_error('FloatVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3))))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntVarBranch(X2,Y2)
                         -> (is_IntAssign(X3,Y3)
                             -> (is_IntBranchFilter(X4,Y4)
                                 -> gecode_constraint_assign_16(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error('IntBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5))))
                             ;  throw(error(type_error('IntAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4))))
                         ;  throw(error(type_error('IntVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3))))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_SetVarBranch(X2,Y2)
                             -> (is_SetAssign(X3,Y3)
                                 -> (is_SetBranchFilter(X4,Y4)
                                     -> gecode_constraint_assign_19(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(error(type_error('SetBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5))))
                                 ;  throw(error(type_error('SetAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4))))
                             ;  throw(error(type_error('SetVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3))))
                         ;  throw(error(type_error('SetVarArgs'(X1)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=2)))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=1)))).

assign(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_BoolVarBranch(X2,Y2)
                 -> (is_BoolAssign(X3,Y3)
                     -> (is_BoolBranchFilter(X4,Y4)
                         -> (is_BoolVarValPrint(X5,Y5)
                             -> gecode_constraint_assign_11(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(error(type_error('BoolVarValPrint'(X5)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  throw(error(type_error('BoolBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(error(type_error('BoolAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(error(type_error('BoolVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatVarBranch(X2,Y2)
                     -> (is_FloatAssign(X3,Y3)
                         -> (is_FloatBranchFilter(X4,Y4)
                             -> (is_FloatVarValPrint(X5,Y5)
                                 -> gecode_constraint_assign_14(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('FloatVarValPrint'(X5)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('FloatBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(error(type_error('FloatAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(error(type_error('FloatVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=3))))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntVarBranch(X2,Y2)
                         -> (is_IntAssign(X3,Y3)
                             -> (is_IntBranchFilter(X4,Y4)
                                 -> (is_IntVarValPrint(X5,Y5)
                                     -> gecode_constraint_assign_17(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(error(type_error('IntVarValPrint'(X5)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  throw(error(type_error('IntBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(error(type_error('IntAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=4))))
                         ;  throw(error(type_error('IntVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=3))))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_SetVarBranch(X2,Y2)
                             -> (is_SetAssign(X3,Y3)
                                 -> (is_SetBranchFilter(X4,Y4)
                                     -> (is_SetVarValPrint(X5,Y5)
                                         -> gecode_constraint_assign_20(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(error(type_error('SetVarValPrint'(X5)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(error(type_error('SetBranchFilter'(X4)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  throw(error(type_error('SetAssign'(X3)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=4))))
                             ;  throw(error(type_error('SetVarBranch'(X2)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=3))))
                         ;  throw(error(type_error('SetVarArgs'(X1)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=2)))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(assign(X0,X1,X2,X3,X4,X5),arg=1)))).

binpacking(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> gecode_constraint_binpacking_21(Y0,Y1,Y2,Y3)
                     ;  throw(error(type_error('IntArgs'(X3)),gecode_argument_error(binpacking(X0,X1,X2,X3),arg=4))))
                 ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(binpacking(X0,X1,X2,X3),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(binpacking(X0,X1,X2,X3),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(binpacking(X0,X1,X2,X3),arg=1)))).

binpacking(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_IntPropLevel(X4,Y4)
                         -> gecode_constraint_binpacking_22(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(error(type_error('IntArgs'(X3)),gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=1)))).

branch(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_BoolValBranch(X2,Y2)
                 -> gecode_constraint_branch_23(Y0,Y1,Y2)
                 ;  throw(error(type_error('BoolValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2),arg=3))))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatValBranch(X2,Y2)
                     -> gecode_constraint_branch_25(Y0,Y1,Y2)
                     ;  throw(error(type_error('FloatValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2),arg=3))))
                 ;  (is_IntVar(X1,Y1)
                     -> (is_IntValBranch(X2,Y2)
                         -> gecode_constraint_branch_27(Y0,Y1,Y2)
                         ;  throw(error(type_error('IntValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2),arg=3))))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_SetValBranch(X2,Y2)
                             -> gecode_constraint_branch_29(Y0,Y1,Y2)
                             ;  throw(error(type_error('SetValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2),arg=3))))
                         ;  throw(error(type_error('SetVar'(X1)),gecode_argument_error(branch(X0,X1,X2),arg=2)))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(branch(X0,X1,X2),arg=1)))).

branch(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_BoolValBranch(X2,Y2)
                 -> (is_BoolVarValPrint(X3,Y3)
                     -> gecode_constraint_branch_24(Y0,Y1,Y2,Y3)
                     ;  throw(error(type_error('BoolVarValPrint'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                 ;  throw(error(type_error('BoolValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatValBranch(X2,Y2)
                     -> (is_FloatVarValPrint(X3,Y3)
                         -> gecode_constraint_branch_26(Y0,Y1,Y2,Y3)
                         ;  throw(error(type_error('FloatVarValPrint'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                     ;  throw(error(type_error('FloatValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
                 ;  (is_IntVar(X1,Y1)
                     -> (is_IntValBranch(X2,Y2)
                         -> (is_IntVarValPrint(X3,Y3)
                             -> gecode_constraint_branch_28(Y0,Y1,Y2,Y3)
                             ;  throw(error(type_error('IntVarValPrint'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                         ;  throw(error(type_error('IntValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_SetValBranch(X2,Y2)
                             -> (is_SetVarValPrint(X3,Y3)
                                 -> gecode_constraint_branch_30(Y0,Y1,Y2,Y3)
                                 ;  throw(error(type_error('SetVarValPrint'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                             ;  throw(error(type_error('SetValBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
                         ;  (is_BoolVarArgs(X1,Y1)
                             -> (is_BoolVarBranch(X2,Y2)
                                 -> (is_BoolValBranch(X3,Y3)
                                     -> gecode_constraint_branch_31(Y0,Y1,Y2,Y3)
                                     ;  throw(error(type_error('BoolValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                                 ;  throw(error(type_error('BoolVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
                             ;  (is_FloatVarArgs(X1,Y1)
                                 -> (is_FloatVarBranch(X2,Y2)
                                     -> (is_FloatValBranch(X3,Y3)
                                         -> gecode_constraint_branch_37(Y0,Y1,Y2,Y3)
                                         ;  throw(error(type_error('FloatValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                                     ;  throw(error(type_error('FloatVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
                                 ;  (is_IntVarArgs(X1,Y1)
                                     -> (is_IntVarBranch(X2,Y2)
                                         -> (is_IntValBranch(X3,Y3)
                                             -> gecode_constraint_branch_40(Y0,Y1,Y2,Y3)
                                             ;  throw(error(type_error('IntValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                                         ;  throw(error(type_error('IntVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
                                     ;  (is_SetVarArgs(X1,Y1)
                                         -> (is_SetVarBranch(X2,Y2)
                                             -> (is_SetValBranch(X3,Y3)
                                                 -> gecode_constraint_branch_46(Y0,Y1,Y2,Y3)
                                                 ;  throw(error(type_error('SetValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3),arg=4))))
                                             ;  throw(error(type_error('SetVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3),arg=3))))
                                         ;  throw(error(type_error('SetVarArgs'(X1)),gecode_argument_error(branch(X0,X1,X2,X3),arg=2)))))))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(branch(X0,X1,X2,X3),arg=1)))).

branch(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_BoolVarBranch(X2,Y2)
                 -> (is_BoolValBranch(X3,Y3)
                     -> (is_BoolBranchFilter(X4,Y4)
                         -> gecode_constraint_branch_32(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_Symmetries(X4,Y4)
                             -> gecode_constraint_branch_34(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5)))))
                     ;  throw(error(type_error('BoolValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(error(type_error('BoolVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatVarBranch(X2,Y2)
                     -> (is_FloatValBranch(X3,Y3)
                         -> (is_FloatBranchFilter(X4,Y4)
                             -> gecode_constraint_branch_38(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error('FloatBranchFilter'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(error(type_error('FloatValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(error(type_error('FloatVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3))))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntVarBranch(X2,Y2)
                         -> (is_IntValBranch(X3,Y3)
                             -> (is_IntBranchFilter(X4,Y4)
                                 -> gecode_constraint_branch_41(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_Symmetries(X4,Y4)
                                     -> gecode_constraint_branch_43(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5)))))
                             ;  throw(error(type_error('IntValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4))))
                         ;  throw(error(type_error('IntVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3))))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_SetVarBranch(X2,Y2)
                             -> (is_SetValBranch(X3,Y3)
                                 -> (is_SetBranchFilter(X4,Y4)
                                     -> gecode_constraint_branch_47(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_Symmetries(X4,Y4)
                                         -> gecode_constraint_branch_49(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5)))))
                                 ;  throw(error(type_error('SetValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4))))
                             ;  throw(error(type_error('SetVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3))))
                         ;  throw(error(type_error('SetVarArgs'(X1)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=2)))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=1)))).

branch(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_BoolVarBranch(X2,Y2)
                 -> (is_BoolValBranch(X3,Y3)
                     -> (is_BoolBranchFilter(X4,Y4)
                         -> (is_BoolVarValPrint(X5,Y5)
                             -> gecode_constraint_branch_33(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(error(type_error('BoolVarValPrint'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  (is_Symmetries(X4,Y4)
                             -> (is_BoolBranchFilter(X5,Y5)
                                 -> gecode_constraint_branch_35(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('BoolBranchFilter'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5)))))
                     ;  throw(error(type_error('BoolValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(error(type_error('BoolVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatVarBranch(X2,Y2)
                     -> (is_FloatValBranch(X3,Y3)
                         -> (is_FloatBranchFilter(X4,Y4)
                             -> (is_FloatVarValPrint(X5,Y5)
                                 -> gecode_constraint_branch_39(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('FloatVarValPrint'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('FloatBranchFilter'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(error(type_error('FloatValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(error(type_error('FloatVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3))))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntVarBranch(X2,Y2)
                         -> (is_IntValBranch(X3,Y3)
                             -> (is_IntBranchFilter(X4,Y4)
                                 -> (is_IntVarValPrint(X5,Y5)
                                     -> gecode_constraint_branch_42(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(error(type_error('IntVarValPrint'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  (is_Symmetries(X4,Y4)
                                     -> (is_IntBranchFilter(X5,Y5)
                                         -> gecode_constraint_branch_44(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(error(type_error('IntBranchFilter'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5)))))
                             ;  throw(error(type_error('IntValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4))))
                         ;  throw(error(type_error('IntVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3))))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_SetVarBranch(X2,Y2)
                             -> (is_SetValBranch(X3,Y3)
                                 -> (is_SetBranchFilter(X4,Y4)
                                     -> (is_SetVarValPrint(X5,Y5)
                                         -> gecode_constraint_branch_48(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(error(type_error('SetVarValPrint'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  (is_Symmetries(X4,Y4)
                                         -> (is_SetBranchFilter(X5,Y5)
                                             -> gecode_constraint_branch_50(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(error(type_error('SetBranchFilter'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6))))
                                         ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5)))))
                                 ;  throw(error(type_error('SetValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4))))
                             ;  throw(error(type_error('SetVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3))))
                         ;  throw(error(type_error('SetVarArgs'(X1)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=2)))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=1)))).

branch(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_BoolVarBranch(X2,Y2)
                 -> (is_BoolValBranch(X3,Y3)
                     -> (is_Symmetries(X4,Y4)
                         -> (is_BoolBranchFilter(X5,Y5)
                             -> (is_BoolVarValPrint(X6,Y6)
                                 -> gecode_constraint_branch_36(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(error(type_error('BoolVarValPrint'(X6)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                             ;  throw(error(type_error('BoolBranchFilter'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                         ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                     ;  throw(error(type_error('BoolValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                 ;  throw(error(type_error('BoolVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarBranch(X2,Y2)
                     -> (is_IntValBranch(X3,Y3)
                         -> (is_Symmetries(X4,Y4)
                             -> (is_IntBranchFilter(X5,Y5)
                                 -> (is_IntVarValPrint(X6,Y6)
                                     -> gecode_constraint_branch_45(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(error(type_error('IntVarValPrint'(X6)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(error(type_error('IntBranchFilter'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                             ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                         ;  throw(error(type_error('IntValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                     ;  throw(error(type_error('IntVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=3))))
                 ;  (is_SetVarArgs(X1,Y1)
                     -> (is_SetVarBranch(X2,Y2)
                         -> (is_SetValBranch(X3,Y3)
                             -> (is_Symmetries(X4,Y4)
                                 -> (is_SetBranchFilter(X5,Y5)
                                     -> (is_SetVarValPrint(X6,Y6)
                                         -> gecode_constraint_branch_51(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(error(type_error('SetVarValPrint'(X6)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                     ;  throw(error(type_error('SetBranchFilter'(X5)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                                 ;  throw(error(type_error('Symmetries'(X4)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                             ;  throw(error(type_error('SetValBranch'(X3)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                         ;  throw(error(type_error('SetVarBranch'(X2)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=3))))
                     ;  throw(error(type_error('SetVarArgs'(X1)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=2))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=1)))).

clause(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVarArgs(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> gecode_constraint_clause_52(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_int(X4,Y4)
                             -> gecode_constraint_clause_54(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error(int(X4)),gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=5)))))
                     ;  throw(error(type_error('BoolVarArgs'(X3)),gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(error(type_error('BoolVarArgs'(X2)),gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=3))))
             ;  throw(error(type_error('BoolOpType'(X1)),gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=1)))).

clause(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVarArgs(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> (is_IntPropLevel(X5,Y5)
                             -> gecode_constraint_clause_53(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  (is_int(X4,Y4)
                             -> (is_IntPropLevel(X5,Y5)
                                 -> gecode_constraint_clause_55(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error(int(X4)),gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=5)))))
                     ;  throw(error(type_error('BoolVarArgs'(X3)),gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(error(type_error('BoolVarArgs'(X2)),gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  throw(error(type_error('BoolOpType'(X1)),gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=1)))).

extensional(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_DFA(X2,Y2)
                 -> gecode_constraint_extensional_56(Y0,Y1,Y2)
                 ;  throw(error(type_error('DFA'(X2)),gecode_argument_error(extensional(X0,X1,X2),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_DFA(X2,Y2)
                     -> gecode_constraint_extensional_62(Y0,Y1,Y2)
                     ;  throw(error(type_error('DFA'(X2)),gecode_argument_error(extensional(X0,X1,X2),arg=3))))
                 ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(extensional(X0,X1,X2),arg=2)))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(extensional(X0,X1,X2),arg=1)))).

extensional(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_DFA(X2,Y2)
                 -> (is_IntPropLevel(X3,Y3)
                     -> gecode_constraint_extensional_57(Y0,Y1,Y2,Y3)
                     ;  throw(error(type_error('IntPropLevel'(X3)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=4))))
                 ;  (is_TupleSet(X2,Y2)
                     -> (is_bool(X3,Y3)
                         -> gecode_constraint_extensional_58(Y0,Y1,Y2,Y3)
                         ;  throw(error(type_error(bool(X3)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=4))))
                     ;  throw(error(type_error('TupleSet'(X2)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=3)))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_DFA(X2,Y2)
                     -> (is_IntPropLevel(X3,Y3)
                         -> gecode_constraint_extensional_63(Y0,Y1,Y2,Y3)
                         ;  throw(error(type_error('IntPropLevel'(X3)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=4))))
                     ;  (is_TupleSet(X2,Y2)
                         -> (is_bool(X3,Y3)
                             -> gecode_constraint_extensional_64(Y0,Y1,Y2,Y3)
                             ;  throw(error(type_error(bool(X3)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=4))))
                         ;  throw(error(type_error('TupleSet'(X2)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=3)))))
                 ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=2)))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(extensional(X0,X1,X2,X3),arg=1)))).

extensional(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_TupleSet(X2,Y2)
                 -> (is_bool(X3,Y3)
                     -> (is_IntPropLevel(X4,Y4)
                         -> gecode_constraint_extensional_59(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_Reify(X4,Y4)
                             -> gecode_constraint_extensional_60(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=5)))))
                     ;  throw(error(type_error(bool(X3)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(error(type_error('TupleSet'(X2)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_TupleSet(X2,Y2)
                     -> (is_bool(X3,Y3)
                         -> (is_IntPropLevel(X4,Y4)
                             -> gecode_constraint_extensional_65(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_Reify(X4,Y4)
                                 -> gecode_constraint_extensional_66(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=5)))))
                         ;  throw(error(type_error(bool(X3)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(error(type_error('TupleSet'(X2)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=3))))
                 ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=2)))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=1)))).

extensional(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_TupleSet(X2,Y2)
                 -> (is_bool(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> (is_IntPropLevel(X5,Y5)
                             -> gecode_constraint_extensional_61(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(error(type_error(bool(X3)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(error(type_error('TupleSet'(X2)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_TupleSet(X2,Y2)
                     -> (is_bool(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> (is_IntPropLevel(X5,Y5)
                                 -> gecode_constraint_extensional_67(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(error(type_error(bool(X3)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(error(type_error('TupleSet'(X2)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=3))))
                 ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=2)))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(extensional(X0,X1,X2,X3,X4,X5),arg=1)))).

ite(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_BoolVar(X2,Y2)
                 -> (is_BoolVar(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> gecode_constraint_ite_68(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(error(type_error('BoolVar'(X4)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(error(type_error('BoolVar'(X3)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=4))))
                 ;  (is_FloatVar(X2,Y2)
                     -> (is_FloatVar(X3,Y3)
                         -> (is_FloatVar(X4,Y4)
                             -> gecode_constraint_ite_70(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error('FloatVar'(X4)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(error(type_error('FloatVar'(X3)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=4))))
                     ;  (is_IntVar(X2,Y2)
                         -> (is_IntVar(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> gecode_constraint_ite_71(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error('IntVar'(X4)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=5))))
                             ;  throw(error(type_error('IntVar'(X3)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=4))))
                         ;  throw(error(type_error('IntVar'(X2)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=3))))))
             ;  throw(error(type_error('BoolVar'(X1)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=1)))).

ite(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_BoolVar(X2,Y2)
                 -> (is_BoolVar(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> (is_IntPropLevel(X5,Y5)
                             -> gecode_constraint_ite_69(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  throw(error(type_error('BoolVar'(X4)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(error(type_error('BoolVar'(X3)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  (is_IntVar(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntPropLevel(X5,Y5)
                                 -> gecode_constraint_ite_72(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('IntVar'(X4)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(error(type_error('IntVar'(X3)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(error(type_error('IntVar'(X2)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=3)))))
             ;  throw(error(type_error('BoolVar'(X1)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=1)))).

linear(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_linear_73(Y0,Y1,Y2,Y3)
                     ;  (is_int(X3,Y3)
                         -> gecode_constraint_linear_77(Y0,Y1,Y2,Y3)
                         ;  throw(error(type_error(int(X3)),gecode_argument_error(linear(X0,X1,X2,X3),arg=4)))))
                 ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3),arg=3))))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatRelType(X2,Y2)
                     -> (is_FloatVal(X3,Y3)
                         -> gecode_constraint_linear_85(Y0,Y1,Y2,Y3)
                         ;  (is_FloatVar(X3,Y3)
                             -> gecode_constraint_linear_87(Y0,Y1,Y2,Y3)
                             ;  throw(error(type_error('FloatVar'(X3)),gecode_argument_error(linear(X0,X1,X2,X3),arg=4)))))
                     ;  throw(error(type_error('FloatRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3),arg=3))))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntRelType(X2,Y2)
                         -> (is_IntVar(X3,Y3)
                             -> gecode_constraint_linear_105(Y0,Y1,Y2,Y3)
                             ;  (is_int(X3,Y3)
                                 -> gecode_constraint_linear_109(Y0,Y1,Y2,Y3)
                                 ;  throw(error(type_error(int(X3)),gecode_argument_error(linear(X0,X1,X2,X3),arg=4)))))
                         ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3),arg=3))))
                     ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(linear(X0,X1,X2,X3),arg=2))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(linear(X0,X1,X2,X3),arg=1)))).

linear(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntPropLevel(X4,Y4)
                         -> gecode_constraint_linear_74(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_Reify(X4,Y4)
                             -> gecode_constraint_linear_75(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))))
                     ;  (is_int(X3,Y3)
                         -> (is_IntPropLevel(X4,Y4)
                             -> gecode_constraint_linear_78(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_Reify(X4,Y4)
                                 -> gecode_constraint_linear_79(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))))
                         ;  throw(error(type_error(int(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))))
                 ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_FloatValArgs(X1,Y1)
                 -> (is_FloatVarArgs(X2,Y2)
                     -> (is_FloatRelType(X3,Y3)
                         -> (is_FloatVal(X4,Y4)
                             -> gecode_constraint_linear_81(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_FloatVar(X4,Y4)
                                 -> gecode_constraint_linear_83(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error('FloatVar'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))))
                         ;  throw(error(type_error('FloatRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(error(type_error('FloatVarArgs'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3))))
                 ;  (is_FloatVarArgs(X1,Y1)
                     -> (is_FloatRelType(X2,Y2)
                         -> (is_FloatVal(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> gecode_constraint_linear_86(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                             ;  (is_FloatVar(X3,Y3)
                                 -> (is_Reify(X4,Y4)
                                     -> gecode_constraint_linear_88(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                                 ;  throw(error(type_error('FloatVar'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))))
                         ;  throw(error(type_error('FloatRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3))))
                     ;  (is_IntArgs(X1,Y1)
                         -> (is_BoolVarArgs(X2,Y2)
                             -> (is_IntRelType(X3,Y3)
                                 -> (is_IntVar(X4,Y4)
                                     -> gecode_constraint_linear_89(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_int(X4,Y4)
                                         -> gecode_constraint_linear_93(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(error(type_error(int(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))))
                                 ;  throw(error(type_error('IntRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                             ;  (is_IntVarArgs(X2,Y2)
                                 -> (is_IntRelType(X3,Y3)
                                     -> (is_IntVar(X4,Y4)
                                         -> gecode_constraint_linear_97(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_int(X4,Y4)
                                             -> gecode_constraint_linear_101(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(error(type_error(int(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))))
                                     ;  throw(error(type_error('IntRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                                 ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3)))))
                         ;  (is_IntVarArgs(X1,Y1)
                             -> (is_IntRelType(X2,Y2)
                                 -> (is_IntVar(X3,Y3)
                                     -> (is_IntPropLevel(X4,Y4)
                                         -> gecode_constraint_linear_106(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_Reify(X4,Y4)
                                             -> gecode_constraint_linear_107(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))))
                                     ;  (is_int(X3,Y3)
                                         -> (is_IntPropLevel(X4,Y4)
                                             -> gecode_constraint_linear_110(Y0,Y1,Y2,Y3,Y4)
                                             ;  (is_Reify(X4,Y4)
                                                 -> gecode_constraint_linear_111(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))))
                                         ;  throw(error(type_error(int(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))))
                                 ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3))))
                             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=2))))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=1)))).

linear(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> (is_IntPropLevel(X5,Y5)
                             -> gecode_constraint_linear_76(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  (is_int(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> (is_IntPropLevel(X5,Y5)
                                 -> gecode_constraint_linear_80(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(error(type_error(int(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4)))))
                 ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  (is_FloatValArgs(X1,Y1)
                 -> (is_FloatVarArgs(X2,Y2)
                     -> (is_FloatRelType(X3,Y3)
                         -> (is_FloatVal(X4,Y4)
                             -> (is_Reify(X5,Y5)
                                 -> gecode_constraint_linear_82(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  (is_FloatVar(X4,Y4)
                                 -> (is_Reify(X5,Y5)
                                     -> gecode_constraint_linear_84(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  throw(error(type_error('FloatVar'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))))
                         ;  throw(error(type_error('FloatRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(error(type_error('FloatVarArgs'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3))))
                 ;  (is_IntArgs(X1,Y1)
                     -> (is_BoolVarArgs(X2,Y2)
                         -> (is_IntRelType(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> (is_IntPropLevel(X5,Y5)
                                     -> gecode_constraint_linear_90(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  (is_Reify(X5,Y5)
                                         -> gecode_constraint_linear_91(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))))
                                 ;  (is_int(X4,Y4)
                                     -> (is_IntPropLevel(X5,Y5)
                                         -> gecode_constraint_linear_94(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_Reify(X5,Y5)
                                             -> gecode_constraint_linear_95(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))))
                                     ;  throw(error(type_error(int(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))))
                             ;  throw(error(type_error('IntRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4))))
                         ;  (is_IntVarArgs(X2,Y2)
                             -> (is_IntRelType(X3,Y3)
                                 -> (is_IntVar(X4,Y4)
                                     -> (is_IntPropLevel(X5,Y5)
                                         -> gecode_constraint_linear_98(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_Reify(X5,Y5)
                                             -> gecode_constraint_linear_99(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))))
                                     ;  (is_int(X4,Y4)
                                         -> (is_IntPropLevel(X5,Y5)
                                             -> gecode_constraint_linear_102(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  (is_Reify(X5,Y5)
                                                 -> gecode_constraint_linear_103(Y0,Y1,Y2,Y3,Y4,Y5)
                                                 ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))))
                                         ;  throw(error(type_error(int(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))))
                                 ;  throw(error(type_error('IntRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4))))
                             ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3)))))
                     ;  (is_IntVarArgs(X1,Y1)
                         -> (is_IntRelType(X2,Y2)
                             -> (is_IntVar(X3,Y3)
                                 -> (is_Reify(X4,Y4)
                                     -> (is_IntPropLevel(X5,Y5)
                                         -> gecode_constraint_linear_108(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  (is_int(X3,Y3)
                                     -> (is_Reify(X4,Y4)
                                         -> (is_IntPropLevel(X5,Y5)
                                             -> gecode_constraint_linear_112(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                         ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                                     ;  throw(error(type_error(int(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4)))))
                             ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3))))
                         ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=2)))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=1)))).

linear(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_IntRelType(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_Reify(X5,Y5)
                             -> (is_IntPropLevel(X6,Y6)
                                 -> gecode_constraint_linear_92(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(error(type_error('IntPropLevel'(X6)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                             ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                         ;  (is_int(X4,Y4)
                             -> (is_Reify(X5,Y5)
                                 -> (is_IntPropLevel(X6,Y6)
                                     -> gecode_constraint_linear_96(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(error(type_error('IntPropLevel'(X6)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                             ;  throw(error(type_error(int(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=5)))))
                     ;  throw(error(type_error('IntRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntRelType(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_Reify(X5,Y5)
                                 -> (is_IntPropLevel(X6,Y6)
                                     -> gecode_constraint_linear_100(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(error(type_error('IntPropLevel'(X6)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                             ;  (is_int(X4,Y4)
                                 -> (is_Reify(X5,Y5)
                                     -> (is_IntPropLevel(X6,Y6)
                                         -> gecode_constraint_linear_104(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(error(type_error('IntPropLevel'(X6)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                     ;  throw(error(type_error('Reify'(X5)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                                 ;  throw(error(type_error(int(X4)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=5)))))
                         ;  throw(error(type_error('IntRelType'(X3)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                     ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=3)))))
             ;  throw(error(type_error('IntArgs'(X1)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=1)))).

nooverlap(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> gecode_constraint_nooverlap_113(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(error(type_error('IntArgs'(X4)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(error(type_error('IntArgs'(X2)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=1)))).

nooverlap(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_IntPropLevel(X5,Y5)
                             -> gecode_constraint_nooverlap_114(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  (is_BoolVarArgs(X5,Y5)
                                 -> gecode_constraint_nooverlap_115(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('BoolVarArgs'(X5)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=6)))))
                         ;  throw(error(type_error('IntArgs'(X4)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(error(type_error('IntArgs'(X2)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=1)))).

nooverlap(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_BoolVarArgs(X5,Y5)
                             -> (is_IntPropLevel(X6,Y6)
                                 -> gecode_constraint_nooverlap_116(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(error(type_error('IntPropLevel'(X6)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                             ;  throw(error(type_error('BoolVarArgs'(X5)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                         ;  throw(error(type_error('IntArgs'(X4)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                     ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntVarArgs(X6,Y6)
                                     -> gecode_constraint_nooverlap_117(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(error(type_error('IntVarArgs'(X6)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(error(type_error('IntVarArgs'(X5)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                             ;  throw(error(type_error('IntVarArgs'(X4)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                         ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                     ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=3)))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=1)))).

nooverlap(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_IntPropLevel(X7,Y7)
                                     -> gecode_constraint_nooverlap_118(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  (is_BoolVarArgs(X7,Y7)
                                         -> gecode_constraint_nooverlap_119(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(error(type_error('BoolVarArgs'(X7)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))))
                                 ;  throw(error(type_error('IntVarArgs'(X6)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=7))))
                             ;  throw(error(type_error('IntVarArgs'(X5)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                         ;  throw(error(type_error('IntVarArgs'(X4)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=5))))
                     ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=4))))
                 ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=1)))).

nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_BoolVarArgs(X7,Y7)
                                     -> (is_IntPropLevel(X8,Y8)
                                         -> gecode_constraint_nooverlap_120(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                         ;  throw(error(type_error('IntPropLevel'(X8)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9))))
                                     ;  throw(error(type_error('BoolVarArgs'(X7)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8))))
                                 ;  throw(error(type_error('IntVarArgs'(X6)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7))))
                             ;  throw(error(type_error('IntVarArgs'(X5)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                         ;  throw(error(type_error('IntVarArgs'(X4)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5))))
                     ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4))))
                 ;  throw(error(type_error('IntVarArgs'(X2)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=1)))).

precede(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> gecode_constraint_precede_121(Y0,Y1,Y2)
                 ;  throw(error(type_error('IntArgs'(X2)),gecode_argument_error(precede(X0,X1,X2),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(precede(X0,X1,X2),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(precede(X0,X1,X2),arg=1)))).

precede(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntPropLevel(X3,Y3)
                     -> gecode_constraint_precede_122(Y0,Y1,Y2,Y3)
                     ;  throw(error(type_error('IntPropLevel'(X3)),gecode_argument_error(precede(X0,X1,X2,X3),arg=4))))
                 ;  (is_int(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_precede_123(Y0,Y1,Y2,Y3)
                         ;  throw(error(type_error(int(X3)),gecode_argument_error(precede(X0,X1,X2,X3),arg=4))))
                     ;  throw(error(type_error(int(X2)),gecode_argument_error(precede(X0,X1,X2,X3),arg=3)))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(precede(X0,X1,X2,X3),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(precede(X0,X1,X2,X3),arg=1)))).

precede(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntPropLevel(X4,Y4)
                         -> gecode_constraint_precede_124(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(error(type_error(int(X3)),gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(error(type_error(int(X2)),gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=3))))
             ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=1)))).

rel(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVar(X3,Y3)
                     -> gecode_constraint_rel_125(Y0,Y1,Y2,Y3)
                     ;  (is_int(X3,Y3)
                         -> gecode_constraint_rel_127(Y0,Y1,Y2,Y3)
                         ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                 ;  throw(error(type_error('BoolVarArgs'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
             ;  (is_BoolVar(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_BoolVar(X3,Y3)
                         -> gecode_constraint_rel_133(Y0,Y1,Y2,Y3)
                         ;  (is_int(X3,Y3)
                             -> gecode_constraint_rel_137(Y0,Y1,Y2,Y3)
                             ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                     ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                 ;  (is_FloatVar(X1,Y1)
                     -> (is_FloatRelType(X2,Y2)
                         -> (is_FloatVal(X3,Y3)
                             -> gecode_constraint_rel_141(Y0,Y1,Y2,Y3)
                             ;  (is_FloatVar(X3,Y3)
                                 -> gecode_constraint_rel_143(Y0,Y1,Y2,Y3)
                                 ;  throw(error(type_error('FloatVar'(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                         ;  throw(error(type_error('FloatRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                     ;  (is_IntVar(X1,Y1)
                         -> (is_IntRelType(X2,Y2)
                             -> (is_IntVar(X3,Y3)
                                 -> gecode_constraint_rel_145(Y0,Y1,Y2,Y3)
                                 ;  (is_int(X3,Y3)
                                     -> gecode_constraint_rel_149(Y0,Y1,Y2,Y3)
                                     ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                             ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                         ;  (is_BoolVarArgs(X1,Y1)
                             -> (is_IntRelType(X2,Y2)
                                 -> (is_BoolVar(X3,Y3)
                                     -> gecode_constraint_rel_153(Y0,Y1,Y2,Y3)
                                     ;  (is_IntPropLevel(X3,Y3)
                                         -> gecode_constraint_rel_156(Y0,Y1,Y2,Y3)
                                         ;  (is_BoolVarArgs(X3,Y3)
                                             -> gecode_constraint_rel_157(Y0,Y1,Y2,Y3)
                                             ;  (is_IntArgs(X3,Y3)
                                                 -> gecode_constraint_rel_159(Y0,Y1,Y2,Y3)
                                                 ;  (is_int(X3,Y3)
                                                     -> gecode_constraint_rel_161(Y0,Y1,Y2,Y3)
                                                     ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))))))
                                 ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                             ;  (is_FloatVarArgs(X1,Y1)
                                 -> (is_FloatRelType(X2,Y2)
                                     -> (is_FloatVal(X3,Y3)
                                         -> gecode_constraint_rel_163(Y0,Y1,Y2,Y3)
                                         ;  (is_FloatVar(X3,Y3)
                                             -> gecode_constraint_rel_164(Y0,Y1,Y2,Y3)
                                             ;  throw(error(type_error('FloatVar'(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                                     ;  throw(error(type_error('FloatRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                                 ;  (is_IntArgs(X1,Y1)
                                     -> (is_IntRelType(X2,Y2)
                                         -> (is_BoolVarArgs(X3,Y3)
                                             -> gecode_constraint_rel_165(Y0,Y1,Y2,Y3)
                                             ;  (is_IntVarArgs(X3,Y3)
                                                 -> gecode_constraint_rel_167(Y0,Y1,Y2,Y3)
                                                 ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                                         ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                                     ;  (is_IntVarArgs(X1,Y1)
                                         -> (is_IntRelType(X2,Y2)
                                             -> (is_IntPropLevel(X3,Y3)
                                                 -> gecode_constraint_rel_170(Y0,Y1,Y2,Y3)
                                                 ;  (is_IntVar(X3,Y3)
                                                     -> gecode_constraint_rel_171(Y0,Y1,Y2,Y3)
                                                     ;  (is_IntArgs(X3,Y3)
                                                         -> gecode_constraint_rel_173(Y0,Y1,Y2,Y3)
                                                         ;  (is_IntVarArgs(X3,Y3)
                                                             -> gecode_constraint_rel_175(Y0,Y1,Y2,Y3)
                                                             ;  (is_int(X3,Y3)
                                                                 -> gecode_constraint_rel_177(Y0,Y1,Y2,Y3)
                                                                 ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))))))
                                             ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                                         ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(rel(X0,X1,X2,X3),arg=2)))))))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(rel(X0,X1,X2,X3),arg=1)))).

rel(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVar(X3,Y3)
                     -> (is_IntPropLevel(X4,Y4)
                         -> gecode_constraint_rel_126(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                     ;  (is_int(X3,Y3)
                         -> (is_IntPropLevel(X4,Y4)
                             -> gecode_constraint_rel_128(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                 ;  throw(error(type_error('BoolVarArgs'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_BoolVar(X1,Y1)
                 -> (is_BoolOpType(X2,Y2)
                     -> (is_BoolVar(X3,Y3)
                         -> (is_BoolVar(X4,Y4)
                             -> gecode_constraint_rel_129(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_int(X4,Y4)
                                 -> gecode_constraint_rel_131(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error(int(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))))
                         ;  throw(error(type_error('BoolVar'(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                     ;  (is_IntRelType(X2,Y2)
                         -> (is_BoolVar(X3,Y3)
                             -> (is_IntPropLevel(X4,Y4)
                                 -> gecode_constraint_rel_134(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_Reify(X4,Y4)
                                     -> gecode_constraint_rel_135(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))))
                             ;  (is_int(X3,Y3)
                                 -> (is_IntPropLevel(X4,Y4)
                                     -> gecode_constraint_rel_138(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_Reify(X4,Y4)
                                         -> gecode_constraint_rel_139(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))))
                                 ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                         ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))))
                 ;  (is_FloatVar(X1,Y1)
                     -> (is_FloatRelType(X2,Y2)
                         -> (is_FloatVal(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> gecode_constraint_rel_142(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                             ;  (is_FloatVar(X3,Y3)
                                 -> (is_Reify(X4,Y4)
                                     -> gecode_constraint_rel_144(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                 ;  throw(error(type_error('FloatVar'(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                         ;  throw(error(type_error('FloatRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                     ;  (is_IntVar(X1,Y1)
                         -> (is_IntRelType(X2,Y2)
                             -> (is_IntVar(X3,Y3)
                                 -> (is_IntPropLevel(X4,Y4)
                                     -> gecode_constraint_rel_146(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_Reify(X4,Y4)
                                         -> gecode_constraint_rel_147(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))))
                                 ;  (is_int(X3,Y3)
                                     -> (is_IntPropLevel(X4,Y4)
                                         -> gecode_constraint_rel_150(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_Reify(X4,Y4)
                                             -> gecode_constraint_rel_151(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))))
                                     ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                             ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                         ;  (is_BoolVarArgs(X1,Y1)
                             -> (is_IntRelType(X2,Y2)
                                 -> (is_BoolVar(X3,Y3)
                                     -> (is_IntPropLevel(X4,Y4)
                                         -> gecode_constraint_rel_154(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                     ;  (is_BoolVarArgs(X3,Y3)
                                         -> (is_IntPropLevel(X4,Y4)
                                             -> gecode_constraint_rel_158(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                         ;  (is_IntArgs(X3,Y3)
                                             -> (is_IntPropLevel(X4,Y4)
                                                 -> gecode_constraint_rel_160(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                             ;  (is_int(X3,Y3)
                                                 -> (is_IntPropLevel(X4,Y4)
                                                     -> gecode_constraint_rel_162(Y0,Y1,Y2,Y3,Y4)
                                                     ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                                 ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))))
                                 ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                             ;  (is_IntArgs(X1,Y1)
                                 -> (is_IntRelType(X2,Y2)
                                     -> (is_BoolVarArgs(X3,Y3)
                                         -> (is_IntPropLevel(X4,Y4)
                                             -> gecode_constraint_rel_166(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                         ;  (is_IntVarArgs(X3,Y3)
                                             -> (is_IntPropLevel(X4,Y4)
                                                 -> gecode_constraint_rel_168(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                             ;  throw(error(type_error('IntVarArgs'(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                                     ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                                 ;  (is_IntVarArgs(X1,Y1)
                                     -> (is_IntRelType(X2,Y2)
                                         -> (is_IntVar(X3,Y3)
                                             -> (is_IntPropLevel(X4,Y4)
                                                 -> gecode_constraint_rel_172(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                             ;  (is_IntArgs(X3,Y3)
                                                 -> (is_IntPropLevel(X4,Y4)
                                                     -> gecode_constraint_rel_174(Y0,Y1,Y2,Y3,Y4)
                                                     ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                                 ;  (is_IntVarArgs(X3,Y3)
                                                     -> (is_IntPropLevel(X4,Y4)
                                                         -> gecode_constraint_rel_176(Y0,Y1,Y2,Y3,Y4)
                                                         ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                                     ;  (is_int(X3,Y3)
                                                         -> (is_IntPropLevel(X4,Y4)
                                                             -> gecode_constraint_rel_178(Y0,Y1,Y2,Y3,Y4)
                                                             ;  throw(error(type_error('IntPropLevel'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                                         ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))))
                                         ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                                     ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=2))))))))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=1)))).

rel(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_BoolOpType(X2,Y2)
                 -> (is_BoolVar(X3,Y3)
                     -> (is_BoolVar(X4,Y4)
                         -> (is_IntPropLevel(X5,Y5)
                             -> gecode_constraint_rel_130(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  (is_int(X4,Y4)
                             -> (is_IntPropLevel(X5,Y5)
                                 -> gecode_constraint_rel_132(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error(int(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))))
                     ;  throw(error(type_error('BoolVar'(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  (is_IntRelType(X2,Y2)
                     -> (is_BoolVar(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> (is_IntPropLevel(X5,Y5)
                                 -> gecode_constraint_rel_136(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  (is_int(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> (is_IntPropLevel(X5,Y5)
                                     -> gecode_constraint_rel_140(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4)))))
                     ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3)))))
             ;  (is_IntVar(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> (is_IntPropLevel(X5,Y5)
                                 -> gecode_constraint_rel_148(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  (is_int(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> (is_IntPropLevel(X5,Y5)
                                     -> gecode_constraint_rel_152(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(error(type_error('IntPropLevel'(X5)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  throw(error(type_error('Reify'(X4)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(error(type_error(int(X3)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4)))))
                     ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3))))
                 ;  throw(error(type_error('IntVar'(X1)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=2)))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=1)))).

rel(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> gecode_constraint_rel_155(Y0,Y1,Y2)
                 ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2),arg=3))))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> gecode_constraint_rel_169(Y0,Y1,Y2)
                     ;  throw(error(type_error('IntRelType'(X2)),gecode_argument_error(rel(X0,X1,X2),arg=3))))
                 ;  throw(error(type_error('IntVarArgs'(X1)),gecode_argument_error(rel(X0,X1,X2),arg=2)))))
         ;  throw(error(type_error('Space'(X0)),gecode_argument_error(rel(X0,X1,X2),arg=1)))).

