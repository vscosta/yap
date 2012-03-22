# This file was automatically extracted from Gecode source files.
# It is subject to the same Copyright as the source files from which
# it is derived, and is distributed under the same Licensing conditions.
ENUM_CLASSES = []

class ScriptMode(object):
    TYPE = 'ScriptMode'
    ENUM = ['SM_SOLUTION','SM_TIME','SM_STAT','SM_GIST']

ENUM_CLASSES.append(ScriptMode)

class IntRelType(object):
    TYPE = 'IntRelType'
    ENUM = ['IRT_EQ','IRT_NQ','IRT_LQ','IRT_LE','IRT_GQ','IRT_GR']

ENUM_CLASSES.append(IntRelType)

class BoolOpType(object):
    TYPE = 'BoolOpType'
    ENUM = ['BOT_AND','BOT_OR','BOT_IMP','BOT_EQV','BOT_XOR']

ENUM_CLASSES.append(BoolOpType)

class IntConLevel(object):
    TYPE = 'IntConLevel'
    ENUM = ['ICL_VAL','ICL_BND','ICL_DOM','ICL_DEF']

ENUM_CLASSES.append(IntConLevel)

class TaskType(object):
    TYPE = 'TaskType'
    ENUM = ['TT_FIXP','TT_FIXS','TT_FIXE']

ENUM_CLASSES.append(TaskType)

class ExtensionalPropKind(object):
    TYPE = 'ExtensionalPropKind'
    ENUM = ['EPK_DEF','EPK_SPEED','EPK_MEMORY']

ENUM_CLASSES.append(ExtensionalPropKind)

class IntVarBranch(object):
    TYPE = 'IntVarBranch'
    ENUM = ['INT_VAR_NONE','INT_VAR_RND','INT_VAR_DEGREE_MIN','INT_VAR_DEGREE_MAX','INT_VAR_AFC_MIN','INT_VAR_AFC_MAX','INT_VAR_MIN_MIN','INT_VAR_MIN_MAX','INT_VAR_MAX_MIN','INT_VAR_MAX_MAX','INT_VAR_SIZE_MIN','INT_VAR_SIZE_MAX','INT_VAR_SIZE_DEGREE_MIN','INT_VAR_SIZE_DEGREE_MAX','INT_VAR_SIZE_AFC_MIN','INT_VAR_SIZE_AFC_MAX','INT_VAR_REGRET_MIN_MIN','INT_VAR_REGRET_MIN_MAX','INT_VAR_REGRET_MAX_MIN','INT_VAR_REGRET_MAX_MAX']

ENUM_CLASSES.append(IntVarBranch)

class IntValBranch(object):
    TYPE = 'IntValBranch'
    ENUM = ['INT_VAL_MIN','INT_VAL_MED','INT_VAL_MAX','INT_VAL_RND','INT_VAL_SPLIT_MIN','INT_VAL_SPLIT_MAX','INT_VAL_RANGE_MIN','INT_VAL_RANGE_MAX','INT_VALUES_MIN','INT_VALUES_MAX']

ENUM_CLASSES.append(IntValBranch)

class IntAssign(object):
    TYPE = 'IntAssign'
    ENUM = ['INT_ASSIGN_MIN','INT_ASSIGN_MED','INT_ASSIGN_MAX','INT_ASSIGN_RND']

ENUM_CLASSES.append(IntAssign)

class ViewSelStatus(object):
    TYPE = 'ViewSelStatus'
    ENUM = ['VSS_BEST','VSS_BETTER','VSS_TIE','VSS_WORSE']

ENUM_CLASSES.append(ViewSelStatus)

class ExecStatus(object):
    TYPE = 'ExecStatus'
    ENUM = ['__ES_SUBSUMED','ES_FAILED','ES_NOFIX','ES_OK','ES_FIX','ES_NOFIX_FORCE','__ES_PARTIAL']

ENUM_CLASSES.append(ExecStatus)

class ActorProperty(object):
    TYPE = 'ActorProperty'
    ENUM = ['AP_DISPOSE','AP_WEAKLY']

ENUM_CLASSES.append(ActorProperty)

class SpaceStatus(object):
    TYPE = 'SpaceStatus'
    ENUM = ['SS_FAILED','SS_SOLVED','SS_BRANCH']

ENUM_CLASSES.append(SpaceStatus)

class SetRelType(object):
    TYPE = 'SetRelType'
    ENUM = ['SRT_EQ','SRT_NQ','SRT_SUB','SRT_SUP','SRT_DISJ','SRT_CMPL','SRT_LQ','SRT_LE','SRT_GQ','SRT_GR']

ENUM_CLASSES.append(SetRelType)

class SetOpType(object):
    TYPE = 'SetOpType'
    ENUM = ['SOT_UNION','SOT_DUNION','SOT_INTER','SOT_MINUS']

ENUM_CLASSES.append(SetOpType)

class SetVarBranch(object):
    TYPE = 'SetVarBranch'
    ENUM = ['SET_VAR_NONE','SET_VAR_RND','SET_VAR_DEGREE_MIN','SET_VAR_DEGREE_MAX','SET_VAR_AFC_MIN','SET_VAR_AFC_MAX','SET_VAR_MIN_MIN','SET_VAR_MIN_MAX','SET_VAR_MAX_MIN','SET_VAR_MAX_MAX','SET_VAR_SIZE_MIN','SET_VAR_SIZE_MAX','SET_VAR_SIZE_DEGREE_MIN','SET_VAR_SIZE_DEGREE_MAX','SET_VAR_SIZE_AFC_MIN','SET_VAR_SIZE_AFC_MAX']

ENUM_CLASSES.append(SetVarBranch)

class SetValBranch(object):
    TYPE = 'SetValBranch'
    ENUM = ['SET_VAL_MIN_INC','SET_VAL_MIN_EXC','SET_VAL_MED_INC','SET_VAL_MED_EXC','SET_VAL_MAX_INC','SET_VAL_MAX_EXC','SET_VAL_RND_INC','SET_VAL_RND_EXC']

ENUM_CLASSES.append(SetValBranch)

class SetAssign(object):
    TYPE = 'SetAssign'
    ENUM = ['SET_ASSIGN_MIN_INC','SET_ASSIGN_MIN_EXC','SET_ASSIGN_MED_INC','SET_ASSIGN_MED_EXC','SET_ASSIGN_MAX_INC','SET_ASSIGN_MAX_EXC','SET_ASSIGN_RND_INC','SET_ASSIGN_RND_EXC']

ENUM_CLASSES.append(SetAssign)

