# This file was automatically extracted from Gecode source files.
# It is subject to the same Copyright as the source files from which
# it is derived, and is distributed under the same Licensing conditions.
ENUM_CLASSES = []

class ScriptMode(object):
    TYPE = 'ScriptMode'
    ENUM = ['SM_SOLUTION','SM_TIME','SM_STAT','SM_GIST']

ENUM_CLASSES.append(ScriptMode)

class RestartMode(object):
    TYPE = 'RestartMode'
    ENUM = ['RM_NONE','RM_CONSTANT','RM_LINEAR','RM_LUBY','RM_GEOMETRIC']

ENUM_CLASSES.append(RestartMode)

class FloatRelType(object):
    TYPE = 'FloatRelType'
    ENUM = ['FRT_EQ','FRT_NQ','FRT_LQ','FRT_LE','FRT_GQ','FRT_GR']

ENUM_CLASSES.append(FloatRelType)

class ReifyMode(object):
    TYPE = 'ReifyMode'
    ENUM = ['RM_EQV','RM_IMP','RM_PMI']

ENUM_CLASSES.append(ReifyMode)

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

