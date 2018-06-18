# -*- python -*-
#==============================================================================
# Copyright (C) 2011 by Denys Duchier
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.
#==============================================================================

import re

NOTICE_CC = """// -*- c++ -*-
//=============================================================================
// Copyright (C) 2011 by Denys Duchier
//
// This program is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//=============================================================================
"""

NOTICE_PROLOG="""%% -*- prolog -*-
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
"""

def prolog_print_notice():
    print(NOTICE_PROLOG)

def cc_print_notice():
    print(NOTICE_CC)

def islegal(a):
    return a[0].islower() and a.replace("_","a").isalnum() or a[0] == '\'' and a[len(a)-1] == '\''

class Type(object):

    DEFAULT   = re.compile("""^(.+)=(.+)$""")
    CONST     = re.compile("""^const\\b(.+)$""")
    UNSIGNED  = re.compile("""^unsigned\\b(.+)$""")
    REFERENCE = re.compile("""^(.+)&$""")

    def __init__(self, text):
        # type: (object) -> object
        if isinstance(text, Type):
            self.clone_from(text)
            return
        text = text.strip()
        m = self.DEFAULT.match(text)
        if m:
            self.default = m.group(2).strip()
            text = m.group(1).strip()
        else:
            self.default = None
        m = self.CONST.match(text)
        if m:
            self.const = True
            text = m.group(1).strip()
        else:
            self.const = False
        m = self.UNSIGNED.match(text)
        if m:
            self.unsigned = True
            text = m.group(1).strip()
        else:
            self.unsigned = False
        m = self.REFERENCE.match(text)
        if m:
            self.reference = True
            text = m.group(1).strip()
        else:
            self.reference = False
        self.type = text

    def __str__(self):
        l = []
        if self.const: l.append("const ")
        if self.unsigned: l.append("unsigned ")
        l.append(self.type)
        if self.reference: l.append("&")
        if self.default is not None:
            l.append("=")
            l.append(self.default)
        return ''.join(l)

    def clone_from(self, other):
        self.const = other.const
        self.unsigned = other.unsigned
        self.type = other.type
        self.reference = other.reference
        self.default = other.default

    def clone(self):
        return type(self)(self)


class Constraint(object):

    DECL = re.compile("""^([^(]+)\\b(\w+)\((.*)\);$""")
    ARG  = re.compile("""((?:[^,<(]|<[^>]*>|\([^)]*\))+),?""")

    def __init__(self, line):
        # type: (object) -> object
        if isinstance(line, Constraint):
            self.clone_from(line)
            return
        line = line.strip()
        m = self.DECL.match(line)
        self.rettype = Type(m.group(1).strip())
        self.name = m.group(2)
        argtypes = []
        for x in self.ARG.finditer(m.group(3).strip()):
            argtypes.append(Type(x.group(1)))
        self.argtypes = tuple(argtypes)
        self.api = None

    def __str__(self):
        l = []
        l.append(str(self.rettype))
        l.append(" ")
        l.append(self.name)
        sep = "("
        for x in self.argtypes:
            l.append(sep)
            sep = ", "
            l.append(str(x))
        l.append(")")
        if self.api is not None:
            l.append(" -> ")
            l.append(self.api)
        l.append(";")
        return ''.join(l)

    def clone_from(self, other):
        self.rettype = other.rettype.clone()
        self.name = other.name
        self.argtypes = tuple(t.clone() for t in other.argtypes)

    def clone(self):
        return type(self)(self)


COMMENT = re.compile("""^\\s*//.*$""")

def load_decls(filename):
    decls = []
    for line in open(filename):
        line = line.strip()
        if not line: continue
        m = COMMENT.match(line)
        if m: continue
        decls.append(Constraint(line))
    return decls

class DeclsLoader(object):

    def __init__(self, filename):
        # type: (object) -> object
        self.decls = load_decls(filename)

    def print_decls(self):
        for con in self.decls:
            print(str(con))

class PredGenerator(DeclsLoader):

    OMIT = ()
        # "VarBranchOptions",
        #     "ValBranchOptions",
        #     "TieBreakVarBranch<IntVarBranch>",
        #     "TieBreak<IntVarBranch>",
        #     "TieBreak<FloatVarBranch>",
        #     "TieBreak<SetVarBranch>",
        #     "TieBreak<BoolVarBranch>",
        #     "TieBreakVarBranchOptions",
        #     "TieBreakVarBranch<SetVarBranch>")

    def __init__(self, filename):
        # type: (object) -> object
        super(PredGenerator, self).__init__(filename)
        self._change_home_to_space()
        self._change_intsharedarray_to_intargs()
        self._generate()
        self._number()

    def _change_home_to_space(self):
        for p in self.decls:
            for t in p.argtypes:
                if t.type=="Home":
                    t.type="Space"

    def _change_intsharedarray_to_intargs(self):
        for p in self.decls:
            for t in p.argtypes:
                if t.type=="IntSharedArray":
                    t.type="IntArgs"

    def _generate(self):
        # drop the constraints and optional arguments we can't handle
        preds = []
        for con in self.decls:
            if self._con_ok(con):
                con = con.clone()
                con.argtypes = tuple(self._drop_deco(t) for t in con.argtypes
                                     if t.type not in self.OMIT)
                preds.append(con)
        # for each pred that has an argument with a default produce
        # 2 preds (1 without, 1 with).  repeat until all defaults have
        # been removed.
        again = True
        while again:
            preds_ = []
            again = False
            for con in preds:
                i = self._defaulted(con.argtypes)
                if i is None:
                    preds_.append(con)
                else:
                    again = True
                    before = con.argtypes[:i]
                    # without the default argument
                    # and therefore without the args that follow
                    con1 = con.clone()
                    con1.argtypes = before
                    preds_.append(con1)
                    # with the default argument (not default anymore)
                    con2 = con.clone()
                    arg = con.argtypes[i].clone()
                    arg.default=None
                    after = con.argtypes[i+1:]
                    con2.argtypes = before + (arg,) + after
                    preds_.append(con2)
            preds = preds_
        self.preds = preds

    def _con_ok(self, con):
        for t in con.argtypes:
            if (t.type in self.OMIT) and (t.default is None):
                return False
        return True

    def _drop_deco(self, t):
        # drop const, ref, and unsigned indications
        t.const = False
        t.reference = False
        t.unsigned = False
        return t

    def _defaulted(self, argtypes):
        i = 0
        for x in argtypes:
            if x.default is not None:
                return i
            i += 1
        return None

    def _number(self):
        i = 1
        for x in self.preds:
            x.api = "%s_%d" % (x.name,i)
            i += 1

    def print_preds(self):
        for p in self.preds:
            print(str(p))

class Cluster(object):

    def __init__(self, name, arity):
        # type: (object, object) -> object
        self.name = name
        self.arity = arity
        self.preds = []

# discriminating tree based on argument types
class DTree(object):

    def __init__(self, i, preds, cluster):
        # type: (object, object, object) -> object
        self.index = i
        self.cluster = cluster
        if len(preds) == 1 and len(preds[0].argtypes) == i:
            self.is_leaf = True
            self.pred = preds[0]
            return
        self.is_leaf = False
        # i is the index of the current arg
        # preds are the predicates to be indexed under this tree node
        dispatch = {}
        for p in preds:
            t = p.argtypes[i]
            d = dispatch.get(t.type, None)
            if d is None:
                d = []
                dispatch[t.type] = d
            d.append(p)
        self.subtrees = tuple((t2,DTree(i+1,p2,cluster))
                              for t2,p2 in dispatch.items())

    def _generate_body(self, user_vars, lib_vars):
        if self.is_leaf:
            return PrologLiteral("gecode_constraint_%s(%s)" % (self.pred.api, ",".join(lib_vars)))
        X = user_vars[self.index]
        Y = lib_vars[self.index]
        return self._generate_dispatch(0, user_vars, lib_vars)

    def _generate_dispatch(self, i, user_vars, lib_vars):
        if i == len(self.subtrees):
            ty = self.subtrees[i-1][0]
            name = self.cluster.name
            if not islegal(ty):
                ty = "\'"+ty+"\'"
            return PrologLiteral("throw(error(type_error(%s(X%d)),gecode_argument_error(%s(%s),arg=%d)))"  % (ty, self.index, name, ",".join(user_vars), self.index+1))
        typ, dtree = self.subtrees[i]
        idx = self.index
        X = user_vars[idx]
        Y = lib_vars[idx]
        if typ=="Space":
            typ = "Space_or_Clause"
        elif typ.startswith("std::function") and typ.endswith(")>"):
            typ = "std_function"
        elif typ.endswith(">"):
            sp = typ.split("<")
            if len(sp) > 1:
                typ = sp[1].rstrip(">")
        return PrologIF(
                    PrologLiteral("is_%s(%s,%s)" % (typ,X,Y)),
                    dtree._generate_body(user_vars, lib_vars),
                    self._generate_dispatch(i+1, user_vars, lib_vars))

    def _cc_descriptors(self, name, argtypes):
        if self.is_leaf:
            return (CCDescriptor(name, argtypes, self.pred.api),)
        descriptors = []
        for typ,dtree in self.subtrees:
            descriptors.extend(dtree._cc_descriptors(name,(argtypes+(typ,))))
        return descriptors

class YAPConstraintGeneratorBase(PredGenerator):

    def __init__(self, filename):
        # type: (object) -> object
        super(YAPConstraintGeneratorBase, self).__init__(filename)
        self._classify()
        self._dtreefy()

    # create clusters (predicate,arity)
    def _classify(self):
        clusters = {}
        for pred in self.preds:
            name = pred.name
            arity = len(pred.argtypes)
            key = (name,arity)
            cluster = clusters.get(key, None)
            if cluster is None:
                cluster = Cluster(name, arity)
                clusters[key] = cluster
            cluster.preds.append(pred)
        self.clusters = clusters

    # for each cluster, create a dtree
    def _dtreefy(self):
        dtrees = {}
        for key, cluster in self.clusters.items():
            dtree = DTree(0, cluster.preds, cluster)
            dtrees[key] = dtree
        self.dtrees = dtrees

    def _user_vars(self, arity):
        return tuple(("X%d" % i) for i in range(arity))

    def _lib_vars(self, arity):
        return tuple(("Y%d" % i) for i in range(arity))

class YAPConstraintPrologGenerator(YAPConstraintGeneratorBase):

    def __init__(self, filename):
        # type: (object) -> object
        super(YAPConstraintPrologGenerator, self).__init__(filename)

    def _prolog_clauses(self):
        clauses = []
        for (name, arity), dtree in self.dtrees.items():
            user_vars = self._user_vars(arity)
            lib_vars = self._lib_vars(arity)
            head = "%s(%s)" % (name, ",".join(user_vars))
            body = dtree._generate_body(user_vars, lib_vars)
            clause = PrologClause(head, body)
            clauses.append(clause)
        return clauses

    def generate(self):
        out = OStream(sys.stdout)
        for clause in self._prolog_clauses():
            clause.pp(out, 0)

class YAPConstraintCCGenerator(YAPConstraintGeneratorBase):

    def __init__(self, filename):
        # type: (object) -> object
        super(YAPConstraintCCGenerator, self).__init__(filename)

    def _cc_descriptors(self):
        descriptors = []
        for (name, arity), dtree in self.dtrees.items():
            descriptors.extend(dtree._cc_descriptors(name,()))
        return descriptors

    def generate_impl(self):
        for d in self._cc_descriptors():
            d.generate_impl()

    def generate_init(self):
        for d in self._cc_descriptors():
            d.generate_init()

# output stream that keeps track of the current column
# to facilitate proper indentation

import sys

class OStream(object):

    def __init__(self, fd=sys.stdout):
        # type: (object) -> object
        self.file = fd
        self.column = 0

    def write(self, s):
        reset = False
        for x in s.split('\n'):
            if reset:
                self.newline()
            else:
                reset = True
            self.file.write(x)
            self.column += len(x)

    def newline(self):
        self.file.write("\n")
        self.column = 0

    def writeln(self, s=None):
        if s is not None:
            self.write(s)
        self.newline()

    def indent_to(self, n):
        if n<self.column:
            self.newline()
        n = n - self.column
        while n>0:
            self.write(' ')
            n -= 1

# representation of prolog code that automatically supports proper indentation

class PrologObject(object):
    pass

class PrologClause(PrologObject):

    def __init__(self, head, body):
        # type: (object, object) -> object
        self.head = head
        self.body = body

    def pp(self, out, offset):
        out.indent_to(offset)
        out.write(self.head)
        out.writeln(" :-")
        self.body.pp(out, offset+8)
        out.writeln(".")
        out.writeln()

class PrologLiteral(PrologObject):

    def __init__(self, lit):
        # type: (object) -> object
        self.literal = lit

    def pp(self, out, offset):
        out.indent_to(offset)
        out.write(self.literal)

class PrologIF(PrologObject):

    def __init__(self, cond, left, right):
        # type: (object, object, object) -> object
        self.cond = cond
        self.left = left
        self.right = right

    def pp(self, out, offset):
        out.indent_to(offset)
        out.write("("),
        indent = offset+1
        self.cond.pp(out, indent)
        out.writeln()
        out.indent_to(indent)
        out.write("-> ")
        self.left.pp(out, indent+3)
        out.writeln()
        out.indent_to(indent)
        out.write(";  ")
        self.right.pp(out, indent+3)
        out.write(")")

ENUM_CLASSES = None
ENUM_CLASSES_AVOID = ('ScriptMode','ViewSelStatus','ExecStatus',
                      'ActorProperty','SpaceStatus')

def enum_classes():
    global ENUM_CLASSES
    if ENUM_CLASSES is None:
        filename = "gecode-enums-%s.py" % gecode_version()
        import imp
        ENUM_CLASSES = imp.load_source(
            "gecode_enums",
            filename).ENUM_CLASSES
        ENUM_CLASSES = tuple(x for x in ENUM_CLASSES if x.TYPE not in ENUM_CLASSES_AVOID)
    return ENUM_CLASSES

class YAPEnumImpl(object):

    def generate(self):
        self._generate_atoms()
        self._generate_from_term()

    def _generate_atoms(self):
        for x in self.ENUM:
            print("static YAP_Term gecode_%s;" % x)
        print()

    def _generate_from_term(self):
        t2 = self.TYPE
        print("static %s gecode_%s_from_term(YAP_Term X)" % (self.TYPE,t2))
        print("{")
        for x in self.ENUM:
            print("  if (X==gecode_%s) return %s;" % (x,x))
        print('  cerr << "this should never happen" << endl; exit(1);')
        print("}")
        print()

    def _generate_from_term_forward_decl(self):
        t2 =  self.TYPE
        print("static %s gecode_%s_from_term(YAP_Term);" % (self.TYPE,t2))

class YAPEnumImplGenerator(object):

    def generate(self):
        # generate_space_function_forward()
        for c in enum_classes():
            class C(c,YAPEnumImpl): pass
            o = C()
            o.generate()

class YAPEnumForwardGenerator(object):

    def generate(self):
        # gsenerate_space_function_forward()
        for c in enum_classes():
            class C(c,YAPEnumImpl): pass
            o = C()
            o._generate_from_term_forward_decl()

class YAPEnumInit(object):

    def generate(self):
        for x in self.ENUM:
            print('{ YAP_Atom X= YAP_LookupAtom("%s");' % x)
            print('  gecode_%s = YAP_MkAtomTerm(X);' % x)
            print('  YAP_AtomGetHold(X); }')
        print()

class YAPEnumInitGenerator(object):

    def generate(self):
        for c in enum_classes():
            class C(c,YAPEnumInit): pass
            o = C()
            o.generate()

class YAPEnumProlog(object):

    def generate(self):
        t = self.TYPE
        if t.startswith("std::function") and t.endswith(")>"):
            t = "std_function"
        elif t.endswith(">"):
            sp = t.split("<")
            if len(sp) > 1:
                t = sp[1].rstrip(">")
        for x in self.ENUM:
            print("is_%s_('%s')." % (t, x))
        print("\n")
        for x in self.ENUM:
            print("is_%s_('%s','%s')." % (t, x, x))
        print("\n")
        print("is_%s(X,Y) :- nonvar(X), is_%s_(X,Y)." % (t,t))
        print("is_%s(X) :- is_%s_(X,_)." % (t,t))
        print("\n")

class YAPEnumPrologGenerator(object):

    def generate(self):
        for c in enum_classes():
            class C(c,YAPEnumProlog): pass
            o = C()
            o.generate()

class CCDescriptor(object):

    def __init__(self, name, argtypes, api):
        # type: (object, object, object) -> object
        self.name = name
        self.argtypes = argtypes
        self.api = api

    def generate_impl(self):
        print("static YAP_Bool gecode_constraint_%s(void)" % self.api)
        print("{")
        i = 1
        args = []
        has_space = False
        for t in self.argtypes:
            v = "X%d" % i
            v2 = v
            a = "YAP_ARG%d" % i
            if t=="Space":
                v = "*space"
                print("  GenericSpace* space = gecode_Space_from_term(%s);" % (a))
                has_space = True
                v2 = v
            else:
                t2 = t
                if t.startswith("std::function") and t.endswith(")>"):
                    sp = t.split("&")
                    if len(sp) > 1:
                        t2 = "std_function"
                        v2 = v #sp[0]+"("+v+")"
                elif t.endswith(">"):
                    sp = t.split("<")
                    if len(sp) > 1:
                        t2 = sp[1].rstrip(">")
                        v2 = v #sp[0]+"("+v+")"
                extra = ""
                if t in ("IntVar","BoolVar","SetVar","FloatVar","IntVarArgs","BoolVarArgs","SetVarArgs","FloatVarArgs","std_function"):
                    extra = "space,"
                    if has_space == False:
                        print("  GenericSpace* space = gecode_Space_from_term(%s);" % a)
                        has_space = True
                print("  %s %s = gecode_%s_from_term(%s%s);" % (t,v,t2,extra,a))
            args.append(v)
            i += 1
        print("  %s(%s);" % (self.name, ",".join(args)))
        print("  return TRUE;")
        print("}")
        print()

    def generate_init(self):
        print('YAP_UserCPredicate("gecode_constraint_%s", gecode_constraint_%s, %d);' \
            % (self.api, self.api, len(self.argtypes)))

GECODE_VERSION = None

def gecode_version():
    #import pdb; pdb.set_trace()
    global GECODE_VERSION
    if GECODE_VERSION is not None:
        return GECODE_VERSION
    from distutils.ccompiler import new_compiler
    try:
        from distutils.ccompiler import customize_compiler
    except:
        from distutils.sysconfig import customize_compiler
    import os
    cxx = new_compiler()
    customize_compiler(cxx)
    pid = os.getpid()
    file_hh = "_gecode_version_%d.hh" % pid
    file_txt = "_gecode_version_%d.txt" % pid
    f = open(file_hh,"w")
    f.write("""#include "gecode/support/config.hpp"
@@GECODE_VERSION""")
    f.close()
    cxx.preprocess(file_hh,output_file=file_txt)
    if True:
        f = open(file_txt)
        version = ""
        for line in f:
            if line.startswith("@@"):
                version = line[3:-2]
                break
        f.close()
        os.remove(file_hh)
        os.remove(file_txt)
    else:
        version = "6.0.0"
    GECODE_VERSION = version
    return version

def generate_files():
    DIR = "../" + gecode_version()
    import os, os.path
    DIR = os.path.abspath(DIR)
    if not os.path.exists(DIR):
        os.mkdir(DIR)
    filename = "gecode-prototypes-" + gecode_version() + ".hh"
    import sys
    stdout = sys.stdout
    try:
        sys.stdout = open(os.path.join(DIR,"gecode-version.txt"),"w")
        print(gecode_version())
        sys.stdout.close()
        sys.stdout = open(os.path.join(DIR,"gecode_yap_auto_generated.yap"),"w")
        prolog_print_notice()
        YAPEnumPrologGenerator().generate()
        YAPConstraintPrologGenerator(filename).generate()
        sys.stdout.close()
        sys.stdout = open(os.path.join(DIR,"gecode_yap_cc_impl_auto_generated.icc"),"w")
        cc_print_notice()
        YAPEnumImplGenerator().generate()
        YAPConstraintCCGenerator(filename).generate_impl()
        sys.stdout.close()
        sys.stdout = open(os.path.join(DIR,"gecode_yap_cc_init_auto_generated.icc"),"w")
        cc_print_notice()
        YAPEnumInitGenerator().generate()
        YAPConstraintCCGenerator(filename).generate_init()
        sys.stdout.close()
        sys.stdout = open(os.path.join(DIR,"gecode_yap_cc_forward_auto_generated.icc"),"w")
        cc_print_notice()
        YAPEnumForwardGenerator().generate()
        sys.stdout.close()
    finally:
        sys.stdout = stdout

if __name__ == '__main__':
    generate_files()
