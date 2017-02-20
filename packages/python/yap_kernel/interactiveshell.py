# -*- coding: utf-8 -*-
"""YAP Stuff for Main IPython class."""

#-----------------------------------------------------------------------------
#  Copyright (C) 2001 Janko Hauser <jhauser@zscout.de>
#  Copyright (C) 2001-2007 Fernando Perez. <fperez@colorado.edu>
#  Copyright (C) 2008-2011  The IPython Development Team
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

from __future__ import absolute_import, print_function

import __future__
import abc
import ast
import atexit
import functools
import os
import re
import runpy
import signal

import sys
import tempfile
import traceback
import types
import subprocess
import warnings
import yap
from io import open as io_open

from pickleshare import PickleShareDB

from traitlets.config.configurable import SingletonConfigurable
from IPython.core import oinspect
from IPython.core import magic
from IPython.core import page
from IPython.core import prefilter
from IPython.core import shadowns
from IPython.core import ultratb
from IPython.core import interactiveshell
from IPython.core.alias import Alias, AliasManager
from IPython.core.autocall import ExitAutocall
from IPython.core.builtin_trap import BuiltinTrap
from IPython.core.events import EventManager, available_events
from IPython.core.compilerop import CachingCompiler, check_linecache_ipython
from IPython.core.debugger import Pdb
from IPython.core.display_trap import DisplayTrap
from IPython.core.displayhook import DisplayHook
from IPython.core.displaypub import DisplayPublisher
from IPython.core.error import InputRejected, UsageError
from IPython.core.extensions import ExtensionManager
from IPython.core.formatters import DisplayFormatter
from IPython.core.history import HistoryManager
from IPython.core.inputsplitter import ESC_MAGIC, ESC_MAGIC2
from IPython.core.logger import Logger
from IPython.core.macro import Macro
from IPython.core.payload import PayloadManager
from IPython.core.prefilter import PrefilterManager
from IPython.core.profiledir import ProfileDir
from IPython.core.usage import default_banner
from IPython.core.interactiveshell import InteractiveShellABC, InteractiveShell, ExecutionResult
from IPython.testing.skipdoctest import skip_doctest_py2, skip_doctest
from IPython.utils import PyColorize
from IPython.utils import io
from IPython.utils import py3compat
from IPython.utils import openpy
from IPython.utils.decorators import undoc
from IPython.utils.io import ask_yes_no
from IPython.utils.ipstruct import Struct
from IPython.paths import get_ipython_dir
from IPython.utils.path import get_home_dir, get_py_filename, ensure_dir_exists
from IPython.utils.process import system, getoutput
from IPython.utils.py3compat import (builtin_mod, unicode_type, string_types,
                                     with_metaclass, iteritems)
from IPython.utils.strdispatch import StrDispatch
from IPython.utils.syspathcontext import prepended_to_syspath
from IPython.utils.text import format_screen, LSString, SList, DollarFormatter
from IPython.utils.tempdir import TemporaryDirectory
from traitlets import (
    Integer, Bool, CaselessStrEnum, Enum, List, Dict, Unicode, Instance, Type,
    observe, default,
)
from warnings import warn
from logging import error

class YAPInteractiveShell:
    """An enhanced, interactive shell for YAP."""

    def __init__(self, kernel):
        self.yapeng = yap.YAPEngine()
        self.q = None
        self.shell = kernel.shell
        self.shell.run_cell = self.run_cell

    def closeq(self):
        if self.q:
            self.q.close()
            self.q = None

    def run_cell(self, s, store_history=True, silent=False, shell_futures=True):

        """Run a complete IPython cell.

        Parameters
        ----------
        raw_cell : str
          The code (including IPython code such as %magic functions) to run.
        store_history : bool
          If True, the raw and translated cell will be stored in IPython's
          history. For user code calling back into IPython's machinery, this
          should be set to False.
        silent : bool
          If True, avoid side-effects, such as implicit displayhooks and
          and logging.  silent=True forces store_history=False.
        shell_futures : bool
          If True, the code will share future statements with the interactive
          shell. It will both be affected by previous __future__ imports, and
          any __future__ imports in the code will affect the shell. If False,
          __future__ imports are not shared in either direction.

        Returns
        -------
        result : :class:`ExecutionResult`
        """

        result = ExecutionResult()

        if (not s) or s.isspace():
            self.shell.last_execution_succeeded = True
            return result

        if store_history:
            result.execution_count = self.shell.execution_count

        def error_before_exec(value):
            result.error_before_exec = value
            self.shell.last_execution_succeeded = False
            return result


        if not self.q:
            try:
                self.q = self.yapeng.query(s)
            except SyntaxError:
                return error_before_exec( sys.exc_info()[1])

        cell = s  # cell has to exist so it can be stored/logged

        # Store raw and processed history
        # if not silent:
        #    self.shell..logger.log(cell, s)

        has_raised = False
        try:
            #f = io.StringIO()
            # with redirect_stdout(f):
            run = self.q.next()
            # print('{0}'.format(f.getvalue()))
            # Execute the user code
            if run:
                myvs = self.q.namedVarsCopy()
                if myvs:
                    i = 0
                    for peq in myvs:
                        name = peq[0]
                        bind = peq[1]
                        if bind.isVar():
                            var = yap.YAPAtom('$VAR')
                            f = yap.YAPFunctor(var, 1)
                            bind.unify(yap.YAPApplTerm(f, (name)))
                        else:
                            i = bind.numberVars(i, True)
                            print(name.text() + " = " + bind.text())
                else:
                    print("yes")
                if self.q.deterministic():
                    self.closeq()
            else:
                print("No (more) answers")
                self.closeq()
        except:
            result.error_in_exec = sys.exc_info()[1]
            # self.showtraceback()
            has_raised = True
            self.closeq()


        self.shell.last_execution_succeeded = not has_raised
        result.result = self.shell.last_execution_succeeded
        print( self.q )
        # Reset this so later displayed values do not modify the
        # ExecutionResult
        # self.displayhook.exec_result = None

        #self.events.trigger('post_execute')
        #if not silent:
        #    self.events.trigger('post_run_cell')

        if store_history:
            # Write output to the database. Does nothing unless
            # history output logging is enabled.
            # self.history_manager.store_output(self.execution_count)
            # Each cell is a *single* input, regardless of how many lines it has
            self.shell.execution_count += 1

        return result
