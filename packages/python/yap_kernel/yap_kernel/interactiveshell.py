# -*- coding: utf-8 -*-
"""YAP Stuff for Main IPython class."""

# -----------------------------------------------------------------------------
#  Copyright (C) 2001 Janko Hauser <jhauser@zscout.de>
#  Copyright (C) 2001-2007 Fernando Perez. <fperez@colorado.edu>
#  Copyright (C) 2008-2011  The IPython Development Team
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
# -----------------------------------------------------------------------------

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
import yap4py.yapi
import yap
from io import open as io_open

from pickleshare import PickleShareDB

from traitlets.config.configurable import SingletonConfigurable
from IPython.core import oinspect
from IPython.core import magic
from IPython.core import page
from IPython.core import prefilter
# from IPython.core import shadows
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
from IPython.testing.skipdoctest import skip_doctest
from IPython.utils import PyColorize
from IPython.utils import io
from IPython.utils import py3compat
from IPython.utils import openpy
from IPython.utils.decorators import undoc
from IPython.utils.io import ask_yes_no
from IPython.utils.ipstruct import Struct
from IPython.paths import get_ipython_dir
from IPython.utils.path import get_home_dir, get_py_filename, ensure_dir_exists
from IPython.utils.py3compat import (builtin_mod, unicode_type, string_types, with_metaclass, iteritems)
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
from collections import namedtuple

use_module = namedtuple('use_module', 'file')
bindvars = namedtuple('bindvars', 'list')
library = namedtuple('library', 'list')
v = namedtuple('_', 'slot')
load_files = namedtuple('load_files', 'file ofile args')
python_query= namedtuple('python_query', 'query_mgr string')
jupyter_query = namedtuple('jupyter_query', 'self string')
enter_cell = namedtuple('enter_cell', 'self' )
exit_cell = namedtuple('exit_cell', 'self' )

class YAPInteraction:
    """An enhanced, interactive shell for YAP."""

    def __init__(self, shell, **kwargs):
        # type: (object, object) -> object
        try:
            if self.yapeng:
                return
        except Exception:
            pass
        pjoin = os.path.join
        here = os.path.abspath(os.path.dirname(__file__))
        yap_lib_path = pjoin(here, "../yap4py/prolog")
        #friend_path = os.path.abspath(pjoin(here, "../yap4py/prolog"))
        yap_dll_path = pjoin(here, "../yap4py")
        self.args = yap.YAPEngineArgs()
        self.args.setYapLibDir(yap_dll_path)
        self.args.setYapShareDir(yap_lib_path)
        # args.setYapPrologBootFile(os.path.join(yap_lib_path."startup.yss"))
        self.yapeng = yap.YAPEngine(self.args)
        self.q = None
        self.shell = shell
        self.run = False
        self.yapeng.goal(use_module(pjoin(here, 'prolog/jupyter.yap')))
        self.os = ""
        self.status = None

    def run_cell(self, s, store_history=True, silent=False,
                 shell_futures=True):
        """Run a complete IPython cell.

        Parameters
                   ----------
                   raw_cell : str
                   The code (including IPython code such as
                   %magic functions) to run.
                   store_history : bool
          If True, the raw and translated cell will be stored in IPython's
                   history. For user code calling back into
                   IPython's machinery, this
                   should be set to False.
                   silent : bool
          If True, avoid side-effects, such as implicit displayhooks and
                   and logging.  silent=True forces store_history=False.
                   shell_futures : bool
          If True, the code will share future statements with the interactive
                   shell. It will both be affected by previous
                    __future__ imports, and any __future__ imports in the code
                     will affect the shell. If False,
                   __future__ imports are not shared in either direction.

        Returns

                   -------

`result : :class:`ExecutionResult`
                   """

        # construct a query from a one-line string
        # q is opaque to Python
        # vs is the list of variables
        # you can print it out, the left-side is the variable name,
        # the right side wraps a handle to a variable
        # pdb.set_trace()
        #     #pdb.set_trace()
        # atom match either symbols, or if no symbol exists, strings, In this case
        # variable names should match strings
        # ask = True
        # launch the query
        result = ExecutionResult()
        if not s or s.isspace():
            self.shell.last_execution_succeeded = True
            return result

        if store_history:
            result.execution_count = self.shell.execution_count
        self.shell.execution_count += 1

        try:
            self.bindings = dict = {}
            state =self.jupyter_query(s, dict)
            if state:
                print("yes")
                self.shell.last_execution_succeeded = True
                result.result = (True, dict)
            else:
                print("no")
                self.shell.last_execution_succeeded = True
                result.result = (True, {})
        except Exception as e:
            print(e)
            self.shell.last_execution_succeeded = False
            result.result = False

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

    def jupyter_query(self, s, bindings):
        # import pdb; pdb.set_trace()
        #
        # construct a self.query from a one-line string
        # self.q is opaque to Python
        self.bindings = {}
        self.status = "call"
        self.yapeng.goal(enter_cell(self))
        if self.q and s != self.os:
            self.q.close()
            self.q = None
        if not self.q:
            self.q = self.yapeng.query(jupyter_query(self, s))
            self.os = s
        # vs is the list of variables
        # you can print it out, the left-side is the variable name,
        # the right side wraps a handle to a variable
        # pdb.set_trace()
        #     #pdb.set_trace()
        # atom match either symbols, or if no symbol exists, sttrings, In this case
        # variable names should match strings
        #for eq in vs:
        #    if not isinstance(eq[0],str):
        #        print( "Error: Variable Name matches a Python Symbol")
        #        return
        ask = True
        # launch the query
        if self.answer(self.q):
            # deterministic = one solution
            if self.status == "exit":
                # done
                self.q.close()
                self.q = None
                self.os = ""
            print("yes")
            self.yapeng.goal(exit_cell(self))
            return True, self.bindings
        print("No (more) answers")
        self.q.close()
        self.q = None
        self.yapeng.goal(exit_cell(self))
        return True, None

    def answer(self, q):
        try:
            return q.next()
        except Exception as e:
            print(e.args[1])
            self.yapeng.goal(exit_cell(self))
            return False, None
