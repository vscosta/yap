"""Global yap_ipython app to support test running.

We must start our own ipython object and heavily muck with it so that all the
modifications yap_ipython makes to system behavior don't send the doctest machinery
into a fit.  This code should be considered a gross hack, but it gets the job
done.
"""

# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import builtins as builtin_mod
import sys
import types
import warnings

from . import tools

from yap_ipython.core import page
from yap_ipython.utils import io
from yap_ipython.terminal.interactiveshell import TerminalInteractiveShell


class StreamProxy(io.IOStream):
    """Proxy for sys.stdout/err.  This will request the stream *at call time*
    allowing for nose's Capture plugin's redirection of sys.stdout/err.

    Parameters
    ----------
    name : str
        The name of the stream. This will be requested anew at every call
    """

    def __init__(self, name):
        warnings.warn("StreamProxy is deprecated and unused as of yap_ipython 5", DeprecationWarning,
            stacklevel=2,
        )
        self.name=name

    @property
    def stream(self):
        return getattr(sys, self.name)

    def flush(self):
        self.stream.flush()


def get_ipython():
    # This will get replaced by the real thing once we start yap_ipython below
    return start_ipython()


# A couple of methods to override those in the running yap_ipython to interact
# better with doctest (doctest captures on raw stdout, so we need to direct
# various types of output there otherwise it will miss them).

def xsys(self, cmd):
    """Replace the default system call with a capturing one for doctest.
    """
    # We use getoutput, but we need to strip it because pexpect captures
    # the trailing newline differently from commands.getoutput
    print(self.getoutput(cmd, split=False, depth=1).rstrip(), end='', file=sys.stdout)
    sys.stdout.flush()


def _showtraceback(self, etype, evalue, stb):
    """Print the traceback purely on stdout for doctest to capture it.
    """
    print(self.InteractiveTB.stb2text(stb), file=sys.stdout)


def start_ipython():
    """Start a global yap_ipython shell, which we need for yap_ipython-specific syntax.
    """
    global get_ipython

    # This function should only ever run once!
    if hasattr(start_ipython, 'already_called'):
        return
    start_ipython.already_called = True

    # Store certain global objects that yap_ipython modifies
    _displayhook = sys.displayhook
    _excepthook = sys.excepthook
    _main = sys.modules.get('__main__')

    # Create custom argv and namespaces for our yap_ipython to be test-friendly
    config = tools.default_config()
    config.TerminalInteractiveShell.simple_prompt = True

    # Create and initialize our test-friendly yap_ipython instance.
    shell = TerminalInteractiveShell.instance(config=config,
                                              )

    # A few more tweaks needed for playing nicely with doctests...

    # remove history file
    shell.tempfiles.append(config.HistoryManager.hist_file)

    # These traps are normally only active for interactive use, set them
    # permanently since we'll be mocking interactive sessions.
    shell.builtin_trap.activate()

    # Modify the yap_ipython system call with one that uses getoutput, so that we
    # can capture subcommands and print them to Python's stdout, otherwise the
    # doctest machinery would miss them.
    shell.system = types.MethodType(xsys, shell)

    shell._showtraceback = types.MethodType(_showtraceback, shell)

    # yap_ipython is ready, now clean up some global state...

    # Deactivate the various python system hooks added by ipython for
    # interactive convenience so we don't confuse the doctest system
    sys.modules['__main__'] = _main
    sys.displayhook = _displayhook
    sys.excepthook = _excepthook

    # So that ipython magics and aliases can be doctested (they work by making
    # a call into a global _ip object).  Also make the top-level get_ipython
    # now return this without recursively calling here again.
    _ip = shell
    get_ipython = _ip.get_ipython
    builtin_mod._ip = _ip
    builtin_mod.get_ipython = get_ipython

    # Override paging, so we don't require user interaction during the tests.
    def nopage(strng, start=0, screen_lines=0, pager_cmd=None):
        if isinstance(strng, dict):
           strng = strng.get('text/plain', '')
        print(strng)
    
    page.orig_page = page.pager_page
    page.pager_page = nopage

    return _ip
