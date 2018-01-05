"""
This module has been deprecated since yap_ipython 6.0.

Wrapper around linecache which decodes files to unicode according to PEP 263.
"""
import functools
import linecache
from warnings import warn

getline = linecache.getline

# getlines has to be looked up at runtime, because doctests monkeypatch it.
@functools.wraps(linecache.getlines)
def getlines(filename, module_globals=None):
    """
    Deprecated since yap_ipython 6.0
    """
    warn(("`yap_ipython.utils.ulinecache.getlines` is deprecated since"
          " yap_ipython 6.0 and will be removed in future versions."),
         DeprecationWarning, stacklevel=2)
    return linecache.getlines(filename, module_globals=module_globals)
