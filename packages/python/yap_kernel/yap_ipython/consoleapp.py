"""
Shim to maintain backwards compatibility with old yap_ipython.consoleapp imports.
"""
# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

from warnings import warn

warn("The `yap_ipython.consoleapp` package has been deprecated. "
     "You should import from jupyter_client.consoleapp instead.")

from jupyter_client.consoleapp import *
