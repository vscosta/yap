"""
Shim to maintain backwards compatibility with old yap_ipython.qt imports.
"""
# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import sys
from warnings import warn

from yap_ipython.utils.shimmodule import ShimModule, ShimWarning

warn("The `yap_ipython.qt` package has been deprecated since yap_ipython 4.0. "
     "You should import from qtconsole instead.", ShimWarning)

# Unconditionally insert the shim into sys.modules so that further import calls
# trigger the custom attribute access above

_console = sys.modules['yap_ipython.qt.console'] = ShimModule(
    src='yap_ipython.qt.console', mirror='qtconsole')

_qt = ShimModule(src='yap_ipython.qt', mirror='qtconsole')

_qt.console = _console
sys.modules['yap_ipython.qt'] = _qt
