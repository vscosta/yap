"""
Shim to maintain backwards compatibility with old yap_ipython.terminal.console imports.
"""
# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import sys
from warnings import warn

from yap_ipython.utils.shimmodule import ShimModule, ShimWarning

warn("The `yap_ipython.terminal.console` package has been deprecated since yap_ipython 4.0. "
     "You should import from jupyter_console instead.", ShimWarning)

# Unconditionally insert the shim into sys.modules so that further import calls
# trigger the custom attribute access above

sys.modules['yap_ipython.terminal.console'] = ShimModule(
    src='yap_ipython.terminal.console', mirror='jupyter_console')
