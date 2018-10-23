"""
Shim to maintain backwards compatibility with old yap_ipython.config imports.
"""
# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import sys
from warnings import warn

from yap_ipython.utils.shimmodule import ShimModule, ShimWarning

warn("The `yap_ipython.config` package has been deprecated since yap_ipython 4.0. "
     "You should import from traitlets.config instead.", ShimWarning)


# Unconditionally insert the shim into sys.modules so that further import calls
# trigger the custom attribute access above

sys.modules['yap_ipython.config'] = ShimModule(src='yap_ipython.config', mirror='traitlets.config')
