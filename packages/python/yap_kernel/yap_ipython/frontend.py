"""
Shim to maintain backwards compatibility with old frontend imports.

We have moved all contents of the old `frontend` subpackage into top-level
subpackages (`html`, `qt` and `terminal`), and flattened the notebook into
just `yap_ipython.html`, formerly `yap_ipython.frontend.html.notebook`.

This will let code that was making `from yap_ipython.frontend...` calls continue
working, though a warning will be printed.
"""

# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import sys
from warnings import warn

from yap_ipython.utils.shimmodule import ShimModule, ShimWarning

warn("The top-level `frontend` package has been deprecated since yap_ipython 1.0. "
     "All its subpackages have been moved to the top `yap_ipython` level.", ShimWarning)

# Unconditionally insert the shim into sys.modules so that further import calls
# trigger the custom attribute access above

sys.modules['yap_ipython.frontend.html.notebook'] = ShimModule(
    src='yap_ipython.frontend.html.notebook', mirror='yap_ipython.html')
sys.modules['yap_ipython.frontend'] = ShimModule(
    src='yap_ipython.frontend', mirror='yap_ipython')
