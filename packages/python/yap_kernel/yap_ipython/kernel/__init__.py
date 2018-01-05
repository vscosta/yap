"""
Shim to maintain backwards compatibility with old yap_ipython.kernel imports.
"""
# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import sys
from warnings import warn

from yap_ipython.utils.shimmodule import ShimModule, ShimWarning

warn("The `yap_ipython.kernel` package has been deprecated since yap_ipython 4.0."
     "You should import from yap_kernel or jupyter_client instead.", ShimWarning)


# zmq subdir is gone
sys.modules['yap_ipython.kernel.zmq.session'] = ShimModule(
    src='yap_ipython.kernel.zmq.session', mirror='jupyter_client.session')
sys.modules['yap_ipython.kernel.zmq'] = ShimModule(
    src='yap_ipython.kernel.zmq', mirror='yap_kernel')

for pkg in ('comm', 'inprocess'):
    src = 'yap_ipython.kernel.%s' % pkg
    sys.modules[src] = ShimModule(src=src, mirror='yap_kernel.%s' % pkg)

for pkg in ('ioloop', 'blocking'):
    src = 'yap_ipython.kernel.%s' % pkg
    sys.modules[src] = ShimModule(src=src, mirror='jupyter_client.%s' % pkg)

# required for `from yap_ipython.kernel import PKG`
from yap_kernel import comm, inprocess
from jupyter_client import ioloop, blocking
# public API
from yap_kernel.connect import *
from jupyter_client import *
