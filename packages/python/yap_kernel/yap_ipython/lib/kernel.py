"""[DEPRECATED] Utilities for connecting to kernels

Moved to yap_ipython.kernel.connect
"""

import warnings
warnings.warn("yap_ipython.lib.kernel moved to yap_ipython.kernel.connect in yap_ipython 1.0," 
        " and will be removed in yap_ipython 6.0.",
    DeprecationWarning
)

from yap_kernel.connect import *

