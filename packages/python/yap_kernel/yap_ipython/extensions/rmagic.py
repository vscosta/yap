# -*- coding: utf-8 -*-

#-----------------------------------------------------------------------------
#  Copyright (C) 2012 The yap_ipython Development Team
#-----------------------------------------------------------------------------

import warnings

def load_ipython_extension(ip):
    """Load the extension in yap_ipython."""
    warnings.warn("The rmagic extension in yap_ipython has moved to "
            "`rpy2.ipython`, please see `rpy2` documentation.")
