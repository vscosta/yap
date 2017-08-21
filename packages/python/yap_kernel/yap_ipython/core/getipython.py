#-----------------------------------------------------------------------------
#  Copyright (C) 2013  The IPython Development Team
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Classes and functions
#-----------------------------------------------------------------------------


def get_ipython():
    """Get the global InteractiveShell instance.

    Returns None if no InteractiveShell instance is registered.
    """
    from yap_ipython.core.interactiveshell import YAPInteractive
    if YAPInteractive.initialized():
        return YAPInteractive.instance()
