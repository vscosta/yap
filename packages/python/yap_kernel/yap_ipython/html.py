"""
Shim to maintain backwards compatibility with old yap_ipython.html imports.
"""
# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import sys
from warnings import warn

from yap_ipython.utils.shimmodule import ShimModule, ShimWarning

warn("The `yap_ipython.html` package has been deprecated since yap_ipython 4.0. "
     "You should import from `notebook` instead. "
     "`yap_ipython.html.widgets` has moved to `ipywidgets`.", ShimWarning)

_widgets = sys.modules['yap_ipython.html.widgets'] = ShimModule(
    src='yap_ipython.html.widgets', mirror='ipywidgets')

_html = ShimModule(
    src='yap_ipython.html', mirror='notebook')

# hook up widgets
_html.widgets = _widgets
sys.modules['yap_ipython.html'] = _html

if __name__ == '__main__':
    from notebook import notebookapp as app
    app.launch_new_instance()
