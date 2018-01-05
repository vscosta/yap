# encoding: utf-8
"""A payload based version of page."""

# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import warnings
from yap_ipython.core.getipython import get_ipython


def page(strng, start=0, screen_lines=0, pager_cmd=None):
    """Print a string, piping through a pager.

    This version ignores the screen_lines and pager_cmd arguments and uses
    yap_ipython's payload system instead.

    Parameters
    ----------
    strng : str or mime-dict
      Text to page, or a mime-type keyed dict of already formatted data.

    start : int
      Starting line at which to place the display.
    """

    # Some routines may auto-compute start offsets incorrectly and pass a
    # negative value.  Offset to 0 for robustness.
    start = max(0, start)
    shell = get_ipython()
    
    if isinstance(strng, dict):
        data = strng
    else:
        data = {'text/plain' : strng}
    payload = dict(
        source='page',
        data=data,
        start=start,
        )
    shell.payload_manager.write_payload(payload)


def install_payload_page():
    """DEPRECATED, use show_in_pager hook
    
    Install this version of page as yap_ipython.core.page.page.
    """
    warnings.warn("""install_payload_page is deprecated.
    Use `ip.set_hook('show_in_pager, page.as_hook(payloadpage.page))`
    """)
    from yap_ipython.core import page as corepage
    corepage.page = page
