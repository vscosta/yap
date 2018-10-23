# coding: utf-8
"""Test suite for our sysinfo utilities."""

# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import json
import nose.tools as nt

from yap_ipython.utils import sysinfo


def test_json_getsysinfo():
    """
    test that it is easily jsonable and don't return bytes somewhere. 
    """
    json.dumps(sysinfo.get_sys_info())
