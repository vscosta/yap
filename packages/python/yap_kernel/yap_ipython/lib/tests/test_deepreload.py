# -*- coding: utf-8 -*-
"""Test suite for the deepreload module."""

# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import os

import nose.tools as nt

from yap_ipython.utils.syspathcontext import prepended_to_syspath
from yap_ipython.utils.tempdir import TemporaryDirectory
from yap_ipython.lib.deepreload import reload as dreload

def test_deepreload():
    "Test that dreload does deep reloads and skips excluded modules."
    with TemporaryDirectory() as tmpdir:
        with prepended_to_syspath(tmpdir):
            with open(os.path.join(tmpdir, 'A.py'), 'w') as f:
                f.write("class Object(object):\n    pass\n")
            with open(os.path.join(tmpdir, 'B.py'), 'w') as f:
                f.write("import A\n")
            import A
            import B

            # Test that A is not reloaded.
            obj = A.Object()
            dreload(B, exclude=['A'])
            nt.assert_true(isinstance(obj, A.Object))

            # Test that A is reloaded.
            obj = A.Object()
            dreload(B)
            nt.assert_false(isinstance(obj, A.Object))
