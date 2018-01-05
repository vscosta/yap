# Copyright (c) yap_ipython Development Team.
# Distributed under the terms of the Modified BSD License.

import os
import shutil
import sys
import tempfile

try:
    from unittest.mock import patch
except ImportError:
    from mock import patch

from jupyter_core import paths as jpaths
from yap_ipython import paths as ipaths
from yap_kernel.kernelspec import install

pjoin = os.path.join

tmp = None
patchers = []

def setup():
    """setup temporary env for tests"""
    global tmp
    tmp = tempfile.mkdtemp()
    patchers[:] = [
        patch.dict(os.environ, {
            'HOME': tmp,
            # Let tests work with --user install when HOME is changed:
            'PYTHONPATH': os.pathsep.join(sys.path),
        }),
    ]
    for p in patchers:
        p.start()
    
    # install yap_ipython in the temp home:
    install(user=True)


def teardown():
    for p in patchers:
        p.stop()

    try:
        shutil.rmtree(tmp)
    except (OSError, IOError):
        # no such file
        pass
