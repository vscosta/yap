# coding: utf-8
"""Tests for yap_ipython.core.application"""

import os
import tempfile

import nose.tools as nt

from traitlets import Unicode

from yap_ipython.core.application import BaseYAPApplication
from yap_ipython.testing import decorators as dec
from yap_ipython.utils.tempdir import TemporaryDirectory


@dec.onlyif_unicode_paths
def test_unicode_cwd():
    """Check that yap_ipython starts with non-ascii characters in the path."""
    wd = tempfile.mkdtemp(suffix=u"€")
    
    old_wd = os.getcwd()
    os.chdir(wd)
    #raise Exception(repr(os.getcwd()))
    try:
        app = BaseYAPApplication()
        # The lines below are copied from Application.initialize()
        app.init_profile_dir()
        app.init_config_files()
        app.load_config_file(suppress_errors=False)
    finally:
        os.chdir(old_wd)

@dec.onlyif_unicode_paths
def test_unicode_ipdir():
    """Check that yap_ipython starts with non-ascii characters in the IP dir."""
    ipdir = tempfile.mkdtemp(suffix=u"€")
    
    # Create the config file, so it tries to load it.
    with open(os.path.join(ipdir, 'ipython_config.py'), "w") as f:
        pass
    
    old_ipdir1 = os.environ.pop("IPYTHONDIR", None)
    old_ipdir2 = os.environ.pop("IPYTHON_DIR", None)
    os.environ["IPYTHONDIR"] = ipdir
    try:
        app = BaseYAPApplication()
        # The lines below are copied from Application.initialize()
        app.init_profile_dir()
        app.init_config_files()
        app.load_config_file(suppress_errors=False)
    finally:
        if old_ipdir1:
            os.environ["IPYTHONDIR"] = old_ipdir1
        if old_ipdir2:
            os.environ["IPYTHONDIR"] = old_ipdir2

def test_cli_priority():
    with TemporaryDirectory() as td:

        class TestApp(BaseYAPApplication):
            test = Unicode().tag(config=True)

        # Create the config file, so it tries to load it.
        with open(os.path.join(td, 'ipython_config.py'), "w") as f:
            f.write("c.TestApp.test = 'config file'")

        app = TestApp()
        app.initialize(['--profile-dir', td])
        nt.assert_equal(app.test, 'config file')
        app = TestApp()
        app.initialize(['--profile-dir', td, '--TestApp.test=cli'])
        nt.assert_equal(app.test, 'cli')

