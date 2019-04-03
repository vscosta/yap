#!/usr/bin/env python
# coding: utf-8

# Copyright (c) IPython Development Team.
# Distributed under the terms of the Modified BSD License.

from __future__ import print_function

# the name of the package
name = 'yap_kernel'

#-----------------------------------------------------------------------------
# Minimal Python version sanity check
#-----------------------------------------------------------------------------

import sys
import os

v = sys.version_info
if v[:2] < (2,7) or (v[0] >= 3 and v[:2] < (3,3)):
    error = "ERROR: %s requires Python version 2.7 or 3.3 or above." % name
    print(error, file=sys.stderr)
    sys.exit(1)

PY3 = (sys.version_info[0] >= 3)

#-----------------------------------------------------------------------------
# get on with it
#-----------------------------------------------------------------------------

import os
from glob import globx
from shutil import copy
from distutils.core import setup

copy(glob("../swig/build/lib/_yap*")[0],"../swig/yap4py")
copy(glob("../../libYap*")[-1],"../swig/yap4py")

packages = ["${CMAKE_CURRENT_SOURCE_DIR}"]

version_ns = {}
setup_args = dict(
    name            = 'yap_kernel',
    version         = '0.0.1',
    packages        = ["yap_kernel"],
    package_dir = {'': '${CMAKE_SOURCE_DIR}/packages/python'  },
    description     = "YAP Kernel for Jupyter",
    long_description="A simple YAP kernel for Jupyter/IPython",
    url="https://github.com/vscosta/yap-6.3",
    author='Vitor Santos Costa, based on the the IPython',
    author_email='vsc@dcc.fc.up.pt',
    license         = 'BSD',
    platforms       = "Linux, Mac OS X, Windows",
    keywords        = ['Interactive', 'Interpreter', 'Shell', 'Web'],
     data_files=[('share/Yap/js', ['${CMAKE_SOURCE_DIR}/misc/editors/prolog.js'])],
    classifiers     = [
        'Intended Audience :: Developers',
        'Intended Audience :: System Administrators',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: Perl License',
        'Programming Language :: Prolog',
        'Programming Language :: Python :: 3',
    ],
)

if 'develop' in sys.argv or any(a.startswith('bdist') for a in sys.argv):
    import setuptools

setuptools_args = {}
install_requires = setuptools_args['install_requires'] = [
    'ipython>=4.0.0',
    'traitlets>=4.1.0',
    'jupyter_client',
    'tornado>=4.0',
]

extras_require = setuptools_args['extras_require'] = {
    'test:python_version=="2.7"': ['mock', 'nose_warnings_filters'],
}

if 'setuptools' in sys.modules:
    setup_args.update(setuptools_args)

if __name__ == '__main__':
    sys.path += ['../swig']
    setup(**setup_args)
