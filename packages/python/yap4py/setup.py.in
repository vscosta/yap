#!/usr/bin/env python
# coding: utf-8

# Copyright (c) IPython Development Team.
# Distributed under the terms of the Modified BSD License.
from __future__ import print_function

from setuptools import setup
from  setuptools.extension import  Extension
from codecs import open
from os import path, makedirs, walk
from shutil import copytree, rmtree, copy2, move
from glob import glob
from pathlib import Path
import platform
import os.path

# the name of the package
name = 'YAP4PY'

#-----------------------------------------------------------------------------
# Minimal Python version sanity check
#-----------------------------------------------------------------------------

import sys

v = sys.version_info
if v[:2] < (2,7) or (v[0] >= 3 and v[:2] < (3,3)):
    error = "ERROR: %s requires Python version 2.7 or 3.3 or above." % name
    print(error, file=sys.stderr)
    sys.exit(1)

PY3 = (sys.version_info[0] >= 3)

#-----------------------------------------------------------------------------
# get on with it
#-----------------------------------------------------------------------------
from codecs import open
from os import path, makedirs, walk
from shutil import copytree, rmtree, copy2, move
from glob import glob
from pathlib import Path
import platform
import os.path
import os
import shutil

from distutils.core import setup

pjoin = os.path.join
here = os.path.abspath(os.path.dirname(__file__))
pkg_root = pjoin(here, name)

my_extra_link_args = []
if platform.system() == 'Darwin':
    my_extra_link_args = ['-Wl,-rpath','-Wl,${_ABS_PYTHON_MODULE_PATH}']
    so = 'dylib'
#or dll in glob('yap/dlls/*'):
#    move(  dll ,'lib' )
libs = glob('../../../*.{dylib;dll;so}'+'../../../*/*.{dylib;dll;so}'+'../../../*/*/*.{dylib;dll;so}')
for l in libs:
    copy2(l, 'yap4py', follow_symlinks=False)

cplus=['${RELATIVE_SOURCE}CXX/yapi.cpp']

py2yap=['${RELATIVE_SOURCE}packages/python/python.c',
        '${RELATIVE_SOURCE}packages/python/pl2py.c',
        '${RELATIVE_SOURCE}packages/python/pybips.c',
        '${RELATIVE_SOURCE}packages/python/py2pl.c',
        '${RELATIVE_SOURCE}packages/python/pl2pl.c',
        '${RELATIVE_SOURCE}packages/python/pypreds.c'
]

native_sources = ['yapPYTHON_wrap.cxx']+py2yap+cplus
here = path.abspath(path.dirname(__file__))

# Get the long description from the README file

extensions=[Extension('_yap', native_sources,
                      define_macros = [('MAJOR_VERSION', '1'),
                                       ('MINOR_VERSION', '0'),
                                       ('_YAP_NOT_INSTALLED_', '1'),
                                       ('YAP_PYTHON', '1')],
                      runtime_library_dirs=['yap4py','${CMAKE_INSTALL_FULL_LIBDIR}','${CMAKE_INSTALL_FULL_BINDIR}'],
    		      swig_opts=['-modern', '-c++', '-py3','-I${RELATIVE_SOURCE}/CXX'],
                      library_dirs=['../../..','../../../CXX','../../packages/python',"${YAP_INSTALL_FULL_DLLDIR}","${CMAKE_INSTALL_FULL_BINDIR}", '.'],
    		      extra_link_args=my_extra_link_args,
                      extra_compile_args=['-g3','-O0'],
    		      libraries=['Yap','${GMP_LIBRARIES}'],
                      include_dirs=['../../..',
                                    '${GMP_INCLUDE_DIRS}',
                                    '${RELATIVE_SOURCE}H',
                                    '${RELATIVE_SOURCE}H/generated',
                                    '${RELATIVE_SOURCE}OPTYap',
                                    '${RELATIVE_SOURCE}os',
                                    '${RELATIVE_SOURCE}include',
				    '${RELATIVE_SOURCE}CXX', '.']
)]

packages = ['yap4py']

def visit(d0, pls):
    for (r,ds,fs) in walk('.'):
        for f in fs:
            f0,ext = os.path.splitext(f)
            if (ext == 'yap' or ext == 'pl' or ext == 'so' or ext == 'dll' or ext == 'yss'):
                pls += [os.path.join(r, f)]
    for i in ds:
        pls = visit(os.path.join(d0, i), pls)
    return pls

package_data = {
    '': visit('.',[])
}

version_ns = {'__version__':'6.3.5','minor-version':'6','minor-version':'3','patch':'5'}


setup_args = dict(
    name            = name,
    version         = version_ns['__version__'],
    scripts         = glob(pjoin('scripts', '*')),
    packages        = packages,
    py_modules      = ['yap'],
    package_data    = package_data,
    include_package_data    = True,
    description     = "YAP in Python",
    author          = 'YAP Development Team',
    author_email    = 'ipython-dev@scipy.org',
    url             = 'http://ipython.org',
    license         = 'BSD',
    ext_modules     = extensions,
    platforms       = "Linux, Mac OS X, Windows",
    keywords        = ['Interactive', 'Interpreter', 'Shell', 'Web'],
    classifiers     = [
        'Intended Audience :: Developers',
        'Intended Audience :: System Administrators',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: BSD License',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
    ],
)

if 'develop' in sys.argv or any(a.startswith('bdist') for a in sys.argv):
    import setuptools

setuptools_args = {}
install_requires = setuptools_args['install_requires'] = [
]

extras_require = setuptools_args['extras_require'] = {
    'test:python_version=="2.7"': ['mock'],
    'test': ['nose_warnings_filters', 'nose-timer'],
}

if 'setuptools' in sys.modules:
    setup_args.update(setuptools_args)

if __name__ == '__main__':
    setup(**setup_args)
