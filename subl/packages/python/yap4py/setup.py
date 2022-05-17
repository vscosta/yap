#!/usr/bin/env python
# coding: utf-8

# Copyright (c) IPython Development Team.
# Distributed under the terms of the Modified BSD License.
from __future__ import print_function

from setuptools import setup, find_packages
from  setuptools.extension import  Extension
from codecs import open
from os import path, makedirs, walk
from shutil import copytree, rmtree, copy2, move
from glob import glob
from pathlib import Path
import platform
import os.path

# the name of the package
name = 'yap4py'

#-----------------------------------------------------------------------------
# Minimal Python version sanity check
#-----------------------------------------------------------------------------

import sys

v = sys.version_info
if v[:2] < (3,3):
    error = "ERROR: %s requires Python version  3.3 or above." % name
    print(error, file=sys.stderr)
    sys.exit(1)


#-----------------------------------------------------------------------------
# get on with it
#-----------------------------------------------------------------------------

pjoin = os.path.join

my_extra_link_args = []
if platform.system() == 'Darwin':
    my_extra_link_args = ['-Wl,-rpath','-Wl,/opt/homebrew/opt/python@3.9/Frameworks/Python.framework/Versions/3.9/lib/python3.9/site-packages/yap4py']
    so = 'dylib'
#or dll in glob('yap/dlls/*'):
#    move(  dll ,'lib' )

native_sources = ['_yap.cpp']

includes = ['-I/Users/vsc/github/yap/subl',
            '-I/Users/vsc/github/yap/H',
            '-I/opt/homebrew/opt/gmp/include',
            '-I/Users/vsc/github/yap/H/generated',
            '-I/Users/vsc/github/yap/utf8proc',
            '-I/Users/vsc/github/yap/OPTYap',
            '-I/Users/vsc/github/yap/os',
            '-I/Users/vsc/github/yap/include',
            '-I/Users/vsc/github/yap/packages/python',
            '-I'+os.path.dirname('/opt/homebrew/bin/swig'),
	    '-I/Users/vsc/github/yap/CXX',
            '-I.']


include_dirs = ['/Users/vsc/github/yap/subl',
            '/Users/vsc/github/yap/H',
            '/opt/homebrew/opt/gmp/include',
            '/Users/vsc/github/yap/H/generated',
            '/Users/vsc/github/yap/utf8proc',
            '/Users/vsc/github/yap/OPTYap',
            '/Users/vsc/github/yap/os',
            '/Users/vsc/github/yap/include',
            '/Users/vsc/github/yap/packages/python',
	    '/Users/vsc/github/yap/CXX',
            '.']

# Get the long description from the README file
opts = [ '-c++', '-py3']
opts += includes
opts += [ "-addextern", "-O","-doxygen","-fastproxy"]

extensions=[Extension('yap4py._yap', native_sources,
                      define_macros = [('MAJOR_VERSION', '7'),
                                       ('MINOR_VERSION', '2'),
                                       ('PATCH', '1'),
                                       ('_YAP_NOT_INSTALLED_', '1'),
                                       ('YAP_PYTHON', '1')],
                      runtime_library_dirs=[
                          '/usr/local/lib',
                              '/usr/local/bin'],
                      swig_opts=opts,
                      library_dirs=[
                          '/Users/vsc/github/yap/subl',
                          '/Users/vsc/github/yap/subl/CXX',
                          '/Users/vsc/github/yap/subl/packages/python',
                          '/Users/vsc/github/yap/subl/packages/python/yap4py'],
                      include_dirs=include_dirs,
    		      libraries=['YAP++','Py4YAP'])]

packages = ['yap4py']

version_ns = {'__version__':'7.2.1',
              'major-version':'7',
              'minor-version':'2',
              'patch':'1'}


setup_args = dict(
    ext_modules     = extensions,
    name            = "yap4py",
    version         = version_ns['__version__'],
    scripts         = glob(pjoin('scripts', '*')),
    zip_safe        = False,
    packages        = packages,
    include_package_data    = True,
    description     = "YAP in Python",
    author          = 'YAP Development Team',
    author_email    = 'ipython-dev@scipy.org',
    url             = 'http://ipython.org',
    license         = 'BSD',
    install_requires = [],
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

if __name__ == '__main__':
    setup(**setup_args)

