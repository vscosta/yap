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
    my_extra_link_args = ['-Wl,-rpath','-Wl,']
    so = 'dylib'
#or dll in glob('yap/dlls/*'):
#    move(  dll ,'lib' )

copy2(os.path.relpath('/home/vsc/github/yap/packages/swig/yap.i'),'yap4py/yap.i')
native_sources = ['yap4py/yap.i']

includes = ['-I/home/vsc/github/yap',
            '-I/home/vsc/github/yap/H',
            '-I',
            '-I/home/vsc/github/yap/H/generated',
            '-I/home/vsc/github/yap/utf8proc',
            '-I/home/vsc/github/yap/OPTYap',
            '-I/home/vsc/github/yap/os',
            '-I/home/vsc/github/yap/include',
            '-I/home/vsc/github/yap/packages/python',
	    '-I/home/vsc/github/yap/CXX',
            '-I.']


include_dirs = ['/home/vsc/github/yap',
            '/home/vsc/github/yap/H',
            '',
            '/home/vsc/github/yap/H/generated',
            '/home/vsc/github/yap/utf8proc',
            '/home/vsc/github/yap/OPTYap',
            '/home/vsc/github/yap/os',
            '/home/vsc/github/yap/include',
            '/home/vsc/github/yap/packages/python',
	    '/home/vsc/github/yap/CXX',
            '.']

# Get the long description from the README file
opts = [ '-c++', '-py3']
opts += includes
opts += [ "-addextern", "-O","-doxygen","-fastproxy"]

extensions=[Extension('yap4py._yap', native_sources,
                      define_macros = [('MAJOR_VERSION', '7'),
                                       ('MINOR_VERSION', '0'),
                                       ('PATCH', '5'),
                                       ('_YAP_NOT_INSTALLED_', '1'),
                                       ('YAP_PYTHON', '1')],
                      runtime_library_dirs=[ '/usr/local/lib64', '/usr/local/lib64/Yap','/usr/local/bin'],
                      swig_opts=opts,
                      library_dirs=['/home/vsc/github/yap','/home/vsc/github/yap/CXX',
                      '/home/vsc/github/yap/packages/python'],
                      include_dirs=include_dirs,
    		      libraries=['Py4YAP','YAP++','Yap','gmp'])]

packages = ['yap4py']
copytree('/home/vsc/github/yap/packages/python/yap4py/prolog','yap4py/prolog', dirs_exist_ok=True)
package_data = {'yap4py':['prolog/*.yap']}

version_ns = {'__version__':'7.0.5',
              'major-version':'7',
              'minor-version':'0',
              'patch':'5'}


setup_args = dict(
    ext_modules     = extensions,
    name            = "yap4py",
    version         = version_ns['__version__'],
    scripts         = glob(pjoin('scripts', '*')),
    packages        = packages,
    package_data    = package_data,
    zip_safe        = False,
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

