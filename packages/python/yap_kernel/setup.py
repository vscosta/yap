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
import sysconfig
import setuptools

v = sys.version_info
if v[:2] < (2,7) or (v[0] >= 3 and v[:2] < (3,3)):
    error = "ERROR: %s requires Python version 3.3 or above." % name
    print(error, file=sys.stderr)
    sys.exit(1)

PY3 = (sys.version_info[0] >= 3)

#-----------------------------------------------------------------------------
# get on with it
#-----------------------------------------------------------------------------

from glob import glob
import os
import shutil

from distutils.core import setup
from glob import glob
from shutil import copy

pjoin = os.path.join
here = os.path.abspath(os.path.dirname(__file__))
packages = ['yap_kernel','yap_ipython']
# pkg_root = pjoin(here, name)

try:
    copy(glob(pjoin(here,"../swig/build/lib*/_yap*"))[0],here)
    copy(glob(pjoin(here,"../../../libYap*"))[-1],here)
except:
    pass

for d, _, _ in os.walk(pjoin(here, 'yap_kernel')):
    if os.path.exists(pjoin(d, '__init__.py')):
        packages.append(d[len(here)+1:].replace(os.path.sep, '.'))
for d, _, _ in os.walk(pjoin(here, 'yap_ipython')):
    if os.path.exists(pjoin(d, '__init__.py')):
        packages.append(d[len(here)+1:].replace(os.path.sep, '.'))

sys.path.insert(0, here)
sys.path.insert(0, pjoin(here,'..','swig'))
package_data = {
'yap_ipython': ['prolog/*.*'],
'yap_kernel': ['resources/*.*']
}

version_ns = {}
with open(pjoin(here, name, '_version.py')) as f:
    exec(f.read(), {}, version_ns)





setup_args = dict(
    name            = name,
    version         = version_ns['__version__'],
    scripts         = glob(pjoin('scripts', '*')),
    packages        = packages,
    py_modules      = ['yap_kernel_launcher'],
    package_data    = package_data,
    #package_dir = {'':here},
    description     = "YAP Kernel for Jupyter",
    author          = 'YAP Development Team',
    author_email    = 'yap-dev@scipy.org',
    url             = 'http://ipython.org',
    license         = 'BSD',
    platforms       = "Linux, Mac OS X, Windows",
    keywords        = ['Interactive', 'Interpreter', 'Shell', 'Web'],
    classifiers     = [
        'Intended Audience :: Developers',
        'Intended Audience :: System Administrators',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: BSD License',
        'Programming Language :: Prolog',
        'Programming Language :: Python',
        'Programming Language :: Python :: 3',
    ],
)

if 'develop' in sys.argv or any(a.startswith('bdist') for a in sys.argv):
    import setuptools

setuptools_args = {}
install_requires = setuptools_args['install_requires'] = [
]

if any(a.startswith(('bdist', 'build', 'install')) for a in sys.argv):
    from yap_kernel.kernelspec import write_kernel_spec, make_yap_kernel_cmd, KERNEL_NAME


    argv = make_yap_kernel_cmd(executable=sys.executable)
    dest = os.path.join(here, 'yap_kernel', 'resources')
    try:
        write_kernel_spec(dest, overrides={'argv': argv})
    except:
        pass
    # shutil.copy('${CMAKE_SOURCE_DIR}/misc/editors/prolog.js',dest)
    #setup_args['data_files'] = [(pjoin('share', 'jupyter', 'kernels', KERNEL_NAME), glob(pjoin(dest, '*')))]
    mode_loc = pjoin( sysconfig.get_path('platlib'), 'notebook', 'static', 'components', 'codemirror', 'mode', 'prolog')
    custom_loc = pjoin( sysconfig.get_path('platlib'), 'notebook', 'static', 'custom')
#    try:
#        shutil.copy( pjoin( custom_loc, "custom.js") , pjoin( custom_loc, "custom.js.orig"))
#        shutil.copy( pjoin( "resources", "custom.js") , pjoin( custom_loc, "custom.js"))
#        if not os.path.exists(mode_loc):
#            os.makedirs(mode_loc)
#        shutil.copy( pjoin( "resources","prolog.js") , mode_loc)
#    except:
#        pass

extras_require = setuptools_args['extras_require'] = {
    'test:python_version=="2.7"': ['mock'],
    'test': ['nose_warnings_filters', 'nose-timer'],
}

if 'setuptools' in sys.modules:
    setup_args.update(setuptools_args)

if __name__ == '__main__':
    setup(**setup_args)
