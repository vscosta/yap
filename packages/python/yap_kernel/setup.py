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

pjoin = os.path.join
here = os.path.abspath(os.path.dirname(__file__))
# pkg_root = pjoin(here, name)

packages = setuptools.find_packages('/home/vsc/github/yap-6.3/packages/python/yap_kernel')
# for d, _, _ in os.walk(pjoin(here, name)):
#     if os.path.exists(pjoin(d, '__init__.py')):
#         packages.append(d[len(here)+1:].replace(os.path.sep, '.'))

sys.path.insert(0, "/home/vsc/github/yap-6.3/packages/python/yap_kernel")
package_data = {
'yap_ipython': ['prolog/*.*'],
'yap_kernel': ['resources/*.*']
}


version_ns = {}
with open(pjoin('/home/vsc/github/yap-6.3/packages/python/yap_kernel', name, '_version.py')) as f:
    exec(f.read(), {}, version_ns)




setup_args = dict(
    name            = name,
    version         = version_ns['__version__'],
    scripts         = glob(pjoin('scripts', '*')),
    packages        = packages,
    py_modules      = ['yap_kernel_launcher'],
    package_data    = package_data,
    package_dir = {'':"/home/vsc/github/yap-6.3/packages/python/yap_kernel"},
    description     = "YAP Kernel for Jupyter",
    author          = 'YAP Development Team',
    author_email    = 'YAP-dev@scipy.org',
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


    argv = make_yap_kernel_cmd(executable='python')
    dest = os.path.join(here, 'resources')
    if not os.path.exists(dest):
      os.makedirs( dest )
    shutil.copy('/home/vsc/github/yap-6.3/docs/icons/yap_32x32x32.png',os.path.join(dest,'logo-32x32.png'))
    shutil.copy('/home/vsc/github/yap-6.3/docs/icons/yap_64x64x32.png',os.path.join(dest,'logo-64x64.png'))
    try:
        write_kernel_spec(dest, overrides={'argv': argv})
    except:
        none
    # shutil.copy('/home/vsc/github/yap-6.3/packages/python/yap_kernel/kernel.js',dest)
    # shutil.copy('/home/vsc/github/yap-6.3/misc/editors/prolog.js',dest)
        setup_args['data_files'] = [
          (pjoin('share', 'jupyter', 'kernels', KERNEL_NAME), glob(pjoin(dest, '*'))),
          ]
    mode_loc = pjoin( sysconfig.get_path('platlib'), 'notebook', 'static', 'components', 'codemirror', 'mode', 'prolog')
    custom_loc = pjoin( sysconfig.get_path('platlib'), 'notebook', 'static', 'custom')
    try:
        shutil.copy( pjoin( custom_loc, "custom.js") , pjoin( custom_loc, "custom.js.orig"))
        shutil.copy( "/home/vsc/github/yap-6.3/packages/python/yap_kernel/custom.js" , pjoin( custom_loc, "custom.js"))
        if not os.path.exists(mode_loc):
            os.makedirs(mode_loc)
        shutil.copy( "/home/vsc/github/yap-6.3/misc/editors/prolog.js" , mode_loc)
    except:
        pass

extras_require = setuptools_args['extras_require'] = {
    'test:python_version=="2.7"': ['mock'],
    'test': ['nose_warnings_filters', 'nose-timer'],
}

if 'setuptools' in sys.modules:
    setup_args.update(setuptools_args)

if __name__ == '__main__':
    setup(**setup_args)
