#!/usr/bin/env python
# coding: utf-8

# Copyright (c) IPython Development Team.
# Distributed under the terms of the Modified BSD License.
from __future__ import print_function

import setuptools
from setuptools import setup
from setuptools.extension import Extension
from codecs import open
from os import path, makedirs, walk
from shutil import copytree, rmtree, copy2, move
from glob import glob
from pathlib import Path
import platform
import os.path

# the name of the package
name = 'YAP4PY'

# -----------------------------------------------------------------------------
# Minimal Python version sanity check
# -----------------------------------------------------------------------------

import sys

v = sys.version_info
if v[:2] < (2, 7) or (v[0] >= 3 and v[:2] < (3, 3)):
    error = "ERROR: %s requires Python version 2.7 or 3.3 or above." % name
    print(error, file=sys.stderr)
    sys.exit(1)

PY3 = (sys.version_info[0] >= 3)

# -----------------------------------------------------------------------------
# get on with it
# -----------------------------------------------------------------------------
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
#pkg_root = pjoin(here, name)

sys.path.insert(0, "/home/vsc/github/yap-6.3/packages/python/swig")

my_extra_link_args = []
if platform.system() == 'Windows':
    local_libs = []
    win_libs = ['wsock32','ws2_32']
    my_extra_link_args = ['-Wl,-export-all-symbols']
else:
    # if platform.system() == 'Darwin':
    my_extra_link_args = ['-Wl,-rpath', '-Wl,']
    win_libs = []
    local_libs = ['YAP++','Py4YAP']
# or dll in glob('yap/dlls/*'):
#    move(  dll ,'lib' )

native_sources = ["yap.i"]

for i in '/home/vsc/github/yap-6.3/packages/python/swig/yap4py/yapi.py;/home/vsc/github/yap-6.3/packages/python/swig/yap4py/__init__.py;/home/vsc/github/yap-6.3/packages/python/swig/yap4py/__main__.py'.split(";"): 
    copy2(i, 'yap4py')
for i in '/home/vsc/github/yap-6.3/packages/python/python.pl;/home/vsc/github/yap-6.3/library/INDEX.pl;/home/vsc/github/yap-6.3/library/apply.yap;/home/vsc/github/yap-6.3/library/apply_macros.yap;/home/vsc/github/yap-6.3/library/arg.yap;/home/vsc/github/yap-6.3/library/assoc.yap;/home/vsc/github/yap-6.3/library/atts.yap;/home/vsc/github/yap-6.3/library/autoloader.yap;/home/vsc/github/yap-6.3/library/avl.yap;/home/vsc/github/yap-6.3/library/bhash.yap;/home/vsc/github/yap-6.3/library/charsio.yap;/home/vsc/github/yap-6.3/library/clauses.yap;/home/vsc/github/yap-6.3/library/coinduction.yap;/home/vsc/github/yap-6.3/library/dbqueues.yap;/home/vsc/github/yap-6.3/library/dbusage.yap;/home/vsc/github/yap-6.3/library/dgraphs.yap;/home/vsc/github/yap-6.3/library/exo_interval.yap;/home/vsc/github/yap-6.3/library/expand_macros.yap;/home/vsc/github/yap-6.3/library/gensym.yap;/home/vsc/github/yap-6.3/library/hacks.yap;/home/vsc/github/yap-6.3/library/heaps.yap;/home/vsc/github/yap-6.3/library/lambda.pl;/home/vsc/github/yap-6.3/library/lineutils.yap;/home/vsc/github/yap-6.3/library/listing.yap;/home/vsc/github/yap-6.3/library/lists.yap;/home/vsc/github/yap-6.3/library/log2md.yap;/home/vsc/github/yap-6.3/library/nb.yap;/home/vsc/github/yap-6.3/library/ordsets.yap;/home/vsc/github/yap-6.3/library/mapargs.yap;/home/vsc/github/yap-6.3/library/maplist.yap;/home/vsc/github/yap-6.3/library/maputils.yap;/home/vsc/github/yap-6.3/library/matlab.yap;/home/vsc/github/yap-6.3/library/matrix.yap;/home/vsc/github/yap-6.3/library/prandom.yap;/home/vsc/github/yap-6.3/library/queues.yap;/home/vsc/github/yap-6.3/library/random.yap;/home/vsc/github/yap-6.3/library/range.yap;/home/vsc/github/yap-6.3/library/rbtrees.yap;/home/vsc/github/yap-6.3/library/regexp.yap;/home/vsc/github/yap-6.3/library/readutil.yap;/home/vsc/github/yap-6.3/library/rltree.yap;/home/vsc/github/yap-6.3/library/sockets.yap;/home/vsc/github/yap-6.3/library/splay.yap;/home/vsc/github/yap-6.3/library/stringutils.yap;/home/vsc/github/yap-6.3/library/system.yap;/home/vsc/github/yap-6.3/library/terms.yap;/home/vsc/github/yap-6.3/library/tries.yap;/home/vsc/github/yap-6.3/library/itries.yap;/home/vsc/github/yap-6.3/library/timeout.yap;/home/vsc/github/yap-6.3/library/trees.yap;/home/vsc/github/yap-6.3/library/ugraphs.yap;/home/vsc/github/yap-6.3/library/undgraphs.yap;/home/vsc/github/yap-6.3/library/varnumbers.yap;/home/vsc/github/yap-6.3/library/wdgraphs.yap;/home/vsc/github/yap-6.3/library/wgraphs.yap;/home/vsc/github/yap-6.3/library/wundgraphs.yap;/home/vsc/github/yap-6.3/library/lam_mpi.yap;/home/vsc/github/yap-6.3/library/ypp.yap;/home/vsc/github/yap-6.3/library/ytest.yap;/home/vsc/github/yap-6.3/library/c_alarms.yap;/home/vsc/github/yap-6.3/library/flags.yap;/home/vsc/github/yap-6.3/library/block_diagram.yap;/home/vsc/github/yap-6.3/packages/real/real.pl;/home/vsc/github/yap-6.3/packages/python/swig/prolog/jupyter.yap;/home/vsc/github/yap-6.3/packages/python/swig/prolog/yapi.yap'.split(";") + '/home/vsc/github/yap-6.3/swi/library/aggregate.pl;/home/vsc/github/yap-6.3/swi/library/base64.pl;/home/vsc/github/yap-6.3/swi/library/broadcast.pl;/home/vsc/github/yap-6.3/swi/library/ctypes.pl;/home/vsc/github/yap-6.3/swi/library/date.pl;/home/vsc/github/yap-6.3/swi/library/debug.pl;/home/vsc/github/yap-6.3/swi/library/edit.pl;/home/vsc/github/yap-6.3/swi/library/error.pl;/home/vsc/github/yap-6.3/swi/library/main.pl;/home/vsc/github/yap-6.3/swi/library/menu.pl;/home/vsc/github/yap-6.3/swi/library/nb_set.pl;/home/vsc/github/yap-6.3/swi/library/occurs.yap;/home/vsc/github/yap-6.3/swi/library/operators.pl;/home/vsc/github/yap-6.3/swi/library/option.pl;/home/vsc/github/yap-6.3/swi/library/pairs.pl;/home/vsc/github/yap-6.3/swi/library/plunit.pl;/home/vsc/github/yap-6.3/swi/library/predicate_options.pl;/home/vsc/github/yap-6.3/swi/library/predopts.pl;/home/vsc/github/yap-6.3/swi/library/prolog_clause.pl;/home/vsc/github/yap-6.3/swi/library/prolog_colour.pl;/home/vsc/github/yap-6.3/swi/library/prolog_source.pl;/home/vsc/github/yap-6.3/swi/library/prolog_xref.pl;/home/vsc/github/yap-6.3/swi/library/pure_input.pl;/home/vsc/github/yap-6.3/swi/library/quasi_quotations.pl;/home/vsc/github/yap-6.3/swi/library/quintus.pl;/home/vsc/github/yap-6.3/swi/library/record.pl;/home/vsc/github/yap-6.3/swi/library/settings.pl;/home/vsc/github/yap-6.3/swi/library/shlib.pl;/home/vsc/github/yap-6.3/swi/library/thread_pool.pl;/home/vsc/github/yap-6.3/swi/library/unix.pl;/home/vsc/github/yap-6.3/swi/library/url.pl;/home/vsc/github/yap-6.3/swi/library/utf8.pl;/home/vsc/github/yap-6.3/swi/library/win_menu.pl;/home/vsc/github/yap-6.3/swi/library/www_browser.pl;/home/vsc/github/yap-6.3/swi/library/dcg/basics.pl'.split(";")  :
    copy2(i, 'yap4py/prolog')
for i in '/home/vsc/github/yap-6.3/os/edio.yap;/home/vsc/github/yap-6.3/os/chartypes.yap'.split(";") :
    copy2(i, 'yap4py/prolog/os')
for i in  '/home/vsc/github/yap-6.3/pl/absf.yap;/home/vsc/github/yap-6.3/pl/arith.yap;/home/vsc/github/yap-6.3/pl/arithpreds.yap;/home/vsc/github/yap-6.3/pl/arrays.yap;/home/vsc/github/yap-6.3/pl/atoms.yap;/home/vsc/github/yap-6.3/pl/attributes.yap;/home/vsc/github/yap-6.3/pl/boot.yap;/home/vsc/github/yap-6.3/pl/bootlists.yap;/home/vsc/github/yap-6.3/pl/callcount.yap;/home/vsc/github/yap-6.3/pl/checker.yap;/home/vsc/github/yap-6.3/pl/consult.yap;/home/vsc/github/yap-6.3/pl/control.yap;/home/vsc/github/yap-6.3/pl/corout.yap;/home/vsc/github/yap-6.3/pl/dbload.yap;/home/vsc/github/yap-6.3/pl/debug.yap;/home/vsc/github/yap-6.3/pl/depth_bound.yap;/home/vsc/github/yap-6.3/pl/dialect.yap;/home/vsc/github/yap-6.3/pl/directives.yap;/home/vsc/github/yap-6.3/pl/eam.yap;/home/vsc/github/yap-6.3/pl/error.yap;/home/vsc/github/yap-6.3/pl/errors.yap;/home/vsc/github/yap-6.3/pl/eval.yap;/home/vsc/github/yap-6.3/pl/flags.yap;/home/vsc/github/yap-6.3/pl/grammar.yap;/home/vsc/github/yap-6.3/pl/ground.yap;/home/vsc/github/yap-6.3/pl/hacks.yap;/home/vsc/github/yap-6.3/pl/init.yap;/home/vsc/github/yap-6.3/pl/listing.yap;/home/vsc/github/yap-6.3/pl/load_foreign.yap;/home/vsc/github/yap-6.3/pl/messages.yap;/home/vsc/github/yap-6.3/pl/meta.yap;/home/vsc/github/yap-6.3/pl/modules.yap;/home/vsc/github/yap-6.3/pl/newmod.yap;/home/vsc/github/yap-6.3/pl/os.yap;/home/vsc/github/yap-6.3/pl/pathconf.yap;/home/vsc/github/yap-6.3/pl/preddecls.yap;/home/vsc/github/yap-6.3/pl/preddyns.yap;/home/vsc/github/yap-6.3/pl/preds.yap;/home/vsc/github/yap-6.3/pl/profile.yap;/home/vsc/github/yap-6.3/pl/protect.yap;/home/vsc/github/yap-6.3/pl/qly.yap;/home/vsc/github/yap-6.3/pl/save.yap;/home/vsc/github/yap-6.3/pl/setof.yap;/home/vsc/github/yap-6.3/pl/signals.yap;/home/vsc/github/yap-6.3/pl/sort.yap;/home/vsc/github/yap-6.3/pl/spy.yap;/home/vsc/github/yap-6.3/pl/statistics.yap;/home/vsc/github/yap-6.3/pl/strict_iso.yap;/home/vsc/github/yap-6.3/pl/swi.yap;/home/vsc/github/yap-6.3/pl/tabling.yap;/home/vsc/github/yap-6.3/pl/threads.yap;/home/vsc/github/yap-6.3/pl/udi.yap;/home/vsc/github/yap-6.3/pl/undefined.yap;/home/vsc/github/yap-6.3/pl/utils.yap;/home/vsc/github/yap-6.3/pl/yapor.yap;/home/vsc/github/yap-6.3/pl/yio.yap'.split(";") :
    copy2(i, 'yap4py/prolog/pl')
my_libs0 = '/home/vsc/github/yap-6.3/packages/python/*YAPPython*.so;/home/vsc/github/yap-6.3/packages/python/*Py4YAP*.so;/home/vsc/github/yap-6.3/*libYap*.so;/home/vsc/github/yap-6.3/packages/myddas/sqlite3/*Yapsqlite3*.so;/home/vsc/github/yap-6.3/packages/myddas/mysql/*Yapmysql*.so;/home/vsc/github/yap-6.3/packages/myddas/odbc/*Yapodbc*.so;/home/vsc/github/yap-6.3/CXX/*YAP++*.so;/home/vsc/github/yap-6.3/library/matrix/*matrix*.so;/home/vsc/github/yap-6.3/library/random/*yap_random*.so;/home/vsc/github/yap-6.3/library/regex/*regexp*.so;/home/vsc/github/yap-6.3/library/rltree/*yap_rl*.so;/home/vsc/github/yap-6.3/library/system/*sys*.so;/home/vsc/github/yap-6.3/library/tries/*tries*.so;/home/vsc/github/yap-6.3/library/tries/*itries*.so;/home/vsc/github/yap-6.3/packages/raptor/*libxml2*.so;/home/vsc/github/yap-6.3/packages/raptor/*raptor*.so;/home/vsc/github/yap-6.3/packages/CLPBN/horus/*horus*.so;/home/vsc/github/yap-6.3/packages/gecode/*gecode_yap*.so;/home/vsc/github/yap-6.3/packages/real/*real*.so;/home/vsc/github/yap-6.3/packages/jpl/src/c/*jplYap*.so;/home/vsc/github/yap-6.3/packages/swi-minisat2/C/*minisat2*.so;/home/vsc/github/yap-6.3/library/lammpi/*yap_mpi*.so'.split(";")+['/home/vsc/github/yap-6.3/startup.yss']
my_libs = []
for i in my_libs0:
    my_libs = glob(i) + my_libs
for i in my_libs:
    copy2(i, 'yap4py')
bpy2yap = []
here = path.abspath(path.dirname(__file__))
#gmp_dir = path.abspath(path.dirname("/usr/lib/x86_64-linux-gnu/libgmp.so"))
#python_libdir = path.abspath(path.dirname("/usr/lib/x86_64-linux-gnu/libpython2.7.so")
# Get the long description from the README file



extensions = [Extension('_yap', native_sources,
                        define_macros=[('MAJOR_VERSION', '1'),
                                       ('MINOR_VERSION', '0'),
                                       ('_YAP_NOT_INSTALLED_', '1'),
                                       ('YAP_PYTHON', '1'),
                                       ('_GNU_SOURCE', '1')],
                        runtime_library_dirs=['yap4py', '/usr/local/lib', '/usr/local/bin', '', ''],
                        swig_opts=['-modern', '-c++', '-py3',
                                   '-DX_API', '-I/home/vsc/github/yap-6.3/CXX', '-I/home/vsc/github/yap-6.3/include',
                                   '-I/home/vsc/github/yap-6.3/H', '-I/home/vsc/github/yap-6.3/H/generated',
                                   '-I/home/vsc/github/yap-6.3/os', '-I/home/vsc/github/yap-6.3/OPTYap',
                                   '-I/home/vsc/github/yap-6.3/packages/python'],
                        library_dirs=['../../..', '../../../CXX', '..', "/usr/local/bin"],
                        extra_link_args=my_extra_link_args,
                        libraries=['Yap','gmp']+win_libs+local_libs,
                        include_dirs=['../../..',
                                      '/usr/include/x86_64-linux-gnu',
                                      '/home/vsc/github/yap-6.3/H',
                                      '/home/vsc/github/yap-6.3/H/generated',
                                      '/home/vsc/github/yap-6.3/OPTYap',
                                      '/home/vsc/github/yap-6.3/os',
                                      '/home/vsc/github/yap-6.3/packages/python',
                                      '/home/vsc/github/yap-6.3/include',
                                      '/home/vsc/github/yap-6.3/CXX', '.']
                        )]#

packages = packages = setuptools.find_packages('/home/vsc/github/yap-6.3/packages/python/swig')



package_data = {
    'yap4py/prolog': 'prolog/*.*',
    '': '*.*'
}

version_ns = {'__version__': '6.3.5', 'minor-version': '6', 'minor-version': '3', 'patch': '5'}

setup_args = dict(
    name=name,
    version=version_ns['__version__'],
    scripts=glob(pjoin('scripts', '*')),
    packages=packages,
    py_modules=['yap','yap4py'],
    package_data=package_data,
    include_package_data=True,
    requirements=[
    'm2w64-gmp',
    'm2-msys2-keyring',
    'm2-msys2-launcher-git',
    'm2-msys2-runtime',
    ],
    description="YAP in Python",
    author='YAP Development Team',
    author_email='ipython-dev@scipy.org',
    url='http://ipython.org',
    license='BSD',
    ext_modules=extensions,
    platforms="Linux, Mac OS X, Windows",
    keywords=['Interactive', 'Interpreter', 'Shell', 'Web'],
    classifiers=[
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
