"""A setuptools based setup module.

See:
https://packaging.python.org/en/latest/distributing.html
https://github.com/pypa/sampleproject


"""

# Always prefer setuptools over distutils
from setuptools import setup
from  setuptools.extension import  Extension
# To use a consistent encoding
from codecs import open
from os import path, makedirs, walk
from shutil import copytree, rmtree, copy2, move
from glob import glob
from pathlib import Path
import platform
import os.path

my_extra_link_args = []
if platform.system() == 'Darwin':
    my_extra_link_args = ['-Wl,-rpath','-Wl,']
    so = 'dylib'
#or dll in glob('yap/dlls/*'):
#    move(  dll ,'lib' )
pls = []
for (r,d,fs) in walk('yap/pl'):
    for f in fs:
        pls += [os.path.join(r, f)]
for (r,d,fs) in walk('yap'):
    for f in fs:
        pls += [os.path.join(r, f)]
#    for f in glob( 'lib*.*' ):
#            ofile.write(f+"\n")
cplus=['../../../CXX/yapi.cpp']
py2yap=['../../../packages/python/python.c',
        '../../../packages/python/pl2py.c',
        '../../../packages/python/pybips.c',
        '../../../packages/python/py2pl.c',
        '../../../packages/python/pl2pl.c',
        '../../../packages/python/pypreds.c'
]
python_sources = ['yapPYTHON_wrap.cxx']+py2yap+cplus
here = path.abspath(path.dirname(__file__))

# Get the long description from the README file

extensions=[Extension('_yap', python_sources,
                      define_macros = [('MAJOR_VERSION', '1'),
                                       ('MINOR_VERSION', '0'),
                                       ('_YAP_NOT_INSTALLED_', '1'),
                                       ('YAP_PYTHON', '1')],
                      runtime_library_dirs=['yap4py','/usr/local/lib','/usr/local/bin'],
    		      swig_opts=['-modern', '-c++', '-py3','-I../../..//CXX'],
                      library_dirs=['../../..','../../../CXX','../../packages/python',"/usr/local/lib/Yap","/usr/local/bin", '.'],
    		      extra_link_args=my_extra_link_args,
                      extra_compile_args=['-g'],
    		      libraries=['Yap','/usr/local/lib/libgmp.dylib'],
                      include_dirs=['../../..',
                                    '/usr/local/include',
                                    '../../../H',
                                    '../../../H/generated',
                                    '../../../OPTYap',
                                    '../../../os',
                                    '../../../include',
				    '../../../CXX', '.']
)]

setup(
    name='YAP4Py',
    version='6.3.5',
    description='The YAP Prolog compiler as  a Python Library',
    url='https://github.com/vscosta/yap-6.3',
    author='Vitor Santos Costa',
    author_email='vsc@dcc.fc.up.pt',
    license='Artistic',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'Topic :: Software Development :: Build Tools',
        'License :: OSI Approved :: Artistic License',
        'Programming Language :: Python :: 3',

        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7'],
    keywords=['Logic Programing'],
    #spackage_data={'': ['yap']},
    include_package_data=True,
    ext_modules = extensions,
    py_modules = ['yap'],
    zip_safe=False,
    eager_resources = ['yap4py'],
    packages=['yap4py'] # find_packages()
#package_dir = {'':'yap4py'}
)
