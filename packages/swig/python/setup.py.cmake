from setuptools import setup, Extension

setup(
    name = "yap",
    version = "0.1",
    ext_modules=[Extension('_yap', [ 'yapPYTHON_wrap.cxx',
                                    '${CMAKE_SOURCE_DIR}/packages/python/pl2py.c',
                                    '${CMAKE_SOURCE_DIR}/packages/python/python.c',
                                     '${CMAKE_SOURCE_DIR}/packages/python/py2pl.c',
                                    '${CMAKE_SOURCE_DIR}/packages/python/pl2pl.c',
                                    '${CMAKE_SOURCE_DIR}/packages/python/pypreds.c',
                                    '${CMAKE_SOURCE_DIR}/packages/python/pybips.c'],
                             define_macros = [('MAJOR_VERSION', '1'),
                                              ('MINOR_VERSION', '0'),
                            ('_YAP_NOT_INSTALLED_', '1')],
                           swig_opts=['-py3', '-c++','-I${CMAKE_SOURCE_DIR}/CXX'],
                             runtime_library_dirs=['/usr/local/lib'],
                             library_dirs=['../../..','../../../CXX'],
                             libraries=['Yap++','Yap'],
                             include_dirs=['../../..',
                                         '${CMAKE_SOURCE_DIR}/H',
                                         '${CMAKE_SOURCE_DIR}/H/generated',
                                         '${CMAKE_SOURCE_DIR}/OPTYap',
                                         '${CMAKE_SOURCE_DIR}/os',
                                         '${CMAKE_SOURCE_DIR}/include',
                                         '${CMAKE_SOURCE_DIR}/CXX', '.']
    )],
py_modules = ['yap']
)
\