from distutils.core import setup, Extension
import sys
import os
import platform

if platform.system() == 'Darwin':
    my_extra_link_args = ['-Wl,-rpath','${dlls}']
else:
    my_extra_link_args = []
                           
setup(
    name = "yap",
    version = "0.1",
    ext_modules=[Extension('_yap', ['${CMAKE_SOURCE_DIR}/packages/swig/yap.i'],    			 
                             define_macros = [('MAJOR_VERSION', '1'),
                                              ('MINOR_VERSION', '0'),
                            ('_YAP_NOT_INSTALLED_', '1')],
                            runtime_library_dirs=['${dlls}'],
			    swig_opts=['-modern', '-c++', '-py3','-I${CMAKE_SOURCE_DIR}/CXX'],
                             library_dirs=['../../..','../../../CXX',
					'../../python',
					'.'],
					extra_link_args=my_extra_link_args,
					libraries=['Yap++','Yap','YAPPython'],
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
