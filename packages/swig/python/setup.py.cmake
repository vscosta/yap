from distutils.core import setup, Extension

setup(
    name = "yap",
    version = "0.1",
    ext_modules=[Extension('_yap', ['${CMAKE_SOURCE_DIR}/packages/swig/python/_yap.c'],    			 
                             define_macros = [('MAJOR_VERSION', '1'),
                                              ('MINOR_VERSION', '0'),
                            ('_YAP_NOT_INSTALLED_', '1')],
                             runtime_library_dirs=['${dlls}'],
                             library_dirs=['../../..','../../../CXX',
					'../../python',
					'.'],
                             libraries=['Yap++','Yap','YAPPython'],
                             include_dirs=['../../..',
                                         '${CMAKE_SOURCE_DIR}/H',
                                         '${CMAKE_SOURCE_DIR}/H/generated',
                                         '${CMAKE_SOURCE_DIR}/OPTYap',
                                         '${CMAKE_SOURCE_DIR}/os',
                                         '${CMAKE_SOURCE_DIR}/include',
                                         '${CMAKE_SOURCE_DIR}/CXX', '.'],
				extra_objects = ['${CMAKE_CURRENT_BINARY_DIR}/_Py2YAP${CMAKE_SHARED_MODULE_SUFFIX}' ]
    )],
py_modules = ['yap']
)
\
