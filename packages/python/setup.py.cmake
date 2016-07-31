from setuptools import setup, Extension


setup(
    name = "yapex",
    version = "0.1",
package_dir = {'': '${CMAKE_SOURCE_DIR}/packages/python'  },
py_modules = ['yapex']

)
