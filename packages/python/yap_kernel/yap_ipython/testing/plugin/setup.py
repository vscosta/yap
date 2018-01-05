#!/usr/bin/env python
"""A Nose plugin to support yap_ipython doctests.
"""

from setuptools import setup

setup(name='yap_ipython doctest plugin',
      version='0.1',
      author='The yap_ipython Team',
      description = 'Nose plugin to load yap_ipython-extended doctests',
      license = 'LGPL',
      py_modules = ['ipdoctest'],
      entry_points = {
        'nose.plugins.0.10': ['ipdoctest = ipdoctest:IPythonDoctest',
                              'extdoctest = ipdoctest:ExtensionDoctest',
                              ],
        },
      )
