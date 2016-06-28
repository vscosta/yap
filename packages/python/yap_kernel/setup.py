from distutils.command.install import install
from distutils.core import setup
from distutils import log
import json
import sys
import os

PY3 = sys.version_info[0] >= 3

kernel_json = {
    "argv": [sys.executable,
	     "-m", "yap_kernel",
	     "-f", "{connection_file}"],
    "display_name": "MetaKernel YAP %i" % (3 if PY3 else 2),
    "language": "prolog",
    "name": "yap_kernel",
}

class install_with_kernelspec(install):
    def run(self):
        install.run(self)
        from IPython.kernel.kernelspec import install_kernel_spec
        from IPython.utils.tempdir import TemporaryDirectory
        with TemporaryDirectory() as td:
            os.chmod(td, 0o755) # Starts off as 700, not user readable
            with open(os.path.join(td, 'kernel.json'), 'w') as f:
                json.dump(kernel_json, f, sort_keys=True)
            log.info('Installing kernel spec')
            try:
                install_kernel_spec(td, 'yap_kernel', user=self.user,
                                    replace=True)
            except:
                install_kernel_spec(td, 'yap_kernel', user=not self.user,
                                    replace=True)

svem_flag = '--single-version-externally-managed'
if svem_flag in sys.argv:
    # Die, setuptools, die.
    sys.argv.remove(svem_flag)

setup(name='yap_kernel',
      version='0.0.1',
      description='A simple YAP kernel for Jupyter/IPython',
      long_description="A simple YAP kernel for Jupyter/IPython, based on MetaKernel",
      url="https://github.com/vscosta/yap-6.3",
      author='Vitor Santos Costa, based on the metakernel from Douglas Blank',
      author_email='vsc@dcc.fc.up.pt',
      py_modules=['yap_kernel'],
      install_requires=["metakernel","yap"],
      cmdclass={'install': install_with_kernelspec},
      classifiers = [
          'Framework :: IPython',
          'License :: OSI Approved :: BSD License',
          'Programming Language :: Python :: 3',
          'Programming Language :: Python :: 2',
          'Topic :: System :: Shells',
      ]
)
