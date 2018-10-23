A Jupyter Kernel for YAP		(#yap_kernel)
=======================

This Jupyter kernel supports interaction with YAP Prolog. The kernel
patches IPython and PyKernel so that the user interacts with the
Prolog interpreter. Most of the original kernels are unaffected by the
changes.

You will need `python3` and `jupyter notebook`to use this package, plus:
	- `setuptools`;
	- `wheel`
	- `pip`

The configuration script should recognize whether these Python
packages are installed.

See `tut.ipynb` for details on how to use  the system.

Both `jupyter notebook` and `jupyter lab` rely on the Javascript
editor ` CodeMirror` for editing tasks such as highlighting, inlining,
and folding. Unfortunately, `CodeMirror` does not support the Prolog
language. Starting from Wielemaker's excellent `CodeMirror` module for
`swish`, YAP includes a codemirror for Prolog, designed to fit easily
with any `CodeMirror`-based application.
	
+ `Jupyter lab` includes a complex dependency mechanism, that always
  tries to download from the Internet. We do a first build to ensure
  all packages are there, set the build offline, patch the different
  copies of codemirror, and build locally.
  
  ~~~~
  CM=$HOME/github/CodeMirror
  PYLIB=$HOME/.local/lib/python3.6/site-packages
  PYSHARE=$HOME/.local/share
  cd $PYLIB/jupyterlab
  jupyter lab build
  cp commands.py commands.py.bk
  sed  's/.node., YARN_PATH,/\"node", YARN_PATH, "--offline",/g' commands.py
  cd $PYSHARE/jupyter/lab/staging/node_modules/codemirror/mode
  split -n l/5 meta.js
  cat xaa xab > meta.js
  echo '     {name: "Prolog", mime: "text\/x-prolog", mode: "prolog", ext: ["yap","pl", "ypp", "prolog"]}' >> meta.js
  cat xac xad xae >> meta.js
  cp -a $CM/mode/prolog prolog
  cd $PYLIB/jupyterlab
  jupyter lab build
  mv commands.py.bk commands.py
  ~~~~



