#YAP Kernel for Jupyter

This package provides the IPython kernel for Jupyter.

## Installation from source

This should install as part of the YAP system

## Jupyter Lab

CodeMirror does not support highlighting for Prolog. YAP includes a
port based on one that is used in SWISH. To use this mode from
jupyter lab, do as follows:

1. run `jupyter lab build` (you may need root permission). Search the
output for a aline such as:

~~~~
[LabBuildApp] > node /usr/local/lib/python3.7/site-packages/jupyterlab/staging/yarn.js install
~~~~

2, Add the following 3 lines below to the webpack.config.js file:

~~~~~~~
fs.ensureDirSync('node_modules/codemirror/mode/prolog');
fs.copySync(path.join(path.resolve(jlab.buildDir),'../../../kernels/yap_kernel/prolog.js'), 'node_modules/codemirror/mode/prolog/prolog.js');
fs.copySync(path.join(path.resolve(jlab.buildDir),'../../../kernels/yap_kernel/meta.js'), 'node_modules/codemirror/mode/meta.js');
..~~~~~~~~
These lines should copy YAP's prolog.js and a new version of the mode directory, meta.js. whenever you rebuild jlab, eg, if you add a new plugin.

Next, please check the lines in context.

be at around line 24:

~~~~~~~
  output: jlab.outputDir
});

fs.ensureDirSync('node_modules/codemirror/mode/prolog');
fs.copySync(path.join(path.resolve(jlab.buildDir),'../../../kernels/yap_kernel/prolog.js'), 'node_modules/codemirror/mode/prolog/prolog.js');
fs.copySync(path.join(path.resolve(jlab.buildDir),'../../../kernels/yap_kernel/meta.js'), 'node_modules/codemirror/mode/meta.js');

// Create the entry point file.
var source = fs.readFileSync('index.js').toString();
~~~~~~~~

3: Rerun "jupyter lab build"

