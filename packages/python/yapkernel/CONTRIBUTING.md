# Contributing

Welcome!

For contributing tips, follow the [Jupyter Contributing Guide](https://jupyter.readthedocs.io/en/latest/contributing/content-contributor.html).
Please make sure to follow the [Jupyter Code of Conduct](https://github.com/jupyter/governance/blob/master/conduct/code_of_conduct.md).

## Installing yapkernel for development

yapkernel is a pure Python package, so setting up for development is the same as most other Python projects:

```bash
# clone the repo
git clone https://github.com/ipython/yapkernel
cd yapkernel
# do a 'development' or 'editable' install with pip:
pip install -e .
```

## Releasing yapkernel

Releasing yapkernel is *almost* standard for a Python package:

- set version for release
- make and publish tag
- publish release to PyPI
- set version back to development

The one extra step for yapkernel is that we need to make separate wheels for Python 2 and 3
because the bundled kernelspec has different contents for Python 2 and 3. This
affects only the 4.x branch of yapkernel as the 5+ version is only compatible
Python 3.

The full release process is available below:

```bash
# make sure version is set in yapkernel/_version.py
VERSION="4.9.0"
# commit the version and make a release tag
git add yapkernel/_version.py
git commit -m "release $VERSION"
git tag -am "release $VERSION" $VERSION

# push the changes to the repo
git push
git push --tags

# publish the release to PyPI
# note the extra `python2 setup.py bdist_wheel` for creating
# the wheel for Python 2
pip install --upgrade twine
git clean -xfd
python3 setup.py sdist bdist_wheel
python2 setup.py bdist_wheel  # the extra step for the 4.x branch.
twine upload dist/*

# set the version back to '.dev' in yapkernel/_version.py
# e.g. 4.10.0.dev if we just released 4.9.0
git add yapkernel/_version.py
git commit -m "back to dev"
git push
```
