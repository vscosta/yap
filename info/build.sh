#!/bin/bash

export MACOSX_DEPLOYMENT_TARGET=10.12 
export CMAKE_BUILD_TYPE=Debug
export CMAKE=$PREFIX/bin/cmake
export MAKE=$PREFIX/bin/make
export GENERATOR="-GNinja"
export PATH=$PREFIX/bin:$SYS_PREFIX/bin:$PATH
export PYTHON_INCLUDE_DIRS=$($PYTHON -c "import  sysconfig; print(sysconfig.get_config_var('INCLUDEPY'))")
export PYTHON_LIBRARIES=$($PYTHON -c "import  sysconfig, os.path; print(os.path.join(sysconfig.get_config_var('LIBDIR'),sysconfig.get_config_var('LDLIBRARY')))")

mkdir $PREFIX/conda
cd  $PREFIX/conda
# The datarootdir option places the docs into a temp folder that won't
  $CMAKE --build=. --target=install \
  -DCMAKE_BUILD_TYPE=Debug -GNinja \
  -DCMAKE_INSTALL_PREFIX="$PREFIX" \
  $RECIPE_DIR/.. -DWITH_CUDD=NO -DWITH_GECODE=NO -DWITH_JAVA=NO -DWITH_RAPTOR=NO

  ninja install

  # Remove the created lib64 directory

rm -rf $PREFIX/conda
