#!/bin/bash

export MACOSX_DEPLOYMENT_TARGET=10.9
export CMAKE_BUILD_TYPE=Debug
export CMAKE=$PREFIX/bin/cmake
export MAKE=$PREFIX/bin/make
export GENERATOR="-GNinja"
export PATH=$PREFIX/bin:$SYS_PREFIX/bin:$PATH

mkdir $PREFIX/conda
cd  $PREFIX/conda
# The datarootdir option places the docs into a temp folder that won't
  $CMAKE --build=. --target=install \
  -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_INSTALL_PREFIX="$PREFIX" \
	$RECIPE_DIR/..

  make -j install

  # Remove the created lib64 directory

rm -rf $PREFIX/conda
