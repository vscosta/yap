#!/bin/bash

export MACOSX_DEPLOYMENT_TARGET=10.12

mkdir $PREFIX/conda
cd  $PREFIX/conda
# The datarootdir option places the docs into a temp folder that won't
CC=$SYS_PREFIX/bin/clang \
  CXX=$SYS_PREFIX/bin/clang++ \
  $SYS_PREFIX/bin/cmake \
  -DCMAKE_INCLUDE_PATH="$SYS_PREFIX"/include \
  -DCMAKE_LIBRARY_PATH="$SYS_PREFIX"/lib \
  -DCMAKE_BUILD_TYPE=Debug \
	$RECIPE_DIR/..
CC=$SYS_PREFIX/bin/clang \
  CXX=$SYS_PREFIX/bin/clang++ \
make
CC=$SYS_PREFIX/bin/clang \
  CXX=$SYS_PREFIX/bin/clang++ \
make install

# Remove the created lib64 directory

rm -rf $PREFIX/conda
