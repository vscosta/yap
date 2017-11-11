#!/bin/bash

export MACOSX_DEPLOYMENT_TARGET=10.9
# export CC=$SYS_PREFIX/bin/clang
# export CXX=$SYS_PREFIX/bin/clang++
export CMAKE_BUILD_TYPE=Debug
export CMAKE=$PREFIX/bin/cmake
export MAKE=$PREFIX/bin/make
export GENERATOR="-GNinja"
#export CMAKE_INCLUDE_PATH=$PREFIX/include
#export CMAKE_LIBRARY_PATH=$PREFIX/lib
#export CMAKE_INSTALL_PREFIX=$PREFIX
#export PYTHON_EXECUTABLE="$PYTHON"
#export PYTHON_LIBRARY="$CMAKE_LIBRARY_PATH/libpython${PY_VER}m$SHLIB_EXT"
#export PYTHON_INCLUDE_DIR="$CMAKE_INCLUDE_PATH/python$PY_VER"m
#export R_COMMAND=$R
export PATH=$PREFIX/bin:$SYS_PREFIX/bin:$PATH

mkdir $PREFIX/conda
cd  $PREFIX/conda
# The datarootdir option places the docs into a temp folder that won't
  $CMAKE --build=. --target=install --DPATH=$PATH\
  -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE" \
	$RECIPE_DIR/..

  make -j install CMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_PREFIX"
  #./yap -B
  #
  # Remove the created lib64 directory

rm -rf $PREFIX/conda