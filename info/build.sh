#!/bin/bash

export MACOSX_DEPLOYMENT_TARGET=10.9
# export CC=$SYS_PREFIX/bin/clang
# export CXX=$SYS_PREFIX/bin/clang++
export R_COMMAND=$R
export CMAKE_BUILD_TYPE=Debug
export CMAKE=$PREFIX/bin/cmake
export CMAKE_INCLUDE_PATH=$PREFIX/include
export CMAKE_LIBRARY_PATH=$PREFIX/lib
export CMAKE_INSTALL_PREFIX=$PREFIX
export GENERATOR="-GNinja"
export PYTHON_EXECUTABLE="$PYTHON"
export PYTHON_LIBRARY="$CMAKE_LIBRARY_PATH/libpython${PY_VER}m$SHLIB_EXT"
export PYTHON_INCLUDE_DIR="$CMAKE_INCLUDE_PATH/python$PY_VER"m

mkdir $PREFIX/conda
cd  $PREFIX/conda
# The datarootdir option places the docs into a temp folder that won't
  $CMAKE --build=. --target=install\
  -DCMAKE_INCLUDE_PATH="$CMAKE_INCLUDE_PATH" \
  -DCMAKE_LIBRARY_PATH="$CMAKE_LIBRARY_PATH" \
  -DCMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_PREFIX" \
  -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE" \
  -DPYTHON_EXECUTABLE:FILEPATH="$PYTHON_EXECUTABLE" \
  -DPYTHON_LIBRARY:FILEPATH="$PYTHON_LIBRARY" \
  -DPYTHON_INCLUDE_DIR:PATH="$PYTHON_INCLUDE_DIR" \
	$RECIPE_DIR/..

  make -j install CMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_PREFIX"
  #./yap -B
  #
  # Remove the created lib64 directory

rm -rf $PREFIX/conda