#!/bin/bash

# Script prepares installation file structure and executes build commands

BUILD_DIR="build"
mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}
cmake ${CMAKE_ARGS}  -DCMAKE_INSTALL_PREFIX=../${BUILD_DIR} ..

make -j 6 && cp analyzer ../.

