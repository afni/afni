#!/bin/sh
CMAKE_PLATFORM_FLAGS+=(-DCMAKE_TOOLCHAIN_FILE="${RECIPE_DIR}/cross-linux.cmake")
mkdir ../build
cd ../build

cmake -DCMAKE_INSTALL_PREFIX=${PREFIX} \
-DCMAKE_BUILD_TYPE=Release \
-DBUILD_SHARED_LIBS=ON \
-DNO_SONAME=ON \
-GNinja \
$SRC_DIR \
${CMAKE_PLATFORM_FLAGS[@]} 

ninja install
